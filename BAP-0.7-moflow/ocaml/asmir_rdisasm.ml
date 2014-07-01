(* Recursive disassembly module *)

open Ast
open BatListFull
module D = Debug.Make(struct let name = "Asmir_rdisasm" and default=`NoDebug end)
open D
open Type

let get_addr expr =
  match expr with
  | Ast.Int (i, _) -> Some (Big_int_Z.int64_of_big_int i)
  | _ -> None

let collect_some f l x =
    match f x with
    | Some i -> i :: l
    | None -> l

let addrt = Reg 32

(*
 * Given a BAP IR for an instruction and the address of the next instruction,
 * returns a list of addresses that we should disassemble.
 *)
let get_code_addrs stmts next =
  let rec
    get_code_addrs' stmts l =
      match stmts with
      (* We must explore the following instructions next.

         To see why, consider what happens with a conditional jump:

         addr 0xd2928 @asm "je     0x00000000000d29b4"
         label pc_0xd2928
         cjmp R_ZF:bool, 0xd29b4:u32, "nocjmp0"
         label nocjmp0

         If the condition is false, control transfers to the next
         instruction.  This is expected to be the next instruction in
         memory. If we recursed to the target first, then we would
         build the IL with the new target there, and thus implicitly
         transfer control to that address even when the condition is
         false!

         As a result of this, next should be at the front of the list,
         and we can use a Stack. *)
      | [] -> next :: l
      | (Ast.Jmp (e, attrs)) :: _ ->
        let addrs = collect_some get_addr l e in
          (* Assume that control returns to the next instruction a call. *)
          if List.mem (Type.StrAttr "call") attrs
          then next :: addrs
          else addrs
      | (Ast.CJmp (_, e1, e2, _)) :: rest ->
          let addrs = List.fold_left (collect_some get_addr) [] [e1; e2]
          in
            (*
             * Only continue looking for addresses operands wasn't an address.
             * This usually means that the operand was a label at the next
             * statement.
             *)
            if List.length addrs == 2
            then addrs
            else get_code_addrs' rest (addrs @ l)
      | _ :: rest -> get_code_addrs' rest l
  in
  (* Reverse so that 'next' is last in the list, last added to the
     stack, and thus first in the stack. *)
  List.rev (get_code_addrs' stmts [])

module Int64Set = Set.Make( 
  struct
    type t = Int64.t
    let compare = Int64.compare
  end)

type callback = addr -> addr -> stmt list -> bool
let default _ _ _ = true

let rdisasm_at ?(f=default) p startaddrs =
  let seen = ref Int64Set.empty in
  let out = ref [] in
  let outasm = ref "" in
  let stack = Stack.create () in
  let numstmts = ref 0 in

  (* Remove duplicates or we'll get duplicate labels *)
  let startaddrs = Util.list_unique startaddrs in

  (* Initialize with the startaddrs *)
  List.iter (fun startaddr ->
    Stack.push startaddr stack;
  ) startaddrs;

  while not (Stack.is_empty stack) do
    let addr = Stack.pop stack in
    try
      if Int64Set.mem addr !seen then (
        (* If we have already seen this address before, add a jump to
           it to preserve control flow. If the instruction before us
           is any type of jump, this is not necessary.*)
        let add_jump = match !out with
          | b::_ ->
            (match List.rev b with
            | Jmp _::_ | CJmp _::_ -> false
            | _ -> true)
          | _ -> true
        in
        if add_jump then
          out := [Jmp(Int(Big_int_Z.big_int_of_int64 addr, addrt), [StrAttr "rdisasm"])] :: !out)
      else (
        let (statements, next) = Asmir.asm_addr_to_bap p addr in
        let asm = Asmir.get_asm_instr_string p addr in
        seen := Int64Set.add addr !seen;
        out := statements :: !out;
        outasm := !outasm ^ "; " ^ asm;
        numstmts := !numstmts + (List.length statements);
        if not (f addr next statements) then raise Asmir.Disassembly_error;
        List.iter
          (fun x ->
            Stack.push x stack;
            dprintf "Adding address %#Lx to the stack" x;
          )
          (get_code_addrs statements next))
    (*
     * Ignore invalid addresses.
     * (some programs have a call 0 for some reason)
     *)
    with Asmir.Memory_error -> ()
  done;
  List.concat (List.rev !out), !outasm

let rdisasm ?(f=default) p =
  let func_starts = List.map (function (_, s, _) -> s) (Func_boundary.get_function_ranges p) in
  let startaddrs = Asmir.get_start_addr p :: func_starts in
  rdisasm_at ~f p startaddrs

let max_callback n =
  let ctr = ref 0 in
  (fun _ _ _ ->
    incr ctr;
    if !ctr > n then false else true)
