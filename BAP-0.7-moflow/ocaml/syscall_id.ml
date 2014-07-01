(** Statically identify obvious use of system call numbers.
*)

open Ast
open Big_int
open Big_int_convenience
open Cfg
module D = Debug.Make(struct let name = "Syscall_id" and default=`NoDebug end)
open D

let syscall_reg = Disasm_i386.eax;;

module S = Big_int_convenience.BIS

module DFSPEC = struct
  module G = Cfg.AST.G
  module L = struct
    type t = Top | Set of S.t
    let top = Top
    let equal x y = match x,y with
      | Top, Top -> true
      | Set s1, Set s2 -> S.equal s1 s2
      | _ -> false
    let meet x y = match x,y with
      | Top, x -> x
      | x, Top -> x
      | Set s1, Set s2 -> Set (S.union s1 s2)
  end

  let transfer_function g n init =
    let stmts = Cfg.AST.get_stmts g n in
    let process_stmt a s = match s with
      | Move(v, Int(i, t), _) when v=syscall_reg ->
        L.Set(S.singleton i)
      | Move(v, _, _) when v=syscall_reg ->
        L.Set(S.empty)
      | _ -> a
    in
    (* We don't want to propagate Syscall values across blocks *)
    (* let init = match init with *)
    (*   | t, m -> t, (AddrMap.map *)
    (*                   (fun v -> match v with *)
    (*                   | L.Syscall _ -> L.Bottom *)
    (*                   | x -> x) *)
    (*                   m) *)
    (* in *)
    List.fold_left process_stmt init stmts
  let s0 g =
    (* let check_v v a = *)
    (*   let stmts = Cfg.AST.get_stmts g v in *)
    (*   if List.exists is_syscall stmts then v::a *)
    (*   else a *)
    (* in *)
    (* Cfg.AST.G.fold_vertex check_v g [snd(Cfg_ast.find_error g)] *)
    snd(Cfg_ast.find_entry g)
  let init _ = L.Set(S.empty)
  let dir = GraphDataflow.Forward

end

module DF = GraphDataflow.Make(DFSPEC)

let find_syscalls g =
  let inh, out = DF.worklist_iterate g in
  let process_vertex n a =
    match out n with
    (* | DFSPEC.L.Term, m -> *)
    (*   AddrMap.fold (fun k v acc -> match k, v with *)
    (*   | addr, DFSPEC.L.Syscall(i, t) -> *)
    (*     dprintf "Found syscall %s! yay" (~% i); *)
    (*     if debug then ( *)
    (*       let stmts = Cfg.AST.get_stmts g n in *)
    (*       Debug_snippets.print_ast stmts; *)
    (*     ); *)
    (*     (n, int_of_big_int i)::acc *)
    (*   | _ -> acc) m a *)
    (* | _ -> a *)
    | DFSPEC.L.Set s ->
      let stmts = Cfg.AST.get_stmts g n in
      (* if debug then Debug_snippets.print_ast stmts; *)
      if List.exists is_syscall stmts then
        (* let () = dprintf "SYSCALL!!!!! O YEA %s" (S.fold (fun e a -> a^" "^(string_of_big_int e)) s "") in *)
        (n, S.elements s)::a
      else a
    | _ -> 
      a
  in
  Cfg.AST.G.fold_vertex process_vertex g []
