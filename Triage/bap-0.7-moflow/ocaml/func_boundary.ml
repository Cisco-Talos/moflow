open Asmir
open Ast
open Type

module D = Debug.Make(struct let name = "Func_boundary" and default=`NoDebug end)
open D


(* This function checks to see if the sequence of assembly insructions
   at start_addr ends at end_addr. *)
let rec liftable_asm_addr_to_bap p start_addr end_addr =
  if start_addr > end_addr then false
  else if start_addr = end_addr then true
  else
    let (_, next) = Asmir.asm_addr_to_bap p start_addr in
    liftable_asm_addr_to_bap p next end_addr

let check_mnemonics ir str =
  List.exists (fun s -> match s with
    | Label (label, attrs) ->
      List.exists (fun attr ->
	match attr with
	| Asm s ->
          (try (String.sub s 0 (String.length str)) = str
           with Invalid_argument _ -> false)
	| _ -> false
      ) attrs
    | _ -> false
  )
  ir

(* p: asmprogram,
   bl: bytelist,
   i: the number of bytes that look backward,
   e_index: the index of byte that look backward,
   last_startaddress: last function's start address, a weak minimum boundary for current function start address. *)

(* This function is to find the real entrance of a function. Given a
   position of push ebp / sub esp, it identifies the start address by
   finding the end of previous function, assuming that function is
   followed by the end instruction of prior function such as ret or nop
   or jmp or call. *)

let rec backward p bl i e_index last_startaddress =
  let end_addr = fst (List.nth bl e_index) in
  let start_addr = fst (List.nth bl (e_index-i)) in
  if start_addr <= last_startaddress then None
  else if i = e_index then Some start_addr
  else
    (* let _ = dprintf "look from %Lx to %Lx, last startaddress %Lx" start_addr end_addr last_startaddress in *)
    try (
      let (ir, n) = Asmir.asm_addr_to_bap p start_addr in
      if (liftable_asm_addr_to_bap p n end_addr) && ((check_mnemonics ir "ret") || (check_mnemonics ir "nop") || (check_mnemonics ir "jmp") || (check_mnemonics ir "call")) then (
	(* Judge if next function is frame_dummy*)
	let (n_ir, nn) = Asmir.asm_addr_to_bap p n in
	let (nn_ir, nnn) = Asmir.asm_addr_to_bap p nn in
	if (check_mnemonics n_ir "lea") && (check_mnemonics nn_ir "lea") then Some nnn
	else Some n 
      )
      else backward p bl (i+1) e_index last_startaddress
    )
    with _ -> backward p bl (i+1) e_index last_startaddress

let start_addresses p =
  if Asmir.get_asmprogram_arch p <> Asmir.arch_i386 then raise (Invalid_argument "Function boundary identification only supported on x86");
  let bytelist = Asmir.get_exec_mem_contents_list p in
  let bytelist = List.map (fun (x,c) -> (x, Char.code c)) bytelist in
  let rec f l index last_startaddress =
    match l with
    (*prolog : 55 89 e5: push %ebp, mov %esp %ebp*)
    | (addr1, 0x55)::(addr2, 0x89)::(addr3, 0xe5)::rest ->
      let _ = dprintf "hit push ebp at address %Lx" addr1 in
      let entrance = backward p bytelist 1 index last_startaddress in
      (match entrance with
        | Some addr -> addr :: f rest (index + 3) addr
        | _ -> f rest (index + 3) last_startaddress)
        
    (* another prolog: 83 ec or 81 ec : sub xxx %esp*)
    | (addr1, 0x81) :: (addr2, 0xec) :: rest
    | (addr1, 0x83) :: (addr2, 0xec) :: rest ->
       let _ = dprintf "hit sub esp at address %Lx" addr1 in
       let entrance = backward p bytelist 1 index last_startaddress in 
       (match entrance with
         | Some addr -> addr :: f rest (index + 2) addr 
         | _ -> f rest (index + 2) last_startaddress)

    (* __libc_csu_fini, started with f3 c3: repz ret *)
    | (addr1, 0xf3)::(addr2, 0xc3)::rest ->
      addr1 :: f rest (index + 2) addr1

    (* __lib_csu_init, started with 55 57 56: push %ebp, push %edi, push %esi *)
    | (addr1, 0x55) :: (addr2, 0x57) :: (addr3, 0x56) :: rest ->
      addr1 :: f rest (index + 3) addr1

    (* _start, started with 31 ed: xor %ebp %ebp, pop %esi *)
    | (addr1, 0x31) :: (addr2, 0xed) :: rest ->
      addr1 :: f rest (index + 2) addr1

    (* __i686.get_pc_thunk.bx, started with 8b 1c 24 c3: mov (%esp) %ebx, ret *)
    | (addr1, 0x8b) :: (addr2, 0x1c) :: (addr3, 0x24) :: (addr4, 0xc3) :: rest ->
      addr1 :: f rest (index + 4) addr1

    (* atexit, stated with 53 e8 ec ff ff ff: push %bx, call __i686.get_pc_thunk.bx *)
    | (addr1, 0x53) :: (addr2, 0xe8) :: (addr3, 0xec) :: (addr4, 0xff) :: (addr5, 0xff) :: (addr6, 0xff) :: rest ->
      addr1 :: f rest (index + 6) addr1

    | first :: rest -> f rest (index + 1) last_startaddress 
    | [] -> [] 
  in
  let start_address_list = f bytelist 0 Int64.zero in
  let _ = List.iter (fun addr -> dprintf "%Lx" addr) start_address_list in
  start_address_list

let get_function_ranges p =
  let open Libbfd in
  let open Libasmir in
  let open Asmir in
  let open Asmir_consts in
  let starts =
    try
      let symb = get_symbols p in
      let is_function = match Asmir.get_flavour p with
        | Bfd_target_elf_flavour
        | Bfd_target_coff_flavour ->
          (fun s -> s.bfd_symbol_flags land bsf_function <> 0)
        | Bfd_target_mach_o_flavour ->
          (fun s -> dprintf "Symbol %s, flags=%#x" s.bfd_symbol_name s.bfd_symbol_flags;
	    s.bfd_symbol_flags land bsf_global <> 0)
        | _ ->
          wprintf "Unknown file format flavour.  Assuming it has a function flag for symbols, which may be incorrect.";
          (fun s -> s.bfd_symbol_flags land bsf_function <> 0)
      and symb_to_tuple s =
        (* FIXME: section_end doesn't seem to get the right values... *)
        (* did this fix it? --aij *)
        let sec = s.bfd_symbol_section in
        let vma = bfd_section_get_vma sec in
        (Int64.add s.bfd_symbol_value vma,
         Int64.add vma (bfd_section_get_size sec),
         s.bfd_symbol_name)
      in
      let starts =
        Array.fold_left
          (fun l s -> if is_function s then symb_to_tuple s :: l else l)
          [] symb
      in starts
    with Failure "get_symbols" ->
      let n = ref 0 in
      (* XXX: Ugly hack: we only use the end address for the last symbol *)
      let end_address = match List.rev (get_exec_mem_contents_list p) with
        | (a, _)::_ -> a
        | _ -> -1L
      in
      List.map (fun a ->
        incr n;
        (a, end_address, "unknown_"^(string_of_int !n))) (start_addresses p)
  in
  let starts = Array.of_list starts in
  (* FIXME: probably should do unsigned comparison *)
  let () = Array.fast_sort compare starts in
  let ranges = Array.mapi
    (fun i (s,e,name) ->
       let e' =
	 try let (s,_,_) = starts.(i+1) in s
	 with Invalid_argument "index out of bounds" -> e
       in
       (name,s,e') (* section_end doesn't work *)
    ) starts
  in
  let unfiltered = Array.to_list ranges in
  (* filter out functions that start at 0 *)
  List.filter (function
		 |(s,0L,_) -> false
		 |("_init",_,_) -> false
		 | _ -> true)
    unfiltered
