(** General disassembly stuff *)

exception Unimplemented

let disasm_instr arch =
  match arch with
  | Libbfd.Bfd_arch_i386 -> Disasm_i386.disasm_instr
  | _ -> raise Unimplemented

let is_temp = Var_temp.is_temp

let is_decode_error = function
  | Ast.Special("VEX decode error", []) -> true
  | _ -> false
