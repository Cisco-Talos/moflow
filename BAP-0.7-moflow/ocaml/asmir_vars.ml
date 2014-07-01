(** Asmir variables *)

open Type

let x86_regs = Disasm_i386.regs
let x86_mem = Disasm_i386.mem

let arm_regs =
  List.map (fun n -> Var.newvar n (Reg 32))
    [ "R0";
      "R1";
      "R2";
      "R3";
      "R4";
      "R5";
      "R6";
      "R7";
      "R8";
      "R9";
      "R10";
      "R11";
      "R12";
      "R13";
      "R14";
      "R15T";
      "CC";
      "CC_OP";
      "CC_DEP1";
      "CC_DEP2";
      "CC_NDEP";
    ]

let all_regs = x86_mem :: x86_regs @ arm_regs

