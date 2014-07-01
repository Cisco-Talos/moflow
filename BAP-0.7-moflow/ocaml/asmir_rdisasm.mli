(** Lift a program using recursive disassembly.

    @author Ricky Zhou
*)

open Ast
open Type

type callback = addr -> addr -> stmt list -> bool

(** Recursively disassemble [p] beginning from known addresses in
    [startaddrs]. If [f] is defined and [f addr stmts] returns false,
    raises {!Asmir.Disassembly_error}. *)
val rdisasm_at : ?f:callback -> Asmir.asmprogram -> addr list -> Ast.program * string

(** Recursively disassemble [p] beginning at the program's defined
    start address and any function symbols.  [f] behaves the same as in
    {!rdisasm_at}. *)
val rdisasm : ?f:callback -> Asmir.asmprogram -> Ast.program * string

(** [max_callback n] produces a callback function that stops
    disassembling after discovering [n] instructions.  The resulting
    callback function can be passed to the [f] arguments in
    [rdisasm_at] or [rdisasm]. *)
val max_callback : int -> callback
