(** Use this to read in a program.

    TODO: Add convenience functions to get SSA directly, and maybe more input
    options.
*)

open Arg
open Grammar_scope

(** A speclist suitable to pass to Arg.parse.
    Add this to the speclist for your program. *)
val speclist : (key * spec * doc) list

(** A speclist with only streaming inputs *)
val stream_speclist : (key * spec * doc) list

(** A speclist with only trace inputs *)
val trace_speclist : (key * spec * doc) list

(** Get the program as specified by the commandline. *)
val get_program : unit -> Ast.program * Scope.t

val get_stream_program : unit -> (Ast.program) Stream.t

val init_ro : bool ref

(* Rate to stream frames at *)
val streamrate : int64 ref
