(** Functions for streaming processing of traces.

    @author Spencer Whitman
    @author Edward J. Schwartz
 *)

(** [concrete ret] returns a streaming function for concretely
    executing a trace.  If [ret] is true, the transformed trace is return.
    If false, the empty program is returned. *)
val concrete : bool -> (Ast.program -> Ast.program)

(** [generate_formula f solver] returns two functions for
    symbolically executing a program and outputting the formula in [f]
    using [solver].  The first function is a streaming function that
    does the symbolic execution.  The second function finishes
    outputting the formula to file. *)
val generate_formula : string -> Smtexec.smtexec -> (Ast.program -> unit) * (unit -> unit)
