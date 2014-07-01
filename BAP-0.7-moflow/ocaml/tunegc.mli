(** Automatically tune Garbage collection parameters.

    Decrease the aggressiveness of OCaml's garbage collector, since BAP
    often uses very large objects.
*)

val set_gc : unit -> unit
(** [set_gc ()] tunes the garbage collection parameters so they are
    more appropriate for BAP. *)
