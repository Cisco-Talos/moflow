(** Recognizing and creating temporary variables

    A temporary is a variable introduced by BAP's lifting process that
    is only referenced inside one assembly block.  The evaluator (and
    other BAP analyses) use this information to throw away any state
    stored for these temporaries once the temporary becomes out of
    scope (i.e., out of that assembly block).
*)

(** [is_temp_name s] returns true iff s denotes a temporary variable
    name.
*)
val is_temp_name : string -> bool

(** [is_temp v] is equivalent to [is_temp_name (Var.name v)]. *)
val is_temp : Var.t -> bool

(** [nt n t] creates a new temporary variable with name [n] and type
    [t]. *)
val nt : string -> Type.typ -> Var.t
