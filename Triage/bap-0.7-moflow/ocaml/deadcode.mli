(** Dead code elimination for SSA graphs. *)

val do_dce : ?globals:Var.t list -> Cfg.SSA.G.t -> Cfg.SSA.G.t * bool
(** Performs dead code elimination, returning the new CFG and a bool
    indicating whether anything changed. A statement [s] is considered live if:
    - [s] has the [Liveout] attribute
    - OR [s] appears in [globals]
    - OR [s] defines [v], and another statement (live or non-live) refers to [v]
    - OR [s] is not a move statement.

    All non-live statements are considered dead, and will be removed.

    @param globals a list of additional variables to be considered
    live out. The safe default is to declare all variables live-out.
*)

val do_aggressive_dce : ?globals:Var.t list -> Cfg.SSA.G.t -> Cfg.SSA.G.t * bool
(** Just like {!do_dce}, except dead code is detected more
    aggressively. Specifically, a statement [s] is considered live if
    - [s] has the [Liveout] attribute
    - OR [s] appears in [globals]
    - OR [s] defines [v], and a {b live} statement refers to [v]
    - OR [s] is a conditional branch that a live statement is control-dependent upon
    - OR [s] is an assertion statement
    - OR [s] is a halt statement
    - OR [s] is a comment statement
    - OR [s] is a special statement

    Although non-move statements can be dead, only dead move and cjmp
    statements are currently removed.

    [do_aggressive_dce] is considered experimental.
*)
