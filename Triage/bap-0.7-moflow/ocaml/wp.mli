(** Functions for computing the weakest preconditions (WPs) of
    programs.

    Given a program [p] and a postcondition [q], the weakest
    precondition [wp(p,q)] describes all input states that will cause
    [p] to terminate in a state satisfying [q]. Generally, the
    precondition is solved with an SMT solver.
*)

(** [wp p q] uses Dijkstra's classic WP algorithm to compute
    [wp(p,q)], applying simp to simplify each intermediate expression
    during the calculation.  See "A Discipline of Programming" by
    Dijkstra, or CMU-CS-08-159 (Brumley's thesis), chapter 3.2.  This
    algorithm may produce formulas exponential in the size of the
    program.

    @param simp is an optional expression simplifier.

    @param p is the program

    @param q is the post-condition.
*)
val wp : ?simp:(Ast.exp -> Ast.exp) -> Gcl.t -> Ast.exp -> Ast.exp

(** [passified_wp] is similar to {!wp}, but is intended for passified
    programs.  Unlike {!wp} it does not duplicate the post-condition. *)
val passified_wp : ?simp:(Ast.exp -> Ast.exp) -> Gcl.t -> Ast.exp -> Ast.exp

val build_uwp : (Gcl.t -> Ast.exp -> Ast.exp) -> Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [build_uwp wp] uses the GCL-based weakest precondition algorithm
    for non-passified programs [wp] and builds an unstructured weakest
    precondition algorithm from it. See "Weakest-Precondition of
    Unstructured Programs" by Barnett for the general technique. *)

val dijkstra_uwp : Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [dijkstra_wp] is [build_uwp Wp.wp] *)

val build_passified_uwp : ((Cfg.AST.G.V.t -> unit) -> Cfg.AST.G.t -> unit) -> (Gcl.t -> Ast.exp -> Ast.exp) -> Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [build_passified_uwp iter wp] uses the GCL-based weakest
    precondition algorithm [wp] for passified programs and builds an
    unstructured weakest precondition algorithm from it. [iter]
    specifies the iteration strategy, e.g., reverse topological
    order. See "Weakest-Precondition of Unstructured Programs" by
    Barnett for the general technique. *)

val efficient_uwp : Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [efficient_uwp] is [build_passified_uwp RToposort.iter Wp.wp]. Note that the
    [Choice] rule is not used, which is the only inefficient aspect of
    [Wp.wp]. *)

val efficient_wp : ?simp:(Ast.exp -> Ast.exp) -> Gcl.t -> Ast.exp -> Ast.exp
(** [efficient_wp p q] computes [wp(p,q)] using an algorithm that
    guarantees the resulting precondition will be linear in the size
    of p.  [efficient_wp] expects p to be assignment-free, e.g., to be
    an SSA acyclic program. See CMU-CS-08-159.pdf (Brumley's thesis),
    chapter 3.3.

    @param simp is an expression simplifier. You can pass in fun x->x
    if you want no simplification.

    @param p is the program

    @param q is the post-condition.
*)

val flanagansaxe :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?less_duplication:bool -> ?k:int -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [flanagansaxe mode p q] computes [wp(p,q)] using Flanagan and
    Saxe's algorithm. The [mode] argument specifies whether the formula
    will be solved for satisfiability or validity. *)

(** {5 Directionless Weakest Precondition Algorithms} *)

val dwp_1st :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?less_duplication:bool ->
  ?k:int -> Gcl.t -> Ast.exp -> Ast.var list * Ast.exp
(** [dwp_1st p q] returns a tuple [(vars, pc)] where [pc] is [wp(p,q)]
    and [vars] is a list of variables that should be quantified using
    foralls. *)

val dwp :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?less_duplication:bool -> ?k:int -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [dwp mode p q] is the same as {!dwp_1st}, except it generates a
    precondition that does not need quantifiers.  However, the [mode]
    argument must be used to specify whether the formula will be used
    for satisfiability or validity. *)

val dwp_let :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?less_duplication:bool -> ?k:int -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [dwp_let] is just like {!dwp}, except that [dwp_let] wraps helper
    variables in [Let] expressions so they do not appear as free
    variables. *)

(** {5 Utility Functions for Building WP Algorithms} *)

val variableify :
  ?name:string -> int -> (Ast.var * Ast.exp) list -> Ast.exp -> (Ast.var * Ast.exp) list * Ast.exp
(** When given a list of variable assignments [v], [variableify s v e]
    returns a tuple [(v',e')] where [v'] is an updated list of
    variable assignments, and [e'] is guaranteed to be at most [s] in
    size. Use this function to avoid duplicating large expressions in
    formulas. *)

val assignments_to_exp : (Ast.var * Ast.exp) list -> Ast.exp
(** Convert a list of variable assignments, such as those returned by
    {!variableify}, to a single expression. *)

val assignments_to_lets : (Ast.var * Ast.exp) list -> Ast.exp -> Ast.exp
(** [assignments_to_lets vars e] converts a list of variable
    assignments, such as those returned by {!variableify}, to let bindings
    surrounding expression [e]. *)
