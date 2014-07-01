(** Forward weakest preconditions.

    @author ejs
*)

val fwp :
  ?normalusage:bool ->
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [fwp mode p q] is the same as {!dwp}, but uses an alternate
    formulation of dwp.  It is arguably easier to understand, and
    generates smaller formulas for programs that do not have [Assume]
    statements. *)

val fwp_uwp :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> Type.formula_mode -> Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [fwp_uwp] is a version of [fwp] that operates directly on a
    CFG. *)

val fwp_lazyconc :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> ?cf:bool -> Type.formula_mode -> Gcl.t -> Ast.exp -> Ast.exp
(** [fwp_lazyconc] is like {!fwp} but with concrete evaluation and
    lazy merging turned on. *)

val fwp_lazyconc_uwp :
  ?simp:(Ast.exp -> Ast.exp) ->
  ?k:int -> ?cf:bool -> Type.formula_mode -> Gcl.Ugcl.t -> Ast.exp -> Ast.exp
(** [fwp_lazyconc_uwp] is a version of [fwp_lazyconc] that
    operates directly on a CFG. *)
