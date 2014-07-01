(** Visitor for SSA programs and expressions. Visitors are a
    systematic method of exploring and modifying programs and
    expressions.

    Users create visitors that describe what actions to take when
    various constructs are encountered.  For instance, the [visit_exp]
    method is called whenever an expression is encountered.  This
    visitor is passed to [accept] functions, which takes an object to
    visit, and calls the visitor's methods are the proper time.
*)

open Type
open Ssa

(** The type for a visitor *)
class type t =
object
  method visit_exp : exp -> exp visit_action
    (** Called when visiting an expression *)

  method visit_stmt : stmt -> stmt visit_action
    (** Called when visiting a statement.

        Note that in a Move() or Let(), referenced variables will be
        visited first, so that this can be used to add the assigned
        variable to your context.

	FIXME: would be nice to be able to add stmts... We may change this. *)

  method visit_value : value -> value visit_action
  (** Called when visiting a SSA value *)

  method visit_rvar : var -> var visit_action
  (** Called when visiting a refenenced variable. (IE: inside an expression) *)

  method visit_avar : var -> var visit_action
(** Called when visiting an assigned variable. (IE: On the LHS of a Move *)
end

(** A nop visitor that visits all children, but does not change
    anything. This visitor can be inherited from to build a new one. *)
class nop : t

(** {3 Accept functions} *)

(** Visit a referenced variable *)
val rvar_accept : #t -> var -> var

(** Visit an assigned variable *)
val avar_accept : #t -> var -> var

(** Visit a value *)
val value_accept : #t -> value -> value

(** Visit an expression *)
val exp_accept : #t -> exp -> exp

(** Visit a statement *)
val stmt_accept : #t -> stmt -> stmt

(** Visit a list of statements *)
val stmts_accept : #t -> stmt list -> stmt list

(** Visit a SSA program/CFG *)
val prog_accept : #t -> Cfg.SSA.G.t -> Cfg.SSA.G.t

(** Alias of {!prog_accept}. *)
val cfg_accept : #t -> Cfg.SSA.G.t -> Cfg.SSA.G.t
