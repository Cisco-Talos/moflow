(** Visitor for AST programs and expressions. Visitors are a
    systematic method of exploring and modifying programs and
    expressions.

    Users create visitors that describe what actions to take when
    various constructs are encountered.  For instance, the [visit_exp]
    method is called whenever an expression is encountered.  This
    visitor is passed to [accept] functions, which takes an object to
    visit, and calls the visitor's methods are the proper time.
*)

open Type
open Ast

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

  method visit_label : label -> label visit_action
  (** Called when visiting a label. (IE: inside a statment like Cjmp) *)

  method visit_rvar : var -> var visit_action
  (** Called when visiting a referenced variable. (IE: inside an expression) *)

  method visit_avar : var -> var visit_action
  (** Called when visiting an assigned variable. (IE: On the LHS of a Move *)

  method visit_lbinding : var * exp -> (var * exp) visit_action
  (** Called on the binding when recursing into a Let. This allows
      doing stuff between the first and second expressions in a Let. *)

  method visit_ulbinding : var -> var visit_action
  (** Called when visiting a variable being unbound.  For instance,
      variable x after visiting let x = y in z. *)

end

(** A nop visitor that visits all children, but does not change
    anything. This visitor can be inherited from to build a new one. *)
class nop : t

(** {3 Accept functions} *)

(** Visit a label definition *)
val label_accept : #t -> label -> label

(** Visit a referenced variable *)
val rvar_accept : #t -> var -> var

(** Visit an assigned variable *)
val avar_accept : #t -> var -> var

(** Visit a let variable being bound *)
val lbinding_accept : #t -> var * exp -> var * exp

(** Visit a let variable being unbound *)
val ulbinding_accept : #t -> var -> var

(** Visit an expression *)
val exp_accept : #t -> exp -> exp

(** Visit a statement *)
val stmt_accept : #t -> stmt -> stmt

(** Visit an AST program *)
val prog_accept : #t -> program -> program

(** Visit an AST program in CFG form *)
val cfg_accept : #t -> Cfg.AST.G.t -> Cfg.AST.G.t
