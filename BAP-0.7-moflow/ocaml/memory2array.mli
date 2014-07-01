(** Convert [Type.TMem] memories to [Type.Array] memories.

    [Type.Array] memories ensure that all memory reads and writes are
    of the element type, which is almost always [Reg 8] (one byte).
    This module converts [Type.TMem] memories, which do not have
    these restrictions, to [Type.Array] memories.
*)


(** [coerce_prog p] returns a new AST program in which all variables
    of type [Type.TMem] are replaced with variables of type
    [Type.Array].  All memory reads and writes are of type [Reg 8]. *)
val coerce_prog : Ast.program -> Ast.program

(** Like {!coerce_prog} but for expressions *)
val coerce_exp : Ast.exp -> Ast.exp

(** {5 Stateful Variants} *)

(** The stateful variants of the coerce functions are like their
    non-stateful versions, but also read and update an external state
    that maps [Type.TMem] variables to [Type.Array] variables.
    These functions are useful when coercing multiple parts of a
    program so that a [Type.TMem] variable is mapped to the
    same [Type.Array] variable in each part of the
    program.

    An optional scope argument can also be passed to each stateful
    variant so that the parser scope can be kept consistent with
    converted variables. This allows the parser to understand
    references to a converted [Type.Array] variable. *)


(** Map of [Type.TMem] variables to their [Type.Array] type equivalent. *)
type state

(** Create an empty state usable by {!coerce_prog_state},
    {!coerce_exp_state}, or {!coerce_rvar_state}. *)
val create_state : unit -> state

(** [coerce_prog_state state p] is like {!coerce_prog}, but uses and updates the external state [state].
    @param scope Also optionally use and update external scope [scope].
*)
val coerce_prog_state :
  ?scope:(string, Ast.var) Hashtbl.t * 'a ->
  state -> Ast.program -> Ast.program

(** Like {!coerce_prog_state}, but for expressions. *)
val coerce_exp_state :
  ?scope:(string, Ast.var) Hashtbl.t * 'a ->
  state -> Ast.exp -> Ast.exp

(** Like {!coerce_prog_state}, but for referenced variables. *)
val coerce_rvar_state :
  ?scope:(string, Ast.var) Hashtbl.t * 'a ->
  state -> Ast.var -> Ast.var

(** {5 Deprecated functions} *)

val split_loads : Ast.exp -> Ast.exp -> Type.typ -> Ast.exp -> Ast.exp
val split_writes :
  Ast.exp -> Ast.exp -> Type.typ -> Ast.exp -> Ast.exp -> Ast.exp

