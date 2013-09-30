(** Break complicated memory write statements a series of flat ones of
    form [Store(Var v, ...)]. This makes it easier to execute the
    memory operations sequentially.

    @author Ed Schwartz
*)

(* val explode_mem_let : (Ast.exp -> bool) -> Ast.exp -> Ast.exp *)
(* val flatten_memexp_rev : *)
(*   Ast.var -> Ast.attrs -> Ast.exp -> Ast.exp * Ast.stmt list *)

(** [flatten_memexp memvl atts e] returns a tuple [(flate, stmts)]
    where [flate] contains no nested Stores, and is equivalent to [e],
    provided [stmts] are executed immediately before evaluating
    [flate]. *)
val flatten_memexp :
  Ast.var -> Ast.attrs -> Ast.exp -> Ast.exp * Ast.stmt list

(** Converts a nested memory assignment to multiple flat (non-nested)
    assignments. Non-memory write statements are not changed.  [Let]
    expressions that evaluate to a memory are removed using
    substitution, although this may change.

    For example, [Move(memv, Store(Store(memv, idx1, value1), idx2,
    value2))] would be converted to [Move(memv, Store(Var memv, idx1,
    value1)) :: Move(memv, Store(Var memv, idx2, value2)) :: []].
*)
val flatten_stores : Ast.stmt -> Ast.stmt list

(** Converts a memory load expression [e] into a form that can be
    sequentially evaluated concretely.  In particular, this means that
    there are no [Let] bindings that bind a memory state to a
    variable, because that cannot be implemented concretely. *)
val flatten_loads : Ast.exp -> Ast.exp

(** Flattens all memory loads and stores in a program. *)
val flatten_mem_program : Ast.program -> Ast.program
