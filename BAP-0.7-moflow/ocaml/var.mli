(** The type of variables.

    @author Ivan Jager
*)

type t = private V of int * string * Type.typ
(** The type for a variable identifier.  The int should uniquely
    identify the variable. The string is to make it easier for humans
    to read.  A variable's type is embedded in it.
*)

(** [newvar s t] creates a fresh variable of type [t] and human
    readable string [s]. *)
val newvar : string -> Type.typ -> t

(** [renewvar v] creates a fresh variable with the same name and type
    as [v].  [renewvar v] is equivalent to [newvar (Var.name v)
    (Var.typ v)]. *)
val renewvar : t -> t

(** [typ v] returns the type of [v]. *)
val typ : t -> Type.typ

(** [name v] returns the name of [v]. *)
val name : t -> string

(** {3 Comparison functions} *)

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int

(** {3 Data structures for storing variables} *)

module VarHash : Hashtbl.S with type key = t
module VarMap : BatMap.S with type key = t
module VarSet : BatSet.S with type elt = t
