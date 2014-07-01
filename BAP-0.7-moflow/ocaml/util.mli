(** Utility functions that are not BAP specific.

    @author Ivan Jager
*)

(** The identity function *)
val id : 'a -> 'a

(** Curry a tupled function *)
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

(** The opposite of [curry] *)
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)

(** [(f <@ g) x] = [f (g x)] *)
val ( <@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(** Return the head and tail of a list *)
val hd_tl : 'a list -> ('a * 'a list)

(** [foldn f i n] is f (... (f (f i n) (n-1)) ...) 0 *)
val foldn : ?t:int -> ('a -> int -> 'a) -> 'a -> int -> 'a

(** Same as [foldn] but for [int64] integers. *)
val foldn64 : ?t:int64 -> ('a -> int64 -> 'a) -> 'a -> int64 -> 'a

(** [mapn f n] is the same as [f 0; f 1; ...; f n] *)
val mapn : (int -> 'a) -> int -> 'a list

(** {3 List utility functions} *)

(** [list_mem] behaves like {!List.mem}, but element equality can be
    specified with [eq], which defaults to [(=)]. *)
val list_mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a list -> bool

(** @return the arg max of [f] where the arguments come from [l] *)
val list_argmax : ?compare:('b -> 'b -> int) -> ('a -> 'b) -> 'a list -> 'a * 'b

(** @return a union b, assuming a and b are sets *)
val list_union : 'a list -> 'a list -> 'a list

(** @return a intersect b, assuming a and b are sets *)
val list_intersection : 'a list -> 'a list -> 'a list

(** @return true if the intersection of two lists is not empty *)
val list_does_intersect : 'a list -> 'a list -> bool

(** @return a - b, assuming a and b are sets *)
val list_difference : 'a list -> 'a list -> 'a list

(** {list_subset a b} returns true when all elements in a are also in b *)
val list_subset : 'a list -> 'a list -> bool

(** @return true when both sets contain the same elements *)
val list_set_eq : 'a list -> 'a list -> bool

(** [list_unique l] returns a list of elements that occur in [l], without
    duplicates. (uses [=] and [Hashtbl.hash])  *)
val list_unique : 'a list -> 'a list

(** Pop the first element off a list ref. *)
val list_pop : 'a list ref -> 'a

(** Push an element onto the front of a list ref. *)
val list_push : 'a list ref -> 'a -> unit

(** @return the last element of a list *)
val list_last : 'a list -> 'a

(** If [l] is non-empty, [list_last_option l] returns [Some (list_last l)].  Otherwise, it returns [None]. *)
val list_last_option : 'a list -> 'a option


(* Deprecated: BatList.find_map *)
(* [list_find_some x] is equivalent to [BatOption.get (List.find
    (function | Some _ -> true | None -> false) x)]. *)
(* val list_find_some : ('a -> 'b option) -> 'a list -> 'b *)


(* Deprecated: Batlist.filter_map *)
(* val list_map_some : ('a -> 'b option) -> 'a list -> 'b list *)

(* list_join is BatList.reduce.

    Deprecated.
*)
(* val list_join : ('a -> 'a -> 'a) -> 'a list -> 'a *)

(* Deprecated, use BatList.findi *)
(* val list_firstindex : ?s:int -> 'a list -> ('a -> bool) -> int *)

(** [list_insert l li i] Insert all elements in [li] into [l] just before index [i] *)
val list_insert : 'a list -> 'a list -> int -> 'a list

(** [list_remove l i n] removes [n] elements in [l] starting at position [i]. *)
val list_remove: 'a list -> int -> int -> 'a list

(** Deletes the first occurrence of e (if it exists) in the list
    and returns the updated list *)
val list_delete: 'a list -> 'a -> 'a list

(** Lexicographic compare of lists *)
val list_compare: ('a -> 'a -> int) -> ('a list) -> ('a list) -> int

(** Calls f with each element in the cartesian product of the input lists *)
val list_cart_prod2: ('a -> 'b -> unit) -> ('a list) -> ('b list) -> unit

(** Calls f with each element in the cartesian product of the input lists *)
val list_cart_prod3: ('a -> 'b -> 'c -> unit) -> ('a list) -> ('b list) -> ('c list) -> unit

(** Calls f with each element in the cartesian product of the input lists *)
val list_cart_prod4: ('a -> 'b -> 'c -> 'd -> unit) -> ('a list) -> ('b list) -> ('c list) -> ('d list) -> unit

(** {list_permutation setlist f} calls f with every value in the
    cartesian product of the sets in setlist. For instance, if setlist is
    [[1;2]; [2;3]], then this function will call f [1;2], f [1;3], f
    [2;2], and f [2;3] in some order.
*)
val list_permutation: 'a list list -> ('a list -> unit) -> unit
(** [list_find_option p l] returns [Some x] for the first element [x] such that [p x] is true.  If no such elements [x] exists, returns [None]. *)
val list_find_option: ('a -> 'b option) -> ('a list) -> 'b option

(** [list_partition_last l] returns [(lst,last)] where [lst] is
    [List.tl l] and [last] is the last element in [l].

    @raise Invalid_argument if [lst] is empty
*)
val list_partition_last : 'a list -> 'a list * 'a

(** [list_shortest_first f l1 l2] Calls [f] with its shorter list argument
    first, and the longer one second. *)
val list_shortest_first : (int -> int -> 'a) -> 'b list -> 'c list -> 'a

(* Deprecated: stupid *)
(* (\** Calls f on each element of l and if there is Some() value returned *)
(*     for each list member, returns Some(unwrapped list). If at least *)
(*     one returns None, None is returned instead. *\) *)
(* val list_for_all_option: ('a -> 'b option) -> ('a list) -> 'b list option *)

(** {3 File/IO functions} *)

(** Change the extension of a filename *)
val change_ext : string -> string -> string

(** Get list of file name in directory (ordered by name) *)
val list_directory : ?sort_files:bool -> string -> string list

(** Remove trailing newline *)
val trim_newline : string -> string

(** {3 Simple algorithms} *)

(** [union_find map items], where [map] is a mapping from items to
    their dependencies, finds independent elements in [items] *)
val union_find : ('a -> 'b list) -> 'a list -> 'a list list

(** [split_common_prefix ?eq l1 l2] returns [(lcom, l1', l2')] where
    [l1'] and [l2'] are the longest lists such that [lcom::l1' eq l1] and
    [lcom::l2' eq l2]. *)
val split_common_prefix : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list * 'a list * 'a list

(** [split_common_suffix ?eq l1 l2] returns [(lcom, l1', l2')] where
    [l1'] and [l2'] are the longest lists such that [l1'::lcom eq l1]
    and [l2'::lcom eq l2]. *)
val split_common_suffix : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list * 'a list * 'a list

(* Deprecated: BatOption.get *)
(* val option_unwrap : ('a option) -> 'a *)

(* Deprecated: BatOption.map *)
(* val option_map : ('a -> 'b) -> 'a option -> 'b option *)

(** [apply_option fopt] is equal to [f] when [fopt = Some f], and is
    equal to the identity function when [fopt = None]. *)
val apply_option : ('a -> 'a) option -> 'a -> 'a

(** Memoize the results of a function *)
val memoize : ?size:int -> ('a -> 'b) -> 'a -> 'b

(* Deprecated: BatOption.is_some *)
(* val has_some: 'a option -> bool *)

(** {3 Hash table functions} *)

(** Get the keys from a hash table.  If a key has multiple bindings,
    it is included once per binding *)
val get_hash_keys : ?sort_keys:bool -> ('a, 'b) Hashtbl.t -> 'a list

(** Get the values from a hash table.  If a key has multiple bindings,
    the value is included once per binding *)
val get_hash_values : ?sort_values:bool -> ('a, 'b) Hashtbl.t -> 'b list

(** Extension of [Hashtbl]s *)
module HashUtil :
  functor (H : Hashtbl.S) ->
sig
  (** Test if two [Hashtbl]s are equal. Keys are tested for equality
      with [eq], which defaults to [(=)]. *)
  val hashtbl_eq : ?eq:('a -> 'a -> bool) -> 'a H.t -> 'a H.t -> bool

  (** Implementation of {!Hashtbl.replace} to work around OCaml
      bug. *)
  val hashtbl_replace : 'a H.t -> H.key -> 'a -> unit

  (** Get the keys from a hash table.  If a key has multiple bindings,
      it is included once per binding *)
  val get_hash_keys : ?sort_keys:bool -> 'a H.t -> H.key list

  (** Get the values from a hash table.  If a key has multiple bindings,
      the value is included once per binding *)
  val get_hash_values : ?sort_values:bool -> 'a H.t -> 'a list

end

(** Version of hashtbl_eq for polymorphic [Hashtbl]s *)
val hashtbl_eq :
  ?eq:('a -> 'a -> bool) -> ('b, 'a) Hashtbl.t -> ('b, 'a) Hashtbl.t -> bool

(** {3 Arithmetic [int64] operations} *)

(** Unsigned compare *)
val int64_ucompare : int64 -> int64 -> int

(** Unsigned division *)
val int64_udiv : int64 -> int64 -> int64

(** Unsigned remainder *)
val int64_urem : int64 -> int64 -> int64

(** Unsigned max *)
val int64_umax : int64 -> int64 -> int64

(** Unsigned min *)
val int64_umin : int64 -> int64 -> int64

(** Convert [int64] to binary string for printing *)
val int64_to_binary : ?pad:int -> int64 -> string

(** {3 Arithmetic [big_int] operations} *)

(** Convert [big_int] to binary string for printing *)
val big_int_to_binary : ?pad:int -> Big_int_Z.big_int -> string

(** Convert [big_int] to hex string for printing *)
val big_int_to_hex : ?pad:int -> Big_int_Z.big_int -> string

(** Convert string to [big_int] *)
val big_int_of_string : string -> Big_int_Z.big_int

(** Convert binary string to [big_int] *)
val big_int_of_binstring : ?e:[`Little | `Big] -> string -> Big_int_Z.big_int

(** execute f with fd_from remapped to fd_to.  useful for redirecting
    output of external code; e.g., redirecting stdout when calling STP
    code. *)
val run_with_remapped_fd :
  Unix.file_descr -> Unix.file_descr -> (unit -> 'a) -> 'a

(* Deprecated: BatList.take *)
(* val take : int -> 'a list -> 'a list *)
(* Deprecated: BatList.append *)
(* val fast_append : 'a list -> 'a list -> 'a list *)

(** {3 Printing/status functions} *)

(** Status printer module *)
module StatusPrinter :
sig
  (** [init s n] starts a new process called [s] with [n] steps. *)
  val init : string -> int -> unit

  (** [inc ()] indicates a step has completed. *)
  val inc  : unit -> unit

  (** [stop ()] indicates the process has completed. *)
  val stop : unit -> unit
end


(** [print_separated_list printer sep l] converts [l] to a string by
    computing [printer e] for each element [e], and concatenating the
    results with [sep]. *)
val print_separated_list : ('a -> string) -> string -> 'a list -> string

(** [print_obj_info s obj] prints the size of object [obj] with string
    [s] in the debug logs if the [UtilSize] module has debugging
    enabled. *)
val print_obj_info : string -> 'a -> unit

(** If the [UtilMemUse] module has debugging enabled, prints
    information about the memory use of the process. *)
val print_mem_usage : unit -> unit
