(* Information gathered while walking through values. *)
type info =
  { data : int
  ; headers : int
  ; depth : int
  }

(* Returns information for first argument. *)
val objsize : 'a -> info

(* Calculates sizes in bytes: *)
val size_with_headers : info -> int
val size_without_headers : info -> int
