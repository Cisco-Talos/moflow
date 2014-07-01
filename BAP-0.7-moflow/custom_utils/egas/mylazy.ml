
type 'a node_t =
  | Nil
  | Cons of 'a * 'a zlist_t
and 'a zlist_t = 'a node_t lazy_t

let rec map f zlst = lazy (
  match Lazy.force zlst with
  | Nil -> Nil
  | Cons(h, t) -> Cons(f h, map f t)
)

let force_all zlst = 
  let rec aux acc zlst = 
    match Lazy.force zlst with
    | Cons(x, zl) -> aux (x::acc) zl
    | Nil -> List.rev acc
  in
  aux [] zlst
