(** Scope module for parsing.

    ejs: I moved this out of the grammar.mly file so that external
    users could access it.
*)

(** Whether or not to strip the trailing _number from variable names *)
let strip_nums = ref false

let stripnum =
  let stripnum1 = Str.replace_first (Str.regexp "_[0-9]+$") "" in
  fun x ->
    if !strip_nums then
      stripnum1 x
    else
      x

let err s =
  prerr_endline s;
  failwith ("Parser: "^s)
    (*raise Parsing.Parse_error*)

module Scope = struct
  type t = (string, Var.t) Hashtbl.t * string Stack.t

  let create decls =
    let h = Hashtbl.create 5700 in
    List.iter (fun v -> Hashtbl.add h (Var.name v) v) decls;
    (h, Stack.create() )

  let empty_scope () : t = create []

  let add_var scope n v =
    Hashtbl.add (fst scope) n v;
    v

  let add scope n t =
    let v = Var.newvar (stripnum n) t in
    add_var scope n v

  let add_push scope n t =
    Stack.push n (snd scope);
    add scope n t

  let pop scope =
    let (h,s) = scope in
    let n = Stack.pop s in
    Hashtbl.remove h n

  (** Gets lval if defined, otherwise raises Not_found *)
  let get_lval_if_defined scope n t =
    let n = stripnum n in
    let v = Hashtbl.find (fst scope) n in
    if t = None || t = Some(Var.typ v)
    then v
    else err ("Variable '"^n^"' used with inconsistent type: "^(Pp.typ_to_string (Var.typ v))^" "^(Pp.typ_to_string (BatOption.get t)))

  let get_lval scope n t =
    let n = stripnum n in
    try
      get_lval_if_defined scope n t
    with Not_found ->
      match t with
      | Some t -> add scope n t
      | None -> err ("Type was never declared for '"^n^"'")

end

let empty_scope = Scope.empty_scope
let create_scope_from_decls decls = Scope.create decls

