(* Type checking for BAP.
*)

open Type
open Ast
open Big_int_Z
open Big_int_convenience
module D = Debug.Make(struct let name="Typecheck" and default=`NoDebug end)
open D

exception TypeError of string

let terror s = raise(TypeError s)

(* returns true if t1 is a subtype of t2 *)
let subt t1 t2 =
  t1 = t2

let check_subt t1 t2 f =
  if not(subt t1 t2) then
    terror (Printf.sprintf f (Pp.typ_to_string t1) (Pp.typ_to_string t2))

let is_integer_type = function
  | Reg _ -> true
  | TMem _ | Array _ -> false

let is_mem_type t = not (is_integer_type t)

let bits_of_width = function
  | Reg n -> n
  | _ -> invalid_arg "bits_of_width"

let bytes_of_width t =
  let b = bits_of_width t in
  if not ((b mod 8) = 0) then invalid_arg "bytes_of_width";
  b / 8

let rec infer_ast_internal check e =
  match e with
  | Var v ->
      (* FIXME: Check context *)
    Var.typ v
  | UnOp(_, e) ->
    if check then 
      (let t = infer_ast_internal true e in
       check_reg t);
    infer_ast_internal false e;
  | BinOp(o,e1,e2) as e ->
    if check then (
      let t1 = infer_ast_internal true e1
      and t2 = infer_ast_internal true e2 in
      check_same t1 t2 ~e;
      match o with
      | EQ | NEQ -> ()
      | _ -> check_reg t1);
    (match o with
    | EQ | NEQ | LT | LE | SLT | SLE -> reg_1
    | _ -> infer_ast_internal false e1
    )
  | Ite(b,e1,e2) ->
    if check then 
      (let t1 = infer_ast_internal true e1
       and t2 = infer_ast_internal true e2 in
       check_same t1 t2);
    infer_ast_internal false e1
  | Extract(h,l,e) ->
    let ns = int_of_big_int(h -% l +% bi1) in
    let nt = Reg ns in
    if check then (
      match infer_ast_internal true e with
      | Reg(oldn) ->
	if (ns <= 0) then terror("Extract must extract at least one bit");
	if l <% bi0 then terror("Lower bit index must be at least 0");
	if h >% (big_int_of_int oldn) -% bi1 then terror("Upper bit index must be at most one less than the size of the original register")
      | _ -> terror ("Extract expects Reg type")
    );
    nt
  | Concat(le, re) ->
    let lt, rt = infer_ast_internal check le, infer_ast_internal check re in
    let nt = match lt, rt with
      | Reg(lb), Reg(rb) ->
	Reg(lb+rb)
      | _ -> terror "Concat expects Reg type"
    in
    nt
  | Lab s ->
      (* FIXME: no type for labels yet *)
    reg_64
  | Int(_,t)
  | Unknown(_,t) ->
    t
  | Cast(ct,t,e) ->
    let te = infer_ast_internal check e in
    if check then (
      check_reg t;
      check_reg te;
      let bitse = bits_of_width te in
      let bitst = bits_of_width t in
      match ct with
      | CAST_UNSIGNED
      | CAST_SIGNED ->
        if bitst < bitse then terror (Printf.sprintf "Cast type %s is a widening case, but it was used to narrow %s to %s" (Pp.ct_to_string ct) (Pp.typ_to_string te) (Pp.typ_to_string t))
      | CAST_HIGH
      | CAST_LOW ->
        if bitst > bitse then terror (Printf.sprintf "Cast type %s is a narrowing case, but it was used to widen %s to %s" (Pp.ct_to_string ct) (Pp.typ_to_string te) (Pp.typ_to_string t))
    );
    t
  | Let(v,e1,e2) ->
    (* XXX: Need a type context to check this correctly *)
    if check then ignore(infer_ast_internal true e1);
    infer_ast_internal check e2
  | Load(arr,idx,endian, t) ->
    if check then check_idx arr idx endian t;
    t
  | Store(arr,idx,vl, endian, t) ->
    if check then (
      check_idx arr idx endian t;
      let tv = infer_ast_internal true vl in
      check_subt tv t "Can't store value with type %s as a %s";
    );
    infer_ast_internal false arr

and check_same ?e ?s t1 t2 =
  if t1 <> t2 then
    let probs = match e, s with
      | Some e, _ -> ("\nProblem expression: "^(Pp.ast_exp_to_string e))
      | _, Some s -> ("\nProblem statement: "^(Pp.ast_stmt_to_string s))
      | None, None -> "" in
    terror ("Similar types expected: "^(Pp.typ_to_string t1)^" <> "^(Pp.typ_to_string t2)^probs)

and check_reg t =
  if not (is_integer_type t) then
    terror (Printf.sprintf "Expected integer type, but got %s" (Pp.typ_to_string t))

and check_bool t =
  if t <> Reg 1 then
    terror (Printf.sprintf "Expected bool type, but got %s" (Pp.typ_to_string t))

and check_idx arr idx endian t =
  let ta = infer_ast_internal true arr
  and ti = infer_ast_internal true idx
  and te = infer_ast_internal true endian in
  if te <> reg_1 then terror "Endian must be a boolean";
  if not(is_integer_type ti) then terror "Index must be a register type";
  match ta with
  | Array(i,e) ->
      check_subt ti i "Index type not suitable for indexing into this array. Index has type %s, but array has type %s.";
      check_subt t e "Can't get a %s from array of %s";
  | TMem i -> check_subt ti i "Index type not suitable for indexing into this array. Index has type %s, but the array has type %s.";

  | _ -> terror "Indexing only allowed from array or mem."

let infer_ast = infer_ast_internal false

let typecheck_expression e = ignore(infer_ast_internal true e)

(* Quick, informal, AST statement type checking.

   XXX: Formal type checking rules!
*)
let typecheck_stmt =
  let infer_te = infer_ast_internal true in
  function
    | Move(v, e, _) as s ->
      let vt = Var.typ v in
      let et = infer_te e in
      check_same ~s vt et
    | Jmp(e, _) ->
      let et = infer_te e in
      check_reg et
    | CJmp(ce, t1, t2, _) ->
      let et = infer_te ce in
      let t1t = infer_te t1 in
      let t2t = infer_te t2 in
      check_bool et;
      check_reg t1t;
      check_reg t2t
    | Halt(e, _) ->
      let et = infer_te e in
      (* Can we return a memory? Does this make sense? *)
      check_reg et
    | Assert(e, _)
    | Assume(e, _) ->
      let et = infer_te e in
      check_bool et
    | Label _
    | Comment _
    | Special _ ->
      ()

let typecheck_prog =
  List.iter typecheck_stmt
