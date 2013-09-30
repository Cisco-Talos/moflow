(** Convert memory style accesses to array accesses.

   This modules converts all TMem references to normalized array references.

    @author Edward J. Schwartz
*)

(* TODO: Handle different endianness.  Use the type of the array expression *)

(* XXX: What should we do when there is a 1 bit access? *)

module D = Debug.Make(struct let name="memory2array" and default=`Debug end)
open D

open Ast
open Ast_convenience
open BatListFull
open Big_int_Z
open Grammar_scope
open Type
open Util
open Var

(** How big are normalized elements? *)
let bitwidth = 8
let normtype = Reg(bitwidth)

let getwidth = Typecheck.bytes_of_width
(* let getwidth regtyp = *)
(*   match regtyp with *)
(*   | Reg(n) -> assert (n mod bitwidth = 0); n/bitwidth *)
(*   | _ -> failwith "Only support register indices!" *)

let split_load array index indextype accesstype endian bytenum =
  let indexplus = BinOp(PLUS, index, Int(big_int_of_int bytenum, indextype)) in
  let exp = Load(array, indexplus, endian, normtype) in
  let exp = Cast(CAST_UNSIGNED, accesstype, exp) in
  let exp = exp_shl exp (Int(big_int_of_int (bytenum * bitwidth), accesstype)) in
  exp

let split_load_list array index indextype accesstype endian =
  assert (endian === exp_false);
  let elesize = getwidth accesstype in
  let mvar = Var_temp.nt "loadnorm" (Array(indextype, normtype)) in
  (Util.mapn (split_load (Var mvar) index indextype accesstype endian) (elesize - 1), mvar)

let split_loads array index accesstype endian =
  let indextype = Typecheck.infer_ast index in
  let (singlereads, mvar) = split_load_list array index indextype accesstype endian in
  let orexp = List.fold_left exp_or (List.hd singlereads) (List.tl singlereads) in
  Let(mvar, array, orexp)

let split_write array index indextype accesstype endian data bytenum =
  let indexplus = BinOp(PLUS, index, Int(big_int_of_int bytenum, indextype)) in
  let exp = exp_shr data (Int(big_int_of_int (bytenum * bitwidth), accesstype)) in
  let exp = Cast(CAST_LOW, normtype, exp) in
  let exp = Store(array, indexplus, exp, endian, normtype) in
  exp

let split_write_list array index accesstype endian data =
  assert (endian === exp_false);
  let inftype = Typecheck.infer_ast array in
  let indextype = Typecheck.infer_ast index in
  let tempmemvar = Var_temp.nt "tempmem" inftype in
  let tempvalvar = Var_temp.nt "tempval" accesstype in
  let elesize = getwidth accesstype in
  let singlewrites = Util.mapn (split_write (Var tempmemvar) index indextype accesstype endian (Var tempvalvar)) (elesize - 2) in
  (singlewrites @ [(split_write array index indextype accesstype endian (Var tempvalvar) (elesize - 1))], tempmemvar, tempvalvar)

let split_writes array index accesstype endian data =
  let (singlewrites, tempmemvar, tempvalvar) = split_write_list array index accesstype endian data in
  let letexp = List.fold_left (fun expr new_expr -> Let(tempmemvar, new_expr, expr)) (Var tempmemvar) singlewrites in
  Let(tempvalvar, data, letexp)


(** This visitor maps each TMem to an array *)
class memory2array_visitor ?scope hash =
(*  let hash = VarHash.create 1000 in *)

  let get_array_var (Var.V(_, _, t) as avar) =
    match t with
    | TMem (idxt) ->
      let array =
        try VarHash.find hash avar
        with Not_found ->
        (* We haven't mapped variable v before. If a scope of
           variables was provided, let's see if v_array is in
           scope. *)
          let new_name = (Var.name avar)^"_array" in
          let new_type = Array (idxt, Reg bitwidth) in
          let make_new_var () = newvar new_name new_type in
	  let newarrvar = match scope with
            | Some(scope) ->
              (try Scope.get_lval_if_defined scope new_name (Some new_type)
              with Not_found -> make_new_var ())
            | None ->
              make_new_var ()
          in

          (* Map in the m2a hash *)
	  VarHash.add hash avar newarrvar;

          (* Update the scope if defined *)
          (match scope with
          | Some(scope) ->
          (* First, ensure that if new_name is already in Scope, that
             it maps to newarrvar.  If not, the memory2array hash and
             Scope are out of sync, which indicates someone did
             something bad. *)
            (try assert (Var.equal (Scope.get_lval_if_defined scope new_name (Some new_type)) newarrvar)
             with Not_found -> (* If not in Scope, it's okay *) ());

            (* Now, update the Scope *)
            ignore(Scope.add_var scope new_name newarrvar);

          | None -> ());

	  newarrvar
      in
      array
    | _ -> failwith "get_array_var expects a TMem variable only"
  in

object (self)
  inherit Ast_visitor.nop

  method visit_avar avar =
    match Var.typ(avar) with
    |	TMem(idxt) ->
      ChangeToAndDoChildren (get_array_var avar)
    |	_ ->
      DoChildren

  method visit_rvar = self#visit_avar

  method visit_exp exp =
      (* Printf.printf "Visiting expression %s\n" (Pp.ast_exp_to_string exp); *)
      match exp with
      | Load(arr,idx,endian,t) -> ((* Printf.printf "Load %s\n" (Pp.ast_exp_to_string exp); *)
	  let width = (getwidth t) in
	  match width with
	  | 1 -> (* Printf.printf "Cool\n"; *)
	      DoChildren
	  | _ -> (* Printf.printf "Need to split\n"; *)
	    let arr = Ast_visitor.exp_accept self arr in
	    let newexpr = split_loads arr idx t endian
	    in
	    (* Printf.printf "New Load %s\n" (Pp.ast_exp_to_string newexpr); *)
	    (* djb: still need to descend into children *)
	    ChangeToAndDoChildren newexpr)
      | Store(arr,idx,data,endian,t) -> ((* Printf.printf "Store %s %s %s Reg%d\n" (Pp.ast_exp_to_string arr) (Pp.ast_exp_to_string idx) (Pp.ast_exp_to_string data) (getwidth t); *)
          let width = (getwidth t) in
          match width with
          | 1 -> (* Printf.printf "Cool!\n"; *)
	      DoChildren
          | _ -> (* Printf.printf "Need to split\n"; *)
	      let arr = Ast_visitor.exp_accept self arr in
              let newexpr = split_writes arr idx t endian data in
	      ChangeToAndDoChildren newexpr
        )
      | _ -> DoChildren
  end


(** deend your average program. Returns new program where all memory
    broken down to byte-level reads and writes using array variables
    with the same name as the old memory variables.  *)

type state = Ast.var VarHash.t

let create_state () = VarHash.create 1000

let coerce_prog prog =
  let hash = create_state () in
  let visitor = new memory2array_visitor hash in
  Ast_visitor.prog_accept visitor prog

let coerce_prog_state ?scope hash prog =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.prog_accept visitor prog

let coerce_exp exp =
  let hash = create_state () in
  let visitor = new memory2array_visitor hash in
  Ast_visitor.exp_accept visitor exp

let coerce_exp_state ?scope hash exp =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.exp_accept visitor exp

let coerce_rvar_state ?scope hash v =
  let visitor = new memory2array_visitor ?scope hash in
  Ast_visitor.rvar_accept visitor v
