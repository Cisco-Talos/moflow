(* Forward and backward slicing *)

open List
(* BAP *)
module Astv = Ast_visitor
module T = Type
module Sy = Symbeval
module Tr = Traces
module TrS = Traces_surgical
module NameSet = TrS.NameSet
module U = Util
module Status = Util.StatusPrinter
(* self *)
module Cm = Common
open Cm.D (* dprintf *)


let mEM_VAR = "mem"
type direction = Forward | Backward 
exception EarlyStop of Ast.stmt list

let make_var_collector maps = 
  let vis = object(self)
    inherit Astv.nop
    method visit_exp = function
      | Ast.Var v -> 
          let vname = Var.name v in
          maps := NameSet.add vname !maps ; 
          T.DoChildren 
      | _ -> T.DoChildren
  end
  in
  vis

(* FIXME: Needs SSA? *)
let slice varname trace dir = 
  let maps = ref (NameSet.singleton varname) in
  let vis = make_var_collector maps in
  let try_to_bail acc =
      if NameSet.is_empty !maps then 
          raise (EarlyStop acc)
      else
          acc
  in
  let exp_shares_vars exp maps = 
    let e_vars = ref (NameSet.empty) in
    let collector = make_var_collector e_vars in
    let _ = Astv.exp_accept collector exp in
    not (NameSet.is_empty (NameSet.inter !maps !e_vars))
  in
  (* backward *)
  let f_backward acc stmt =
    match stmt, acc with
    | Ast.Move(v, e, attrs), _ ->
        let name = Var.name v in
        let _ = 
          if name = mEM_VAR then
            failwith "Global 'mem' var detected. Concretize your trace."
          else () in
        if NameSet.mem name !maps then
          (
            maps := NameSet.remove name !maps ;
            (* Add referenced vars to 'maps' *)
            ignore( Astv.exp_accept vis e );
            let acc = stmt::acc in
            try_to_bail acc
          )
        else acc
    | Ast.Assert(e, _), _ when exp_shares_vars e maps ->
        stmt::acc
    (* Do not add unnecessary labels *)
    | Ast.Label(T.Name _, _), (Ast.Move _ :: tl)
    | Ast.Label(T.Name _, _), (Ast.Assert _ :: tl)
    | Ast.Label(T.Addr _, _), (Ast.Label(T.Name _, _) :: tl) ->
        stmt::acc
    | _ -> acc
  in
  (* forward *)
  let f_forward acc stmt =
    match stmt, acc with
    | Ast.Move(v, e, _), _ ->
        let v_name = Var.name v in
        if not (exp_shares_vars e maps) then (
            maps := NameSet.remove v_name !maps ;
            try_to_bail acc
        )
        else (
            maps := NameSet.add v_name !maps;
            stmt::acc
        )
    | Ast.Assert(e, _), _ when exp_shares_vars e maps ->
        stmt::acc
    (* Do not add unnecessary labels *)
    | Ast.Label(T.Addr _, _), (Ast.Move _ :: tl)
    | Ast.Label(T.Addr _, _), (Ast.Assert _ :: tl)
    | Ast.Label(T.Name _, _), (Ast.Label(T.Addr _, _) :: tl) ->
        stmt::acc
    (* Remove labels without stmts *)
    | Ast.Label(T.Addr _, _), (Ast.Label(T.Name _, _) :: _ :: tl) ->
        stmt::tl
    | _ -> acc
  in
  let cut_orphan_labels = function
    | Ast.Label _ :: Ast.Label _ :: tl -> tl
    | l -> l
  in
  let walk_trace f_stmt trace = 
    try List.fold_left f_stmt [] trace
    with EarlyStop(trace) -> trace 
  in
  let cut_up_to pred trace =
    let rec aux acc stmts =
      match stmts with
      | st::tl -> 
        if pred st then List.rev tl else aux (st::acc) tl
      | [] -> acc
    in
    let trace = aux [] trace in
    List.rev trace
  in
  match dir with
    | Forward -> 
      let pred_first_move = function 
        | Ast.Move(v,e,_) -> Var.name v = varname 
        | _ -> false
      in
      let trace = cut_up_to pred_first_move trace in
      let trace = walk_trace f_forward trace in
      let trace = cut_orphan_labels trace in
      List.rev trace
    | Backward -> 
      let trace = walk_trace f_backward (List.rev trace) in
      trace

let check_arg arg bad_val msg =
    if arg=bad_val then
        raise(Arg.Bad(msg)) 
    else () 

let main () =
    let usage = "Usage: "^Sys.argv.(0)^" <options>\n" in
    let tRACE_FN = ref "" in
    let vAR_NAME = ref "" in
    let sLICE_BACKWARD = ref true in
    let oUT_FILE = ref "/dev/stdout" in
    let spec = [
        ("-il", Arg.Set_string(tRACE_FN), "<trace.il>");
        ("-var", Arg.Set_string(vAR_NAME), "<var to slice by>");
        ("-f", Arg.Clear(sLICE_BACKWARD), "slice forward");
        ("-b", Arg.Set(sLICE_BACKWARD), "slice backward (default)");
        ("-o", Arg.Set_string(oUT_FILE), "output file (default: /dev/stdout)");
    ] in
    let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'")) in
    let _ = Arg.parse spec anon usage in
    let _ = check_arg !tRACE_FN "" "No trace file" in
    let _ = check_arg !vAR_NAME "" "No var name" in
    let trace = Cm.read_il_from_file !tRACE_FN in
    let dir = if !sLICE_BACKWARD then Backward else Forward in
    let sliced = slice !vAR_NAME trace dir in
    let _ = Cm.pp_ast !oUT_FILE sliced in
    ()

let _ = main()

