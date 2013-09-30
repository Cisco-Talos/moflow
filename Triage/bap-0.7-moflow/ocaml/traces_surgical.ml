(*************************************************************)
(******************** Surgical Slicing  **********************)
(*************************************************************)

open Ast
open Big_int_Z
open Big_int_convenience
module D=Traces.D
open D
open Symbeval
open Traces
open Type

module NameSet = Set.Make(String)

(*  Precise Slicing - may result in underapproximations *)
(*
  The idea is the following: 

    1. Pass in a straight-line program (assertions instead of cjmps)

    2. Convert all memories to scalars (using mem_42 instead of mem[42])

  Then a program of this form:

    x1 = symb_0
    x2 = x1 + 1
    assert (x2 > 0)
    x3 = 4
    x3 = x2 - 5
    assert (x3 = 0)

  should be converted to:

    x1 = symb_0
    x2 = x1 + 1
    assert (x2 > 0)
    x3 = x2 - 5
    assert (x3 = 0)

  and the formula should be:

    let x1 = symb_0 in
    let x2 = x1 + 1 in
     (x2 > 0) &
     let x3 = x2 - 5 in
      (x3 = 0)

*)

let mem_name num =  "mem_"^(Printf.sprintf "%Lx" num)

let used_vars = Hashtbl.create 65536

let newvar num t = 
  if not (Hashtbl.mem used_vars num) then
    (
      let v = Var.newvar (mem_name num) t in
        Hashtbl.add used_vars num v;
        v
    )
  else
    Hashtbl.find used_vars num

(** Transformations needed for traces. *)
let trace_transform_stmt2 stmt evalf = 
  let get_bytes = function
    | Reg n -> n / 8
    | _ -> failwith "cannot load mem!"
  in
  let load num t bytes = 
    let rec load_aux acc shift = function
      | 0 -> acc
      | n -> 
          let var = Var (newvar (Int64.add num (Int64.of_int (bytes - n))) reg_8) in
          let var = Cast(CAST_UNSIGNED, t, var) in
          let acc = BinOp (OR, BinOp(LSHIFT, var, Int(big_int_of_int64 shift, reg_32)), acc) in
            load_aux acc (Int64.add shift 8L) (n-1)
    in
    let var = Var (newvar num reg_8) in
    let exp = Cast(CAST_UNSIGNED, t, var) in
      load_aux exp 8L (bytes-1)
  in
  let concretize idx = match evalf idx with
    | Int (n, t) -> n
    | _ -> failwith "cannot concretize index"
  in
    
  let cvis = object(self)
    inherit Ast_visitor.nop
    method visit_exp = function
      | Load (mem, idx, _endian, t) ->
          let num = concretize idx in
          let bytes = get_bytes t in
          let exp = load (int64_of_big_int num) t bytes in
            ChangeTo exp
      | _ -> DoChildren
    method visit_stmt = function
      | _ -> DoChildren
  end
  in
  let break_move num value t bytes =
    let rec break_aux acc shift = function
      | 0 -> acc
      | n ->
          let var = newvar (Int64.add num (Int64.of_int (bytes-n))) reg_8 in
          let exp = BinOp (RSHIFT, value, Int(big_int_of_int64 shift, reg_32)) in
          let exp = Cast(CAST_UNSIGNED, reg_8, exp) in
          let acc = (Move(var, exp, [])) :: acc in
            break_aux acc (Int64.add shift 8L) (n - 1)
    in
    let var = newvar num reg_8 in
    let exp = Cast(CAST_UNSIGNED, reg_8, value) in
      break_aux [Move(var, exp, [])] 8L (bytes - 1)
  in
    
  (* Concretize memory addresses *)
  let stmt =
    if not !allow_symbolic_indices then
      Ast_visitor.stmt_accept cvis stmt
    else stmt
  in
  let s = Printf.sprintf "Removed: %s" (Pp.ast_stmt_to_string stmt) in
  let com = Ast.Comment(s, []) in
  let s = match stmt with
    | (Ast.CJmp (e,tl,_,atts1)) when full_exp_eq (evalf e) exp_true ->
    	[com; Ast.Assert(e,atts1)]
    | (Ast.CJmp (e,_,fl,atts1)) when full_exp_eq (evalf e) exp_false ->
    	[com; Ast.Assert(UnOp(NOT,e),atts1)]
    | Ast.CJmp _ -> failwith "Evaluation failure!"
    | (Ast.Jmp _) ->
	[com]
	  (* Removing assignment of tainted operands: symbolic
	     execution does not need these *)
    | Ast.Move (v, Store(mem, idx, value, _endian, t), _) ->
        let idx = concretize idx in
        let bytes = get_bytes t in
          break_move (int64_of_big_int idx) value t bytes 
    | Ast.Move (_, _, atts) when List.exists is_tconcassign atts -> []
    | s -> [s] in
    (*if not !allow_symbolic_indices && !exp <> exp_true then
    (* The assertion must come first, since the statement may modify value of the expression! *)
      s @ [Assert(!exp, [])]
      else*)
    s

let get_symbolic_seeds2 memv = function
  | Ast.Comment (s,atts) when is_seed_label s ->
      List.fold_left
	(fun acc {index=index; taint=Taint taint} ->
	   let newvarname = "symb_" ^ (string_of_int taint) in
	   let sym_var = Var (Var.newvar newvarname reg_8) in
	     pdebug ("Introducing symbolic: " 
		     ^(Printf.sprintf "%Lx" index)
		     ^" -> "
		     ^(Pp.ast_exp_to_string sym_var));
	     add_symbolic index sym_var ;
	     (* symbolic variable *)
	     let mem = newvar index reg_8 in
	     let move = Move(mem, sym_var, []) in
	     move::acc				       
	) [] (filter_taint atts)
  | _ -> []

(** Running each block separately *)
let run_and_subst_block state memv thread_map block = 
  let addr, block = hd_tl block in
  let input_seeds = get_symbolic_seeds2 memv addr in
  pdebug ("Running block: " ^ (string_of_int !counter) ^ " " ^ (Pp.ast_stmt_to_string addr));
  let info, block = hd_tl block in
  counter := !counter + 1 ;
  let _ = ignore(update_concrete addr) in
  if !consistency_check then (
    (* remove temps *)
	(* SWXXX *)
    clean_delta state.delta;
    ignore(check_delta state);
    (* TraceConcrete.print_values state.delta; *)
    (* TraceConcrete.print_mem state.delta; *)
  );

  (* Assign concrete values to regs/memory *)
  let block, extra =
    let assigns = assign_vars memv (lookup_thread_map thread_map (get_tid block)) false in
    (* List.iter *)
    (*   (fun stmt -> dprintf "assign stmt: %s" (Pp.ast_stmt_to_string stmt)) assigns;	 *)
    assigns @ block, List.length assigns
  in

  let place = ref 0 in

  let block = append_halt block in 
  TraceConcrete.initialize_prog state block ;
  clean_delta state.delta;
  let init = TraceConcrete.inst_fetch state.sigma state.pc in
  let executed = ref [] in
  let rec eval_block state stmt = 
    pdebug ("Executing: " ^ (Pp.ast_stmt_to_string stmt));
    (*    Hashtbl.iter (fun k v -> pdebug (Printf.sprintf "%Lx -> %s" k (Pp.ast_exp_to_string v))) concrete_mem ;*)
    let evalf e = match TraceConcrete.eval_expr state.delta e with
      | Symbolic(e) -> e
      | _ -> failwith "Expected symbolic" 
    in
    executed := BatList.append input_seeds !executed ;
      if !place >= extra then (
    executed := BatList.append (trace_transform_stmt2 stmt evalf) !executed ; 
      );
        place := !place + 1;
    (*print_endline (Pp.ast_stmt_to_string stmt) ;*)

    try 
      (match TraceConcrete.eval_stmt state stmt with
	 | [newstate] ->
	     let next = TraceConcrete.inst_fetch newstate.sigma newstate.pc in
	       (*pdebug ("pc: " ^ (Int64.to_string newstate.pc)) ;*)
	       eval_block newstate next
	 | _ -> 
	    failwith "multiple targets..."
      )
    with
	(* Ignore failed assertions -- assuming that we introduced them *)
    | TraceConcrete.AssertFailed _ as _e -> 
	  wprintf "failed assertion: %s" (Pp.ast_stmt_to_string stmt);
	  (* raise e; *)
	  let new_pc = Int64.succ state.pc in
	  let next = TraceConcrete.inst_fetch state.sigma new_pc in
	  eval_block {state with pc=new_pc} next
  in
    try
      eval_block state init
    with 
      |	Failure s as e -> 
	  pwarn ("block evaluation failed :(\nReason: "^s) ;
	  List.iter (fun s -> pdebug (Pp.ast_stmt_to_string s)) block ;
	  (*if !consistency_check then ( *)
	    raise e
	  (* ) else 
	  ((addr,false)::(info,false)::(List.tl !executed)) *)
      | TraceConcrete.UnknownLabel _ ->
	  (addr::info::List.rev (!executed))
      | TraceConcrete.Halted _ -> 
	  (addr::info::List.rev (List.tl !executed))

let run_and_subst_blocks blocks memv thread_map length =
  counter := 1 ;
  Status.init "Concrete Substitution Run" length ;
  let state = TraceConcrete.create_state () in
  let rev_trace = List.fold_left 
    (fun acc block -> 
       Status.inc() ;   
       List.rev_append (run_and_subst_block state memv thread_map block) acc
    ) [] blocks
  in
  Status.stop () ;
  List.rev rev_trace

let dicer = 

  let process_stmt outset stmt = 
    let inset = ref NameSet.empty in
    let delset = ref NameSet.empty in
    let vis = object(self)
      inherit Ast_visitor.nop
      method visit_exp = function
        | Ast.Var v -> inset := NameSet.add (Var.name v) !inset ; DoChildren 
        | _ -> DoChildren
    end
    in
    let make_sets = function
      | Ast.Move(v, e, _) ->
	  let name = Var.name v in
	    if NameSet.mem name outset then
	      (
	        ignore (Ast_visitor.exp_accept vis e );
	        delset := NameSet.add name !delset
	      )
      | Ast.Assert(e,_) ->
          ignore (Ast_visitor.exp_accept vis e)
      | _ -> ()
    in
      make_sets stmt;
      !inset, !delset
  in
       
  let rec slicer outset acc = function
    | [] ->
        acc
    | (Ast.Move _ as stmt)::stmts -> 
        let inset, delset = process_stmt outset stmt in
        let outset = NameSet.diff outset delset in
        let outset = NameSet.union outset inset in
          if NameSet.is_empty inset then
            slicer outset acc stmts
          else
            slicer outset (stmt::acc) stmts
    | (Ast.Assert _ as stmt)::stmts ->
        let inset, _ = process_stmt outset stmt in
        if NameSet.is_empty (NameSet.inter inset outset) then
          slicer outset acc stmts
        else
          slicer outset (stmt::acc) stmts
    | stmt::stmts ->
        slicer outset (stmt::acc) stmts
        
  in
    slicer

let concrete_substitution trace = 
  dsa_rev_map := None;
  (*let trace = Memory2array.coerce_prog trace in*)
  let no_specials = remove_specials trace in
  (* let no_unknowns = remove_unknowns no_specials in *)
  let memv = find_memv no_specials in
  let thread_map = create_thread_map_state() in
  let no_specials = explicit_thread_stmts no_specials thread_map in
  let blocks = trace_to_blocks no_specials in
  (*pdebug ("blocks: " ^ (string_of_int (List.length blocks)));*)
  let length = List.length blocks in
  let actual_trace = run_and_subst_blocks blocks memv thread_map length in
    actual_trace

let check_slice trace = 
  let actual_trace = concrete_substitution trace in
  let accurate_trace = dicer (NameSet.singleton "ra") [] (List.rev actual_trace) in
    accurate_trace

(*  Approximate/Conservative Slicing  *)
let slice varname trace = 
  let rev = List.rev trace in
  let maps = ref (NameSet.singleton varname) in
  let vis = object(self)
    inherit Ast_visitor.nop
    method visit_exp = function
      | Ast.Var v -> maps := NameSet.add (Var.name v) !maps ; DoChildren 
      | _ -> DoChildren
  end
  in
  let run_all acc = function 
    | Ast.Move(v, e, _) as s ->
	let name = Var.name v in
	  if NameSet.mem name !maps then
	    (
	      ignore( Ast_visitor.exp_accept vis e );
	      maps := NameSet.remove name !maps ;
	      s::acc
	    )
	  else acc
    | _ -> acc
  in
    List.fold_left run_all [] rev
		   
	    
