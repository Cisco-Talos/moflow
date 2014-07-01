module L = List
module Hsh = Hashtbl
module Pr = Printf

module Ty = Type
module Tr = Traces
module Sy = Symbeval
module Big = Big_int_Z
module Astv = Ast_visitor
module Var = Var
module VarSet = Var.VarSet
module G = Graph

module Cm = Common
module La = Mylazy
module D = Cm.D

let big2int = Big.int_of_big_int

let tmr_solver = new Stats.timer "solver interaction"
let tmr_actual_solver = new Stats.timer "actual solver interaction"

let timers = [tmr_solver; ] 
let _ = L.map (fun t -> Stats.register_timer t) timers

(* The variables that we care about *)
let is_input v = String.sub v 0 4 = "symb" 

(*****************************************************************************
 * Adapter/Formula/Branch modules for non-incremental solvers
 *****************************************************************************)

(* Convert from user_init to form_init, which has to be the same as Form.init
 * type. *)
module AdapterForSTP =
struct
  type user_init = (Ast.exp -> Ast.exp) list
  type form_init = user_init
  let adapt l = l
end

(*Store all constraints in a list,deferring construction of the final formula.*)
module FormulaForSTP =
struct
  open Ast
  open Type
  open Symbeval (* For Equal | Rename *)
  type output_elt = (Ast.exp -> Ast.exp)
  type preds_list = output_elt list
  type t = {lets: preds_list; asserts: preds_list}
  type init = preds_list
  type output = preds_list

  let init l = {lets=[]; asserts=[]}

  let make_constr expression typ = 
    (match expression, typ with
     | _, Equal -> 
       (fun newe -> (BinOp(AND, expression, newe))), true
     | BinOp(EQ, Var v, value), Rename ->
       (fun newe -> (Let(v, value, newe))), false
     | _ -> failwith "CollectingLetBind: malformed constraint"
    )
  
  let compose inner outer = fun newe -> outer (inner newe)

  let add_to_formula c expression typ =
    let f_constr, is_assert = make_constr expression typ in
    if is_assert then
      let f_assert = L.fold_left compose f_constr c.lets in
      {lets=[]; asserts=f_assert::c.asserts}
    else
      {c with lets=f_constr::c.lets}

  (* Consumer will construct the final formula(s) himself. *)
  let output_formula c = L.rev c.asserts
end

(* Branch module for non-incremental solver *)
module BranchForSTPIn : Branches.BRANCH_IN with type lbl_t = Ty.label with type 
  dir_t = bool with type exp_t = Ast.exp -> Ast.exp with type form_acc_t =
  (Ast.exp -> Ast.exp) option =
struct
  type lbl_t = Ty.label
  type dir_t = bool
  type exp_t = Ast.exp -> Ast.exp
  type form_acc_t = (Ast.exp -> Ast.exp) option
 
  let id = (fun e -> e)
  (* Take the current path formula and current assert. 
   * Return (path_formula+assert, path_formula + negated assert)
   *)
  let append_or_negate f_formula f_exp = 
    (* Negate current assert *)
    let neg = Ast.UnOp(Ty.NOT, f_exp Ast.exp_true) in
    (* Construct path formula with last assert negated *)
    let neg_path = f_formula neg in
    (* Append current assert to path prefix *)
    let new_f = fun newe -> f_formula (f_exp newe) in 
    new_f, neg_path

  let produce_path_formula form_acc f_exp = 
    let f_acc = 
      match form_acc with
      | None -> id
      | Some(f_acc) -> f_acc 
    in
    let form_acc, neg_path = append_or_negate f_acc f_exp in
    Some(form_acc), neg_path

  let label2str lbl = Pp.label_to_string lbl
end

module BranchForSTP = Branches.MakeBranch(BranchForSTPIn)

module TraceSymbolicForSTP = Tr.MakeTraceSymbolic(Sy.DontEvalSymbLet)
  (Tr.PredAssignTraces) (AdapterForSTP)(FormulaForSTP)

(* model is a list of (var_name:string, value:bigint) pairs 
 * Return sorted list of pairs (var_id:int, value:int) *)
let parse_model model =
  let small_int x = (0<=x && x<=255) in

  let var_vals = model  in
  (* A special function to sort interesting variables by name *)
  let underscore = Str.regexp_string "_" in
  let split_var = Str.split underscore in
  let var_to_string_num var = List.nth (split_var var) 1 in
  let var_to_num var = int_of_string (var_to_string_num var) in
  let sort =
    let sort_aux (var1, _) (var2,_) =
      compare (var1) (var2)
    in
    List.sort sort_aux
  in
  let vars = List.filter (fun (v,_) -> is_input v) var_vals in
  let vars = List.map (fun (s,v) -> (var_to_num s, big2int v)) vars in
  let small = L.for_all small_int (snd (L.split vars)) in
  if not small then
    failwith "Model includes value outside byte range"
  else
    let sorted = sort vars in
    sorted

let solver_loop solver_step formula_acc branches =
  let produce_model idx formula_acc branch = 
    D.dprintf "Flipping branch: %d" idx;
    tmr_solver#start;
    let formula_acc, model = solver_step formula_acc branch in
    tmr_solver#stop;
    let msg = BatOption.map_default (fun _ -> "Success!") "Fail" model in
    D.dprintf "%s" msg;
    (* Only parse Some(m) models. 
     * model is a list of (var id, int value) list *)
    let model = BatOption.map parse_model model in
    formula_acc, model
  in
  let rec lazy_solving = function
    | (_, _, []) -> La.Nil
    | (idx, formula_acc, branch::tl) -> 
      let formula_acc', model = produce_model idx formula_acc branch in
      La.Cons(model, lazy (lazy_solving (idx+1, formula_acc', tl)))
  in
  lazy (lazy_solving (0, formula_acc, branches))

module BoxSTPIn =
struct
  module Br = BranchForSTP

  type pred_t = Br.t

  let solve_branches flip_limit prefix suffix = 
    let smt = Smtexec.STP.si in
    let save_formula formula file =
      let oc = open_out file in
      let pp = smt#printer oc in
      pp#assert_ast_exp formula;
      pp#flush;
      pp#close;
    in
    let save_and_solve pred formula_storage = 
      save_formula pred formula_storage;
      match smt#solve_formula_file ~getmodel:true formula_storage with
      (* This means formula is satisfiable *)
      | Smtexec.Invalid m -> m
      (* Fail on all errors. *)
      | Smtexec.SmtError s -> failwith s
      | _ -> None
    in
    let solver_step formula_acc branch = 
      let formula_acc, neg_path = Br.produce_path_formula formula_acc branch in
      let formula_storage = "formula.txt" in
      (* model is a sorted list of (var_name, big int value) *)
      let model = save_and_solve neg_path formula_storage in
      formula_acc, model
    in
    let build_prefix_formula acc branch =
        let acc, _ = Br.produce_path_formula acc branch in
        acc
    in
    let formula_acc = L.fold_left build_prefix_formula None prefix in
    let models = solver_loop solver_step formula_acc suffix in
    models

end

(*****************************************************************************
 * Adapter/Formula/Branch modules for incremental solvers (like z3)
 *****************************************************************************)
(* These two store lets/asserts in a list of pairs:
 * (list of lets, assert). This format can be easily used by z3.
 *)
module AdapterForZ3 =
struct
  type user_init = (Ast.exp list * Ast.exp) list
  type form_init = user_init
  let adapt l = l
end

(*Store all constraints in a list,deferring construction of the final formula.*)
module FormulaForZ3 =
struct
  open Ast
  open Type
  open Symbeval (* For Equal | Rename *)
  type exp_list = Ast.exp list 
  type output_elt = (Ast.exp list * Ast.exp)
  type io =  output_elt list
  type t = {lets: exp_list; asserts: io}
  type init = io
  type output = io

  let init _ = {lets=[]; asserts=[]}

  let make_exp expression typ = 
    match expression, typ with
    | _, Equal -> expression, true
    | BinOp(EQ, Var v, value), Rename ->
      let letx = Let(v, value, Ast.exp_true) in
      letx, false
    | _ -> failwith "FormulaForZ3: malformed constraint"
  
  let add_to_formula c expression typ =
    let exp, is_assert = make_exp expression typ in
    if is_assert then
      {lets=[]; asserts=(L.rev c.lets, exp)::c.asserts}
    else
      {c with lets=exp::c.lets}

  let output_formula c = L.rev c.asserts
end


module BranchForZ3In =
struct
  type lbl_t = Ty.label
  type dir_t = bool
  type exp_t = (Ast.exp list * Ast.exp)
  type form_acc_t = unit option

  let produce_path_formula _ _ = 
    assert false

  let label2str lbl = Pp.label_to_string lbl
end

module BranchForZ3 = Branches.MakeBranch(BranchForZ3In)

module type BOX = 
sig
  type pred_t
  val solve_branches : int -> pred_t list -> pred_t list -> 
                      (int * int) list option La.node_t lazy_t
end

module type MyFlexibleFormula =
sig
  type t
  type init
  type output_elt
  type output = output_elt list
  val init : init -> t 
  val add_to_formula : t -> Ast.exp -> Sy.form_type -> t
  val output_formula : t -> output
end

module BoxZ3In =
struct
  module Br = BranchForZ3
  
  module Fp = Formulap

  type pred_t = Br.t 
  
  let get_all_free_vars prefix suffix = 
    let per_branch g_acc branch = 
      let f f_acc = function
        | Ast.Let(v, w, _) -> (fun newe -> f_acc (Ast.Let(v, w, newe)))
        | exp -> (fun newe -> f_acc (Ast.BinOp(Ty.AND, exp, newe)))
      in
      let (lets, exp_assert) = Br.get_assert branch in
      let f_acc = L.fold_left f g_acc lets in
      f f_acc exp_assert
    in
    let g_acc = L.fold_left per_branch (fun e -> e) prefix in
    let g = L.fold_left per_branch g_acc suffix in
    (* Whole formula *)
    let exp = g Ast.exp_true in
    (* List of Ast.Var(v) *)
    Fp.freevars exp
  
  let make_model z3 free_vars =
    let ast_const_unwrap = function
      | Ast.Int(x,_) -> x
      | _ -> assert false
    in
    (* XXX: Seems like model_exp can use different models in every invocation. 
     * We can't split checking/getting values into two steps. *)
    let var2value var = 
      try
        let v = z3#model_exp (Ast.Var(var)) in
        let v = ast_const_unwrap v in
        (* D.dprintf "Var: %s in model. Value: 0x%02x" (Var.name var) (big2int v);
         * *)
        Some(var, v)
      with 
      | Solver.Solver_error _ -> None
      | _ -> failwith "is_in_model: unexpected exn type"
    in
    let var2str = Var.name in
    if z3#is_sat then 
      let var_val = L.map var2value free_vars in
      let var_val = Cm.keep_some var_val in
      (* List of var names (strings) in the model *)
      let model = L.map (fun (var,v) -> (var2str var, v)) var_val in
      Some(model)
    else
      None

  let solve_branches flip_limit prefix suffix = 
    (* If BAP wasn't compiled with z3 bindings, invoking any method of z3 object
      * will result in failwith "Solver Unimplemented" *)
    let z3 = Solver.Z3.newsolver () in
    (* List of Ast.Var(v) *)
    let free_vars = get_all_free_vars prefix suffix in

    let add_branch_assert ?(negate=false) branch =
      let add_let = function
        | Ast.Let(v, expr, _) -> z3#add_binding v expr
        | _ -> failwith "add_branch_assert: unexpected exp type"
      in
      let (lets, exp_assert) = Br.get_assert branch in
      let exp_assert = 
        if negate then Ast.UnOp(Ty.NOT, exp_assert) 
        else exp_assert
      in
      let _ = L.iter add_let lets in
      z3#add_constraint exp_assert
    in
    let solver_step _ branch = 
      z3#push;
      add_branch_assert ~negate:true branch;
      let model = make_model z3 free_vars in
      z3#pop;
      add_branch_assert ~negate:false branch;
      (), model
    in
    let build_prefix_formula prefix = L.iter add_branch_assert prefix in
    build_prefix_formula prefix;
    solver_loop solver_step () suffix

end

module BoxZ3OptiIn =
struct
  module Br = BoxZ3In.Br
  module Fp = BoxZ3In.Fp
  module ExpSet = 
    Set.Make( 
      struct
        let compare = Pervasives.compare
        type t = Ast.exp
      end)

  module IntSet = 
    Set.Make( 
      struct
        let compare = Pervasives.compare
        type t = int
      end)
  
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(
    struct 
      type t = int 
      let compare = Pervasives.compare
      let hash x = x
      let equal = (=)
    end)

  module Components = Graph.Components.Make(G)
  module Dot = Graph.Graphviz.Dot(struct
       include G (* use the graph module from above *)
       (* Light blue for edges. *)
       let edge_attributes _ = []
       let default_edge_attributes _ = []
       let get_subgraph _ = None
       let vertex_attributes _ = []

       let vertex_name id = string_of_int id
       let default_vertex_attributes _ = []
       let graph_attributes _ = []
    end)
  
  (* FIXME: use this in z3/stp? *)
  type def_assume = Def of Var.t * Ast.exp | Assume of Ast.exp
  (* Solver state for each SCC. solver is the solver object, index is the index
   * of the last assert pushed to solver. *)
  type z3_state = {solver: Solver.Z3.solver; index: int}
  type pred_t = Br.t 
  
  let get_all_free_vars = BoxZ3In.get_all_free_vars
  
  let make_model = BoxZ3In.make_model

  let solve_branches flip_limit prefix suffix = 
    let free_vars = get_all_free_vars prefix suffix in
    let new_solver () = 
      let z3 = Solver.Z3.newsolver () in
      z3
    in
    let new_z3_state () = 
      let s = new_solver () in
      {solver=s; index=(-1)}
    in
    (*
    (* How many times did we flip a branch? *)
    let flip_counts = Hsh.create 256 in
    (* cset contains path's constraints. If exp is in cset, then subsitute 
     * exp := Ast.exp_true, because exp was already asserted. 
     * Return cset += {exp} *)
    let add_branch_assert cset ?(negate=false) branch =
      let add_let = function
        | Ast.Let(v, expr, _) -> 
          z3#add_binding v expr
        | _ -> failwith "add_branch_assert: unexpected exp type"
      in
      let maybe_negate exp = if negate then Ast.UnOp(Ty.NOT, exp) else exp in
      let maybe_subst_with_true exp = 
        if ExpSet.mem exp cset then (
          let s = Pp.ast_exp_to_string exp in
          D.dprintf "OPT, assert substituted with exp_true: %s" s;
          Ast.exp_true)
        else exp
      in
      let (lets, exp_assert) = Br.get_assert branch in
      let exp_assert = maybe_negate exp_assert in
      let exp_assert = maybe_subst_with_true exp_assert in 
      let _ = L.iter add_let lets in
      let _ = z3#add_constraint exp_assert in
      ExpSet.add exp_assert cset
    in
    *)
    (*
    (* Increment and return number of flips for a branch. *)
    let inc_flips branch = 
      let lbl = Br.get_label branch in
      let _ = 
        if not (Hsh.mem flip_counts lbl) then Hsh.add flip_counts lbl 0 
        else () 
      in
      let flips = Hsh.find flip_counts lbl in
      let _ = Hsh.replace flip_counts lbl (flips+1) in
      flips+1
    in
    let calc_model_for_negated cset branch = 
      z3#push;
      (* Ignore new cset, since the negated branch is going away. *)
      let _ = add_branch_assert cset ~negate:true branch in
      let model = make_model z3 free_vars in
      z3#pop;
      model
    in
    (*
    let solver_step cset branch = 
      let flips = inc_flips branch in
      (* Don't flip the same branch too many times. *)
      let model = 
        if flips <= flip_limit then 
          calc_model_for_negated cset branch
        else (
          D.dprintf "OPT, skipping branch because flips=%d>flip_limit=%d" flips
            flip_limit;
          None
        )
      in
      let cset = add_branch_assert cset ~negate:false branch in
      cset, model
    in
    *)
    *)
    let make_var_collector vset = 
      let vis = object(self)
        inherit Astv.nop
        method visit_exp = function
          | Ast.Var v -> 
            vset := VarSet.add v !vset; 
            Ty.DoChildren 
          | _ -> Ty.DoChildren
      end
      in
      vis
    in
    (* Make sure we work on SSA form. *)
    let check_if_ssa branches = 
      let update_let_defs lets defs = 
        let f defs = function 
          | Ast.Let(v,_,_) -> 
            if Var.VarSet.mem v defs then raise (Failure "redefinition")
            else Var.VarSet.add v defs 
          | _ -> assert false
        in
        L.fold_left f defs lets
      in
      let rec aux defs branches =
        match branches with
        | branch::tl ->
          let (lets, _) = Br.get_assert branch in
          (try
            let defs = update_let_defs lets defs in
            aux defs tl
          with Failure _ ->
            false
          )
        | [] -> true
      in
      aux Var.VarSet.empty branches
    in
    (* exp->set of vars that influence it. *)
    let vars_used_in exp = 
      let var_set = ref (VarSet.empty) in
      let get_vars = make_var_collector var_set in
      let _ = Astv.exp_accept get_vars exp in
      !var_set
    in
    (* sigma[i]=ith exp *)
    let build_sigma exps = 
      let sigma = Hsh.create 256 in
      let _ = BatList.iteri (fun i exp -> Hsh.add sigma i exp) exps in
      sigma
    in
    (* Fill defs hashtable and collect indexes of assume stmts. *)
    let walk sigma exps = 
      let handle_vars (defs, assume_idxs, i) exp = 
        match exp with
        | Def(v, e) ->
          (* v is defined here *)
          let _ = Hsh.replace defs v i in
          (defs, assume_idxs, i+1)
        | Assume(e) ->
          (defs, i::assume_idxs, i+1)
      in
      let defs = Hsh.create 256 in
      let (defs, assume_idxs, _) = L.fold_left handle_vars (defs, [], 0) exps in
      (defs, L.rev assume_idxs)
    in
    let build_graph defs exps =
      let g = G.create () in
      let add_v i = 
        let v = G.V.create i in 
        G.add_vertex g v;
        v
      in
      (* Add bidirectional edge. *)
      let add_e i j = 
        let vi = add_v i in
        let vj = add_v j in
        let _ = G.add_edge g vi vj in
        let _ = G.add_edge g vj vi in
        ()
      in
      (* Add edges from definitions of vars. used in exp to target_i *)
      let add_use_edges target_i exp =
        let get_def defs v = 
          try Some(Hsh.find defs v)
          with Not_found ->
            (if (is_input (Var.name v)) then None
            else failwith "get_def: can't find a definition"
            )
        in
        let eset = vars_used_in exp in
        let f v = 
          match get_def defs v with
          | Some(def_idx) -> 
            (* Definitions must come before uses. *)
            let _ = assert (target_i > def_idx) in
            add_e def_idx target_i
          | None -> ()
        in
        let _ = VarSet.iter f eset in
        ()
      in
      (* For each exp take all of its vars and add edges from var definitions
       * to this exp. *)
      let make_edges i exp = 
        match exp with
        | Def(_, e) 
        | Assume(e) ->
          let _ = add_use_edges i e in
          i+1
      in
      let _ = L.fold_left make_edges 0 exps in
      g
    in
    let exp2str = function
      | Def(v, w) -> Pp.ast_stmt_to_string (Ast.Move(v,w,[]))
      | Assume(e) -> Pp.ast_stmt_to_string (Ast.Assert(e, []))
    in
    let dump_graph g = 
      let _ = flush stdout in
      let file = open_out_bin "g.dot" in
      let () = Dot.output_graph file g in
      close_out file
    in
    let flatten_branches branches = 
      let let2def = function
        | Ast.Let(v,w,_) -> Def(v,w)
        | _ -> assert false
      in
      let flat branch = 
        let (lets, exp) = Br.get_assert branch in
        let defs = L.map let2def lets in
        defs @ [Assume(exp)]
      in
      let l = L.map flat branches in
      L.flatten l
    in
    let make_br2idx branches assume_idxs = 
      let br2idx = Hsh.create 256 in
      let pairs = L.combine branches assume_idxs in
      let _ = L.map (fun (br,idx) -> Hsh.replace br2idx br idx) pairs in 
      br2idx
    in
    let get_idx br2idx branch = 
      try Hsh.find br2idx branch 
      with _ ->
        failwith "get_idx"
    in
    let get_exp sigma id = 
      try Hsh.find sigma id 
      with Not_found ->
        failwith "get_exp"
    in
    let to_solver z3 ?(neg=false) exp =
      match exp with
      | Def(v, exp) -> z3#add_binding v exp
      | Assume(exp) -> 
        let exp = 
          if neg then Ast.UnOp(Ty.NOT, exp)
          else exp
        in
        z3#add_constraint exp 
    in
    (* FIXME: change how we control which assert to negate? *)
    let solve_scc ({solver=z3; index=last_idx} as state) sigma neg_idx idxs =
      match neg_idx with
      (* We want to flip a branch. *)
      | Some(assume_idx) -> 
        (* tmr_actual_solver#start; *)
        let idxs = L.filter (fun idx -> last_idx<idx && idx<assume_idx) idxs in
        let _ = L.map (fun idx -> to_solver z3 (get_exp sigma idx)) idxs in
        (* Push to save state. *)
        z3#push;
        let _ = to_solver z3 ~neg:true (get_exp sigma assume_idx) in
        let model = make_model z3 free_vars in
        (* Pop, negated assert is removed. *)
        z3#pop;
        (* Add the original assert. *)
        let _ = to_solver z3 (get_exp sigma assume_idx) in
        flush stdout;
        (* tmr_actual_solver#stop; *)
        model, {solver=z3; index=assume_idx}
      (* Just solve everything, do not negate any branch. *)
      | None ->
        let _ = L.map (fun idx -> to_solver z3 (get_exp sigma idx)) idxs in
        let model = make_model z3 free_vars in
        model, state
    in
    let simple_solve sigma idxs = 
      let state = new_z3_state () in
      let model, _ = solve_scc state sigma None idxs in
      model
    in
    (* If trace isn't satisfiable, we need to find the longest satisfiable 
     * prefix to save time and not generate divergent samples. We do this with
     * binary search. Bail early if the trace is satisfiable. 
     * Return: index of last sat assert. *)
    let find_last_sat_assume sigma assume_idxs =
      let rec aux l r = 
        (* If l>r then we didn't find a SAT assert at all! *)
        if l>r then 
          let _ = assert false in
          -1
        else
          let cur = (l+r)/2 in
          let assume_idx = L.nth assume_idxs cur in
          let prefix = Util.mapn (fun i -> i) assume_idx in
          let model = simple_solve sigma prefix in
          match model with
          | Some _ -> 
            if l=r-1 then L.nth assume_idxs l
            else aux cur r
          | None -> 
            aux l cur
      in
      let n = L.length assume_idxs in
      let last_idx = L.nth assume_idxs (n-1) in
      let prefix = Util.mapn (fun i -> i) last_idx in
      match simple_solve sigma prefix with
      | Some _ -> last_idx
      | None -> aux 0 (n)
    in
    let dfs v g = 
      let rec aux visited stack = 
        match stack with
        | v::tl -> 
          if IntSet.mem v visited then 
            aux visited tl
          else
            let neighbors = G.succ g v in
            let stack = neighbors @ stack in
            let visited = IntSet.add v visited in
            aux visited stack
        | [] -> visited
      in
      let s = aux (IntSet.empty) [v] in
      let l = IntSet.elements s in
      L.sort compare l
    in
    let make_step_and_verify_funs branches = 
      (* Branches -> Def/Assume *)
      let exps = flatten_branches branches in
      (* sigma[i]=def/assume number i. *)
      let sigma = build_sigma exps in
      (* defs[v]=position of definition. assume_idxs=indexes of assume stmts. *)
      let (defs, assume_idxs) = walk sigma exps in
      (* br2idx[branch] = assume index *)
      let br2idx = make_br2idx branches assume_idxs in
      (* Build def-use graph. *)
      let g = build_graph defs exps in
      (* Compute strongly connected components. *)
      let (n_components, f_scc) = Components.scc g in
      let _ = dump_graph g in
      let scc_array = Components.scc_array g in
      let scc_list = Array.to_list scc_array in
      (* Sort vertices in SCCs in increasing order. *)
      let scc_list = L.map (L.sort compare) scc_list in
      (* let print_exp idx = *) 
      (*   let exp = get_exp sigma idx in *)
      (*   D.dprintf "%d %s\n" idx (exp2str exp) *)
      (* in *)
      (* Check various properties we expect to be true. *)
      let verify () = 
        let dump_scc_to_file scc_idx scc =
          let fn = Pr.sprintf "scc_%d.txt" scc_idx in
          let attrs = [Open_text; Open_creat; Open_wronly; Open_trunc] in
          let fh = open_out_gen attrs 0o666 fn in
          let fprintf = Pr.fprintf in
          let f idx = 
            let exp = get_exp sigma idx in
            fprintf fh "%d %s\n" idx (exp2str exp)
          in
          let _ = L.map f scc in
          let _ = fprintf fh "XXX" in
          close_out fh
        in
        let emit_debug_info scc_list =
          let _ = BatList.mapi (fun i scc -> dump_scc_to_file i scc) scc_list in
          let all = L.sort compare (L.concat scc_list) in
          let _ = dump_scc_to_file 666 all in
          ()
        in
        (* Dump SCCs to files. *)
        (* let _ = emit_debug_info scc_list in *)
        (* Sum of SCCs contains all statements. *)
        (* let total_len = L.length exps in *)
        (* let total_len2 = L.fold_left (fun acc l -> acc+L.length l) 0 scc_list in *)
        (* FIXME: investigate why some defs aren't used at all. Bug in dead
         * code elimination? *)
        (* let _ = if total_len <> total_len2 then *) 
        (*   diff_lens total_len total_len2 exps scc_list else () in *)

        (* Verify that ocamlgraph returned correct SCCs. *)
        let check_scc_with_dfs scc = 
          let idx = L.hd scc in
          (* Compute the SCC using DFS and check the result. *)
          let scc2 = dfs idx g in
          assert (scc = scc2)
        in
        let _ = L.map check_scc_with_dfs scc_list in

        (* Check if all SCCs are solvable (we don't negate any branches). *)
        let is_sat scc = 
          let model = simple_solve sigma scc in
          match model with
          | Some _ -> true
          | None -> false
        in
        let emit_unsat (idx, scc) = 
          let _ = D.dprintf "UNSAT SCC: %d, size: %d\n" idx (L.length scc) in
          ()
        in
        let sccs = BatList.mapi (fun i scc -> (i,scc)) scc_list in
        let unsat = L.filter (fun (_,scc) -> not (is_sat scc)) sccs in
        let _ = L.map emit_unsat unsat in
        ()
      in
      (* Finally, the step function. We don't need to carry anything in the
       * accumulator. *)
      let solver_step last_sat_idx z3_state_array branch = 
        let assume_idx = get_idx br2idx branch in
        let scc_idx = f_scc assume_idx in
        let last_scc_idx = f_scc last_sat_idx in
        let accumulator, model = 
          (* This is subtle. If current assert is in the same SCC as the last
           * SAT one, negating it can cause the last SAT assert to change. After
           * all, current assert can cause the contradiction, so negating it
           * might fix the problem. *)
          if (scc_idx <> last_scc_idx) && (assume_idx > last_sat_idx) then 
            z3_state_array, None
          else
            let scc = L.nth scc_list scc_idx in
            let z3_state = z3_state_array.(scc_idx) in
            (* Indexes are filtered inside solve_scc. *)
            let neg_idx = Some(assume_idx) in
            let model, new_z3_state = solve_scc z3_state sigma neg_idx scc in
            z3_state_array.(scc_idx) <- new_z3_state;
            z3_state_array, model
        in
        (* Accumulator, model. *)
        accumulator, model
      in
      (* Prepare z3 state for each SCC. *)
      let prep_z3_array n =
        let state = new_z3_state () in
        let arr = Array.make n state in
        Array.map (fun _ -> new_z3_state ()) arr
      in
      let last_sat_idx = find_last_sat_assume sigma assume_idxs in
      let z3_state_array = prep_z3_array n_components in
      let step = (fun acc branch -> solver_step last_sat_idx acc branch) in

      verify, z3_state_array, step 
    in
    (* New opt: unrelated constraint elimination. We need to drop incremental
     * solving for this :/ *)
    let branches = prefix @ suffix in
    let ssa = check_if_ssa branches in
    let _ = assert ssa in
    (* state array is the accumulator for solver_state. *)
    let verify, state_array, solver_step = make_step_and_verify_funs branches in
    let _ = verify () in
    let models = solver_loop solver_step state_array suffix in
    models
 
end

module MakeBox(Adapter:Tr.FormulaAdapter)
    (Formula:MyFlexibleFormula with type init = Adapter.form_init)
    (Br:Branches.BRANCH_OUT)
    (Box:BOX) = 
struct
  module TS = Tr.MakeTraceSymbolic(Sy.DontEvalSymbLet)(Tr.PredAssignTraces)
              (Adapter)(Formula)
  module Sp = Space.MakeSearchSpace(Br)

  let symbolic_run = TS.symbolic_run 
  let output_formula = TS.output_formula 

  let solve_branches = Box.solve_branches
  let mk_branch = Br.mk_branch 

  let split_by_and_grow_leaf = Sp.split_by_and_grow_leaf 
  let export_graph = Sp.export_graph 
  let replace_unks = Sp.replace_unks 
  let tree_from_leaf = Sp.tree_from_leaf 
end

module BoxZ3 = MakeBox(AdapterForZ3)(FormulaForZ3)(BranchForZ3)(BoxZ3In)
module BoxZ3Opti = MakeBox(AdapterForZ3)(FormulaForZ3)(BranchForZ3)(BoxZ3OptiIn)
module BoxSTP = MakeBox(AdapterForSTP)(FormulaForSTP)(BranchForSTP)(BoxSTPIn)
