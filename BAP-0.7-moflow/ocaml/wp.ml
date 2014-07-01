(** Weakest Precondition.

    These functions calculate the weakest precondition wp(p,q) for
    postcondition q and gcl program p.
*)

open Ast
open Ast_convenience
open Type
open Gcl

module D = Debug.Make(struct let name = "Wp" and default=`Debug end)
open D

module BH = Cfg.BH
module VH = Var.VarHash

let wp ?(simp=Util.id) (p:Gcl.t) (q:exp) : exp =
  (*  We use CPS to avoid stack overflows *)
  let rec wp q s k =
    match s with
    | Skip -> k q
    | Assume e ->
	k (simp(exp_implies e q))
    | Choice(s1, s2) ->
	wp q s1 (fun x -> wp q s2 (fun y -> k(simp(exp_and x y ))))
    | Seq(s1, s2) ->
	wp q s2 (fun x -> wp x s1 k)
    | Assign(t, e) ->
	k(simp(Let(t, e, q)))
    | Assert e ->
	k (simp(exp_and e q))
  in
  wp q p  (fun x->x)

let passified_wp ?(simp=Util.id) (p:Gcl.t) (q:exp) : exp =
  (*  We use CPS to avoid stack overflows *)
  let rec wp q s k =
    match s with
    | Skip -> k q
    | Assume e ->
      k (simp(exp_implies e q))
    | Choice(s1, s2) ->
      let v = Var.newvar "q" (Reg 1) in
      let qe = Var v in
      wp qe s1 (fun x -> wp qe s2 (fun y -> k(simp(Let(v,q,exp_and x y)))))
    | Seq(s1, s2) ->
      wp q s2 (fun x -> wp x s1 k)
    | Assign(t, e) ->
      failwith "Expected a passified program"
    | Assert e ->
      k (simp(exp_and e q))
  in
  wp q p  (fun x->x)

module CA = Cfg.AST

module RevCFG =
struct
  type t = CA.G.t
  module V = CA.G.V
  let iter_vertex = CA.G.iter_vertex
  let iter_succ = CA.G.iter_pred
  let in_degree = CA.G.out_degree
end

module Toposort = Graph.Topological.Make(Cfg.AST.G)
module RToposort = Graph.Topological.Make(RevCFG);;

let build_uwp wp ((cfg,ugclmap):Gcl.Ugcl.t) (q:exp) : exp =
  Checks.acyclic_astcfg cfg "UWP";
  (* Block -> exp *)
  let wpvar = BH.create (CA.G.nb_vertex cfg) in
  let lookupwpvar = BH.find wpvar in
  let setwp bbid q =
    assert (not (BH.mem wpvar bbid));
    BH.add wpvar bbid q;
  in
  let rec compute_at bb =
    (* Find the weakest precondition from the program exit to our
       successors *)
    let bbid = CA.G.V.label bb in
    let q_in = match CA.G.succ cfg bb with
      | [] when bbid = Cfg.BB_Exit ->
        q (* The user supplied q *)
      | [] when bbid = Cfg.BB_Error ->
        exp_true (* The default postcondition is exp_true.  Note that
                    BB_Error contains an assert false, so this doesn't really
                    matter. *)
      | [] -> failwith (Printf.sprintf "BB %s has no successors but should" (Cfg_ast.v2s bb))
      | s -> (* Get the conjunction of our successors' preconditions *)
        let get_wp bb = lookupwpvar (CA.G.V.label bb) in
        (* Note: This duplicates the postcondition *)
        BatList.reduce (fun acc e -> binop AND acc e) (List.map get_wp s)
    in
    let q_out = wp (ugclmap bbid) q_in in
    setwp bbid q_out
  in
  RToposort.iter compute_at cfg;
  lookupwpvar Cfg.BB_Entry

let dijkstra_uwp = build_uwp wp

let build_passified_uwp iter wp ((cfg,ugclmap):Gcl.Ugcl.t) (q:exp) : exp =
  Checks.acyclic_astcfg cfg "UWP";
  (* dprintf "Starting uwp"; *)
  (* Block -> var *)
  let wpvar = BH.create (CA.G.nb_vertex cfg) in
  (* Var -> exp *)
  let varexp = VH.create (CA.G.nb_vertex cfg) in
  let lookupwpvar bbid =
    let makevar bbid =
      assert (not (BH.mem wpvar bbid));
      let v = Var.newvar (Printf.sprintf "q_pre_%s" (Cfg.bbid_to_string bbid)) reg_1 in
      BH.add wpvar bbid v;
      v
    in
    try BH.find wpvar bbid
    with Not_found -> makevar bbid
  in
  let setwp bbid q =
    let v = lookupwpvar bbid in
    VH.add varexp v q
  in
  let rec compute_at bb =
    (* Find the weakest precondition from the program exit to our
       successors *)
    let bbid = CA.G.V.label bb in
    let q_in = match CA.G.succ cfg bb with
      | [] when bbid = Cfg.BB_Exit ->
        q (* The user supplied q *)
      | [] when bbid = Cfg.BB_Error ->
        exp_true (* The default postcondition is exp_true.  Note that
                    BB_Error contains an assert false, so this doesn't really
                    matter. *)
      | [] -> failwith (Printf.sprintf "BB %s has no successors but should" (Cfg_ast.v2s bb))
      | s -> (* Get the conjunction of our successors' preconditions *)
        let get_wp bb = Var(lookupwpvar (CA.G.V.label bb)) in
        BatList.reduce (fun acc e -> binop AND acc e) (List.map get_wp s)
    in
    let q_out = wp (ugclmap bbid) q_in in
    setwp bbid q_out
  in
  iter compute_at cfg;
(* Now we have a precondition for every block in terms of
   postcondition variables of its successors. We just need to visit in
   topological order and build up the whole expression now. *)
  let build_assigns bb acc =
    let bbid = CA.G.V.label bb in
    let v = lookupwpvar bbid in
    (v, VH.find varexp v)::acc
  in
  let assigns = RToposort.fold build_assigns cfg [] in
  let build_exp bige (v,e) =
    Let(v, e, bige)
  in
  (* FIXME: We shouldn't use entry here *)
  List.fold_left build_exp (Var(lookupwpvar Cfg.BB_Entry)) assigns

let efficient_uwp = build_passified_uwp RToposort.iter wp

let efficient_wp ?(simp=Util.id) (p:Gcl.t) =
  let wlp_f_ctx = Hashtbl.create 113 in
  let rec wlp_f s k =
    try k (Hashtbl.find wlp_f_ctx s)
    with Not_found ->
      let remember q = Hashtbl.add wlp_f_ctx s q; k q in
      match s with
      | Skip ->
	  remember exp_false
      | Assume e
      | Assert e ->
	  remember (exp_not e)
      | Choice(s1, s2) ->
	  wlp_f s1 (fun q1 -> wlp_f s2
		      (fun q2 ->
			 let q = simp(exp_and q1 q2 ) in
			 remember q
		      ))
      | Seq(s1, s2) ->
	  wlp_f s1 (fun q1 -> wlp_f s2
		      (fun q2 ->
			 let q = simp(exp_or q1 q2 ) in
			 remember q
		      ))
      | Assign _ ->
	  raise (Invalid_argument("efficient_wp requires an assignment-free program"))
  in
  let rec wp_t s k : exp =
    match s with
    | Skip
    | Assume _ ->
	k exp_true
    | Assert e ->
	k e
    | Choice(s1,s2) ->
	wp_t s1 (fun q1 -> wp_t s2 (fun q2 -> k(simp(exp_and q1 q2))))
(*	  (* by the book^Wthesis *)
    | Seq(s1, s2) ->
	wp_t s2 (fun q1 -> wp_t s1
		   (fun q2 -> wlp_f s1
		      (fun q3 -> k(simp(exp_and q1 (exp_or q2 q3)))) ))
*)
	  (* by my own derivation *)
    | Seq(s1, s2) ->
	wp_t s1 (fun q1 -> wp_t s2
		   (fun q2 -> wlp_f s1
		      (fun q3 -> k(simp(exp_and q1 (exp_or q2 q3)))) ))
    | Assign _ ->
	invalid_arg "efficient_wp requires an assignment-free program"
  in
  let q0 = wlp_f p (fun x -> x) in
  let qpr = wp_t p (fun x -> x) in
  (fun q -> simp(exp_and qpr (exp_or q0 q)))

let dwp_name = "dwptemp"
let dwp_name = "t"

let ast_size e =
  let s = ref 0 in
  let vis = object
    inherit Ast_visitor.nop
    method visit_exp _ =
      incr s;
      DoChildren
  end in
  ignore(Ast_visitor.exp_accept vis e);
  !s

(* helper for dwp *)
let variableify ?(name=dwp_name) k v e =
    if ast_size e > k then
      let x = Var.newvar name (Typecheck.infer_ast e) in
      let xe = Var x in
      ((x, e) :: v, xe)
    else
      (v, e)

let rm_useless_vars vs n w =
  (* FIXME: remove vars that are only referenced once *)
  let l = List.length vs in
  let h = VH.create l
  and c = VH.create l in
  List.iter (fun (v,e) -> VH.add h v e; VH.add c v (ref 0)) vs;
  let inc v = try incr (VH.find c v) with Not_found -> () in
  let counter =
    object(self)
      inherit Ast_visitor.nop
	(* FIXME: worry about Let? *)
      method visit_rvar r =
	inc r;
	DoChildren
    end
  in
  ignore(Ast_visitor.exp_accept counter n);
  ignore(Ast_visitor.exp_accept counter w);
  List.iter (fun(v,e)-> ignore(Ast_visitor.exp_accept counter e)) vs;
  let to_remove v = try !(VH.find c v) <= 1 with Not_found -> false in
  let vs = List.filter (fun p -> not(to_remove (fst p))) vs in
  let subst =
    object(self)
      inherit Ast_visitor.nop
      method visit_exp = function
	| Var v when to_remove v ->
	    ChangeToAndDoChildren(VH.find h v)
	| _ -> DoChildren
    end
  in
  let n = Ast_visitor.exp_accept subst n
  and w = Ast_visitor.exp_accept subst w
  and vs = List.map (fun (v,e) -> (v, Ast_visitor.exp_accept subst e)) vs in
  (vs,n,w)



let assignments_to_exp = function
  | [] -> exp_true
  | v::vs ->
      let p2e (v,e) = BinOp(EQ, Var v, e) in
      let rec h e = function
	| a::rest ->
	    if true then h (exp_and e (p2e a)) rest
	    else h (exp_and (p2e a) e) rest
	| [] -> e
      in
      h (p2e v) vs
let assignments_to_exp_opt = function
  | [] -> None
  | e -> Some(assignments_to_exp e)

let assignments_to_lets = function
  | [] -> (fun e -> e)
  | vars ->
    (fun e -> List.fold_left (fun exp (v,e) -> Let(v,e,exp)) e vars)

(** [dwp_g] is an implementation of [G] from the DWP paper.

   @arg f A function that maps a GCL statement to the [V], [N], and [W]
   lists from the DWP algorithm.
   @arg p The GCL statement/program to run DWP on.
*)

let dwp_g ?(simp=Util.id) ?(k=1) f (p:Gcl.t) =
  let g (v, n, w) =
    let rec g' v ns ws fn fw =
      match (ns,ws) with
      | (n::ns, w::ws) ->
	let (v,n) = variableify k v n in
	g' v ns ws (exp_and n fn) (exp_or w (exp_and n fw))
      | ([],[]) ->
	(v, fn, fw)
      | _ -> failwith "n and w are supposed to have the same length"
    in
    match (n,w) with
    | (n::ns, w::ws) -> let (v,n) = variableify k v n in g' v ns ws n w
    | ([],[]) -> (v, exp_true, exp_false)
    | _ -> failwith "n and w are supposed to have the same length"
  in
  let (vs,n,w) = g (f g ([],[],[]) p) in
  (* Surprisingly enough, this is a toss up. It makes some formulas
     much easier for CVC and others much harder. *)
  (* let (vs,n,w) = rm_useless_vars vs n w in *)
  (assignments_to_exp_opt vs, vs, n, w)

(** [f] from the DWP paper. *)
let dwp_f ?(less_duplication=true) ?(k=1) g =
  let rec f ((v,n,w) as vnw) s = match s with
    | Assert e ->
      let (v,e) = if less_duplication then variableify k v e else (v,e) in
      (v, e::n, exp_not e :: w)
    | Assume e ->
      (v, e::n, exp_false::w)
    | Seq(a, b) ->
      let vnw' = f vnw a in (* FIXME: do we need tail recursion?*)
      f vnw' b
    | Choice(a, b) ->
      let (v,na,wa) = f (v,[],[]) a in
      let (v,nb,wb) = f (v,[],[]) b in
      let (v,na,wa) = g (v,na,wa) in
      let (v,nb,wb) = g (v,nb,wb) in
      (v, (exp_or na nb)::n, (exp_or wa wb)::w)
    | Skip ->
      vnw
    | Assign _ ->
      invalid_arg "dwp requires an assignment-free program"
  in
  f

(** Generates a 1st order logic VC using the DWP algorithm. *)
let dwp_1st ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let f' = dwp_f ~less_duplication ~k in
  let (vo, vs, n, w) = dwp_g ~simp ~k f' p in
  match vo with
  | Some v ->
      let vars = List.map fst vs in
      (fun q -> (vars, exp_implies v (exp_and (exp_not w) (exp_implies n q))))
  | None ->
      (fun q -> ([], exp_and (exp_not w) (exp_implies n q)))

let dwp_pred_help ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let f' = dwp_f ~less_duplication ~k in
  dwp_g ~simp ~k f' p

(** Generates a predicate logic VC using the DWP algorithm. *)
let dwp ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (mode:formula_mode) (p:Gcl.t) =
  let (vo, _, n, w) = dwp_pred_help ~simp ~less_duplication ~k p in
  match vo, mode with
  | Some v, Sat ->
    (fun q -> (exp_and v (exp_and (exp_not w) (exp_implies n q))))
  | Some v, Validity ->
    (fun q -> (exp_implies v (exp_and (exp_not w) (exp_implies n q))))
  | None, Sat ->
    (fun q -> (exp_and (exp_not w) (exp_implies n q)))
  | None, Validity ->
    (fun q -> (exp_and (exp_not w) (exp_implies n q)))
  | _, Foralls ->
    failwith "Foralls not supported in predicate DWP"


let dwp_let ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (mode:formula_mode) (p:Gcl.t) =
  let (_, vars, n, w) = dwp_pred_help ~simp ~less_duplication ~k p in
  (fun q ->
    let exp = match mode with
      | Sat -> exp_and (exp_not w) (exp_and n q)
      | Validity -> exp_and (exp_not w) (exp_implies n q)
      | Foralls -> failwith "Foralls not supported in predicate DWP"
    in
    assignments_to_lets vars exp
  )

(*let flanagansaxe_dumb ?(simp=Util.id) (p:Gcl.t) =
  let rec n = function
    | Assert e -> e
    | Assume e -> e
    | Seq(a,b) -> exp_and (n a) (n b)
    | Choice(a,b) -> exp_or (n a) (n b)
  in
  let rec w = function
    | Assert e -> exp_not e
    | Assume e -> exp_false
    | Seq(a,b) -> exp_or (w a) (exp_and (n a) (w b))
    | Choice(a,b) -> exp_or (w a) (w b)
  in
  let ws = w p and ns = n p in
  (fun q ->
     exp_and (exp_not ws) (exp_implies ns q)
  )
*)

let flanagansaxe ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (mode:formula_mode) (p:Gcl.t) =
  let rec nw v = function
    | Assume e -> (e, exp_false, v)
    | Assert e ->
	let (v,e) = if less_duplication then variableify k v e else (v,e) in
	(e, exp_not e, v)
    | Choice(a,b) ->
	let (na,wa,v) = nw v a in
	let (nb,wb,v) = nw v b in
	(exp_or na nb, exp_or wa wb, v)
    | Seq(a,b) ->
	let (na,wa,v) = nw v a in
	let (v,na) = variableify k v na in
	let (nb,wb,v) = nw v b in
	(exp_and na nb, exp_or wa (exp_and na wb), v)
    | Assign _ -> failwith "gcl should be passified"
    | Skip -> (exp_true, exp_false, v)
  in
  let (ns,ws,v) = nw [] p in
  match assignments_to_exp v, mode with
  | v, Sat ->
    (fun q -> exp_and v (exp_and (exp_not ws) (exp_implies ns q)) )
  | v, Validity ->
    (fun q -> exp_implies v (exp_and (exp_not ws) (exp_implies ns q)) )
  | _, Foralls ->
    failwith "Foralls not supported in predicate FS"

