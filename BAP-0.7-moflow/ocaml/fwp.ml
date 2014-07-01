open Ast
open Ast_convenience
module D = Debug.Make(struct let name = "Fwp" and default=`NoDebug end)
open D
open Gcl
open Symbeval
open Type
module VH = Var.VarHash
module VM = Var.VarMap

module CA=Cfg.AST

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

module SymbolicMap = Symbeval.SymbolicSlowMap

let unwrap_symb = function
  | Symbeval.Symbolic e -> e
  | Symbeval.ConcreteMem(m,v) -> Symbeval.symb_to_exp (Symbeval.conc2symb m v)

(* Use the simplified duplicate expression if it is a constant,
   otherwise use the duplicated one. *)
let choose_best e edup =
  match edup with
  | Int _ -> edup
  | _ -> e

module type Delta =
sig
  type t
  val create : unit -> t
  val merge : t -> t -> t
  (** Take the intersection of two deltas. When there are conflicting
      bindings for a variable, there will be no binding in the final
      delta for that variable. *)
  val set : t -> var -> Symbeval.varval -> t
  (** Setter method *)
  val get : t -> var -> Symbeval.varval
  (** Getter method.  Raises [Not_found] exception if variable is
      not set. *)
  val set_exp : t -> var -> exp -> t
  val get_exp : t -> var -> exp
  val simplify : t -> exp -> Symbeval.varval
  (** [simplify d e] simplifies exp in context d. *)
end

module VMDelta =
struct
  type t = Symbeval.varval VM.t
  let create () =
    VM.empty
  let merge (d1:t) (d2:t) =
    let f var x y = match x, y with
      | Some a, Some b when a === b -> x
      | _, _ -> dprintf "Merge conflict: %s" (Pp.var_to_string var); None
    in
    VM.merge f d1 d2
  let set h v e =
    dprintf "Setting %s to %s" (Pp.var_to_string v) (Symbeval.symb_to_string e);
    VM.add v e h
  let get h v =
    VM.find v h
  let get_exp h v =
    unwrap_symb (get h v)
  let set_exp h v e =
    set h v (Symbeval.Symbolic e)
  let simplify d e =
    (* Reduce to constant if possible *)
    SymbolicMap.eval_expr d e
end

(* This helps keeps ms syntactically equal to true *)
let or_simp = function
  | BinOp(OR, e1, e2) when e1 === exp_true || e2 === exp_true -> exp_true
  | BinOp(OR, e1, e2) when e1 === exp_false -> e2
  | BinOp(OR, e1, e2) when e2 === exp_false -> e1
  | BinOp(OR, e1, UnOp(NOT, e2))
  (* Note: this is == on purpose to be fast *)
  | BinOp(OR, UnOp(NOT, e1), e2) when e1 == e2 -> exp_true
  | BinOp(OR, e1, e2) when e1 == e2 -> e1
  | BinOp(AND, e1, e2) when e1 === exp_true -> e2
  | BinOp(AND, e1, e2) when e2 === exp_true -> e1
  | BinOp(AND, e1, e2) when e1 === exp_false || e2 === exp_false -> exp_false
  | BinOp(AND, e1, UnOp(NOT, e2))
  (* Note: this is == on purpose to be fast *)
  | BinOp(AND, UnOp(NOT, e1), e2) when e1 == e2 -> exp_false
  | BinOp(AND, e1, e2) when e1 == e2 -> e1
  | e -> e

(* Fwp base implementation.  Returns a tuple consisting of variable
   bindings, de-duplicated ms, de-duplicated af, duplicated ms,
   duplicated af.  We need to keep both duplicated and de-duplicated
   versions of expressions for simplification.  For instance, we
   cannot infer e \/ \lnot e = true if if is written as v=e /\ (v \/
   \lnot e).  Yet, simplifications are very important because for Sat
   queries MS should simplify to true!

   Memory is not a concern for the duplicated expressions because of
   memory sharing.
*)
let rec fwpint ?(simp=or_simp) ?(k=1) ?assign_mode p =
  let fwp = fwpint ~simp ~k ?assign_mode in match p with
    | Assign (v,e) when assign_mode <> None ->
      (match assign_mode with
      | Some Sat -> fwp (Assert (exp_eq (Var v) e))
      | Some Validity -> fwp (Assume (exp_eq (Var v) e))
      | Some Foralls -> failwith "fwp: Foralls not implemented"
      | None -> failwith "fwp: impossible")
    | Assign _ -> failwith "fwp requires an assignment free program"
    | Assert e -> let ne = exp_not e in [], exp_true, ne, exp_true, ne
    | Assume e -> [], e, exp_false, e, exp_false
    | Choice (s1, s2) ->
      let v1, ms1, af1, msdup1, afdup1 = fwp s1 in
      let v2, ms2, af2, msdup2, afdup2 = fwp s2 in

      let ms = simp (exp_or ms1 ms2) in
      let msdup = simp (exp_or msdup1 msdup2) in
      let af = simp (exp_or af1 af2) in
      let afdup = simp (exp_or afdup1 afdup2) in

      v1@v2, choose_best ms msdup, choose_best af afdup, msdup, afdup
    | Seq (s1, s2) as _s ->
      let v1, ms1, af1, msdup1, afdup1 = fwp s1 in
      let v2, ms2, af2, msdup2, afdup2 = fwp s2 in
      let v = [] in
      let (v,ms1) = Wp.variableify ~name:"fwp_seq_ms1" k v ms1 in
      let (v,af1) = Wp.variableify ~name:"fwp_seq_af1" k v af1 in

      let ms = simp (exp_and ms1 (simp (exp_or af1 ms2))) in
      let msdup = simp (exp_and msdup1 (simp (exp_or afdup1 msdup2))) in
      let af = simp (exp_and ms1 (simp (exp_or af1 af2))) in
      let afdup = simp (exp_and msdup1 (simp (exp_or afdup1 afdup2))) in

      v@v1@v2, choose_best ms msdup, choose_best af afdup, msdup, afdup
  | Skip -> [], exp_true, exp_false, exp_true, exp_false

module Make(D:Delta) = struct

(* Internal function for fwp lazy concrete eval *)
  let mark_used needh v =
    VH.replace needh v true
  let mark_used_vars_in needh e =
    List.iter (mark_used needh) (Formulap.freevars e)
  let trans_close needh vmaph =
    let h = VH.create (VH.length needh) in
    let rec t = function
      | [] -> ()
      | x::tl ->
        if VH.mem h x then t tl
        else (mark_used needh x;
              VH.add h x ();
              t (tl@VH.find_all vmaph x))
    in
    let module HU = Util.HashUtil(VH) in
    t (HU.get_hash_keys needh)
  (* Returns delta, a lazy computation of v, ms, af, msdup, afdup,
     and fallthrough, which indicates whether execution may pass to a
     following statement.  Fallthrough is false after assert false or
     assume false. *)
  let rec fwpconcint simp eval k needh vmaph mode delta =
    let is_needed v =
      try VH.find needh v
      with Not_found -> false
    (* Mark v as being needed in the formula *)
    in
    let punt delta s = let v, ms, af, msdup, afdup = fwpint ~simp ~k ~assign_mode:mode s
                       in
                       let e = match s with
                         | Gcl.Assign (_, e) -> Some e
                         | Gcl.Assume e -> Some e
                         | Gcl.Assert e -> Some e
                         | Gcl.Skip -> None
                         | _ -> failwith "Not allowed to punt for complex statements"
                       in
                       BatOption.may (mark_used_vars_in needh) e;
                       delta, v, ms, af, msdup, afdup
    in
    let punt_internal delta s = let _, v, ms, af, msdup, afdup = punt delta s in
                                (v, ms, af, msdup, afdup)
    in
    let punt_external ?(fallthrough=true) delta s = let delta, v, ms, af, msdup, afdup = punt delta s in
                                delta, lazy (v, ms, af, msdup, afdup), fallthrough
    in
    let fwpconc = fwpconcint simp eval k needh vmaph mode in
    function
      | Gcl.Assign (v, e) as s ->
        let value = eval delta e in
        if Symbeval.is_concrete_mem_or_scalar value then
          let e' = match value with
            | Symbolic _ -> unwrap_symb value
            | ConcreteMem _ -> e
          in
          List.iter (fun v' -> VH.add vmaph v v') (Formulap.freevars e');
          let delta = D.set delta v value in
          delta, lazy (
            if is_needed v
            (* If needed, put the assignment in the formula.  For
               memories, use the expression, since the conrete values can
               be huge.  For scalars, use the concrete value. *)
            then punt_internal delta (Gcl.Assign (v, e'))
            (* If the value is not needed in the formula, act like a Skip *)
            else punt_internal delta Gcl.Skip
          ), true
        else (List.iter (fun v' -> VH.add vmaph v v') (Formulap.freevars e);
              punt_external delta s)
      | Gcl.Assume e as s ->
        let value = eval delta e in
        if value = Symbolic exp_true then punt_external delta Gcl.Skip
        else if value = Symbolic exp_false then delta, lazy([], exp_false, exp_false, exp_false, exp_false), false
        else punt_external delta s
      | Gcl.Assert e as s ->
        let value = eval delta e in
        if value = Symbolic exp_true then punt_external delta Skip
        else if value = Symbolic exp_false then delta, lazy([], exp_true, exp_true, exp_true, exp_true), false
        else punt_external delta s
      | Gcl.Choice (s1, s2) ->
        let delta1, lazy1, fallthrough1 = fwpconc delta s1 in
        let delta2, lazy2, fallthrough2 = fwpconc delta s2 in
        let deltamerge = match fallthrough1, fallthrough2 with
          | true, true -> D.merge delta1 delta2
          | true, false -> delta1
          | false, true -> delta2
          | false, false -> D.create ()
        in

        deltamerge, lazy (
          let v1, ms1, af1, msdup1, afdup1 = Lazy.force lazy1 in
          let v2, ms2, af2, msdup2, afdup2 = Lazy.force lazy2 in

          let ms = simp (exp_or ms1 ms2) in
          let msdup = simp (exp_or msdup1 msdup2) in
          let af = simp (exp_or af1 af2) in
          let afdup = simp (exp_or afdup1 afdup2) in

          v1@v2, choose_best ms msdup, choose_best af afdup, msdup, afdup), fallthrough1 || fallthrough2
      | Gcl.Seq (s1, s2) as _s ->
        let delta1, lazy1, fallthrough1 = fwpconc delta s1 in
        let delta2, lazy2, fallthrough2 = fwpconc delta1 s2 in
        delta2, lazy (
          let v1, ms1, af1, msdup1, afdup1 = Lazy.force lazy1 in
          if msdup1 = exp_false then v1, exp_false, exp_false, exp_false, exp_false
          else if afdup1 = exp_true then v1, ms1, ms1, msdup1, msdup1
          else
            let v2, ms2, af2, msdup2, afdup2 = Lazy.force lazy2 in
            let v = [] in
            let (v,ms1) = Wp.variableify ~name:"fwp_seq_ms1" k v ms1 in
            let (v,af1) = Wp.variableify ~name:"fwp_seq_af1" k v af1 in

            let ms = simp (exp_and ms1 (simp (exp_or af1 ms2))) in
            let msdup = simp (exp_and msdup1 (simp (exp_or afdup1 msdup2))) in
            let af = simp (exp_and ms1 (simp (exp_or af1 af2))) in
            let afdup = simp (exp_and msdup1 (simp (exp_or afdup1 afdup2))) in

            v@v1@v2, choose_best ms msdup, choose_best af afdup, msdup, afdup), fallthrough1 && fallthrough2
      | Gcl.Skip as s -> punt_external delta s

(* Ed's FWP formulation + concrete evaluation + lazy merging.

   This function concrete evaluates the entire program eagerly, but
   produces lazy expressions for the formulas.  We do this so that we
   know what variables need to be placed in the formula at merge
   points.  For instance, in the program [Choice(Assign (x, 5), Skip)]
   we do not need to put x in the formula, because there is no
   reference to [x] in the formula. However, [Seq(Choice(Assign (x,
   5), Skip), Assert (x == 5))] would need to put [x] in the formula.
   The difficulty is that FWP is going forward, but we don't know if a
   variable is used until later in the program.  Thus the lazy
   expressions.
*)
let fwp_lazyconc ?(simp=or_simp) ?(k=1) ?(cf=true) (mode:formula_mode) (p:Gcl.t) q =
  (*
    Returns (v, fwpms P, fwpaf P).

    Note: fwpms P = Not (wp P true) \/ Not (wlp P false)
    and fwpaf P = Not (wp P true) *)
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  (* var -> is var needed? *)
  let needh = VH.create 1000 in
  (* var -> vars referenced by var *)
  let vmaph = VH.create 1000 in
  let (delta, lazyr, _) = fwpconcint simp eval k needh vmaph mode (D.create ()) p in
  let q' =
    let value = eval delta q in
    unwrap_symb value
  in
  mark_used_vars_in needh q';
  (* Compute transitive closure over used variables *)
  dprintf "Computing transitive closure..";
  trans_close needh vmaph;
  dprintf "done.";
  let v, ms, af, _, _ = Lazy.force lazyr in
  if mode = Sat then assert (ms === exp_true);
  match mode with
  | Sat | Validity ->
    Wp.assignments_to_lets v (exp_implies ms (exp_and (exp_not af) q'))
  | Foralls ->
    failwith "Foralls not supported yet"

let fwp_lazyconc_uwp ?(simp=or_simp) ?(k=1) ?(cf=true) mode ((cfg,ugclmap):Gcl.Ugcl.t) (q:exp) : exp =

  Checks.acyclic_astcfg cfg "UWP";
  Checks.connected_astcfg cfg "UWP";
  Checks.exit_check_astcfg ~allowed_exits:[Cfg.BB_Exit; Cfg.BB_Error] ~expected_exits:[Cfg.BB_Exit] cfg "UWP";

  (* We want executions that go to BB_Error to return false in the VC,
     so we must add an edge from Error to Exit. *)
  let cfg =
    if CA.G.mem_vertex cfg (CA.G.V.create Cfg.BB_Error) then
      CA.add_edge cfg (CA.G.V.create Cfg.BB_Error) (CA.G.V.create Cfg.BB_Exit)
    else cfg
  in

  (* dprintf "Starting uwp"; *)
  (* Block -> var *)
  let module BH = Cfg.BH in
  let eval delta e = if cf
    then D.simplify delta e
    else Symbeval.Symbolic e
  in
  (* var -> is var needed? *)
  let needh = VH.create 1000 in
  (* var -> vars referenced by var *)
  let vmaph = VH.create 1000 in
  (* bb mapped to delta, ms, and af variables, msdup, afdup *)
  let wpvar = BH.create (CA.G.nb_vertex cfg) in
  (* maps from BB to ms, af variables *)
  let bbinfo = BH.create (CA.G.nb_vertex cfg) in
  (* maps from BB to delta, lazy(ms, af values, and dups) *)
  let lookupwpvar bbid =
    let makevar bbid =
      assert (not (BH.mem wpvar bbid));
      let v = Var.newvar (Printf.sprintf "ms_%s" (Cfg.bbid_to_string bbid)) reg_1 in
      let v2 = Var.newvar (Printf.sprintf "af_%s" (Cfg.bbid_to_string bbid)) reg_1 in
      BH.add wpvar bbid (v, v2);
      v, v2
    in
    try BH.find wpvar bbid
    with Not_found -> makevar bbid
  in
  let setbbinfo bbid i =
    BH.add bbinfo bbid i
  in
  let getbbinfo bbid =
    BH.find bbinfo bbid
  in
  let rec compute_at bb =
    (* Find the weakest precondition from the program exit to our
       successors *)
    let bbid = CA.G.V.label bb in
    let delta_in, fallthrough1 = match CA.G.pred cfg bb with
      | [] when bbid = Cfg.BB_Entry ->
        D.create (), true
      | [] -> failwith (Printf.sprintf "BB %s has no predecessors but should" (Cfg_ast.v2s bb))
      | s -> let get_delta bb =
               let delta,_,fallthrough = getbbinfo (CA.G.V.label bb) in
               dprintf "pred bb %s fallthrough %b" (Cfg_ast.v2s bb) fallthrough;
               if fallthrough then Some delta else None
             in
             dprintf "merging at %s" (Cfg_ast.v2s bb);
             let fallthrough_deltas = BatList.filter_map get_delta s in
             let merged_delta = try BatList.reduce D.merge fallthrough_deltas
               with Invalid_argument "Empty List" -> D.create ()
             in merged_delta, fallthrough_deltas <> []
    in
    let (delta, lazyr, fallthrough2) = fwpconcint simp eval k needh vmaph mode delta_in (ugclmap (CA.G.V.label bb)) in
    let (delta, lazyr, fallthrough) =
      delta, lazy (
        let ms1, af1, msdup1, afdup1 = match CA.G.pred cfg bb with
          | [] when bbid = Cfg.BB_Entry ->
            exp_true, exp_false, exp_true, exp_false
          | [] -> failwith (Printf.sprintf "BB %s has no predecessors but should" (Cfg_ast.v2s bb))
          | s ->
            (* Get the conjunction of our predecessors' post-conditions.

               This is basically fwp's Choose rule. *)
            let bbinfo bb =
              let _,l,_ = getbbinfo (CA.G.V.label bb) in
              let _,_,_,msdup,afdup = Lazy.force l in
              let ms,af = lookupwpvar (CA.G.V.label bb) in
              Var ms,Var af,msdup,afdup
            in
            BatList.reduce (fun (ms1,af1,msdup1,afdup1) (ms2,af2,msdup2,afdup2) ->

              let ms = simp (exp_or ms1 ms2) in
              let msdup = simp (exp_or msdup1 msdup2) in
              let af = simp (exp_or af1 af2) in
              let afdup = simp (exp_or afdup1 afdup2) in

              choose_best ms msdup, choose_best af afdup, msdup, afdup) (List.map bbinfo s)
        in

        (* This is basically fwp's Sequence rule. *)
        if msdup1 = exp_false then [], exp_false, exp_false, exp_false, exp_false
        else if afdup1 = exp_true then [], ms1, ms1, msdup1, msdup1
        else if fallthrough1 = false then [], ms1, af1, msdup1, afdup1
        else
          let v, ms2, af2, msdup2, afdup2 = Lazy.force lazyr in
          (* let (v,ms1) = Wp.variableify ~name:"fwp_seq_ms1" k v ms1 in *)
          (* let (v,af1) = Wp.variableify ~name:"fwp_seq_af1" k v af1 in *)

          let ms = simp (exp_and ms1 (simp (exp_or af1 ms2))) in
          let msdup = simp (exp_and msdup1 (simp (exp_or afdup1 msdup2))) in
          let af = simp (exp_and ms1 (simp (exp_or af1 af2))) in
          let afdup = simp (exp_and msdup1 (simp (exp_or afdup1 afdup2))) in

          v, choose_best ms msdup, choose_best af afdup, msdup, afdup
    ), fallthrough1 && fallthrough2
    in
    setbbinfo bbid (delta, lazyr, fallthrough)
  in
  Toposort.iter compute_at cfg;

  let q' =
    let delta,_,_ = getbbinfo Cfg.BB_Exit in
    let value = eval delta q in
    unwrap_symb value
  in
  dprintf "evaluated q: %s" (Pp.ast_exp_to_string q');
  mark_used_vars_in needh q';

  (* Now we have a precondition for every block in terms of
     postcondition variables of its successors. We just need to visit in
     topological order and build up the whole expression now. *)
  trans_close needh vmaph;
  let build_assigns bb acc =
    let bbid = CA.G.V.label bb in
    let msv, afv = lookupwpvar bbid in
    let _, lazywp, _ = getbbinfo bbid in
    let v, msvalue, afvalue, _, _ = Lazy.force lazywp in
    BatList.append ((msv, msvalue)::(afv, afvalue)::v) acc
  in
  let assigns = Toposort.fold build_assigns cfg [] in
  (* FIXME: We shouldn't hardcode exit here *)
  let msv, afv = lookupwpvar Cfg.BB_Exit in
  let ms, af = Var msv, Var afv in
  let wp = exp_implies ms (exp_and (exp_not af) q') in
  match mode with
  | Sat | Validity ->
    Wp.assignments_to_lets assigns wp
  | Foralls ->
    failwith "Foralls not supported yet"
end

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

(* Ed's FWP formulation.  If there are no Assumes, fwpms will always
   be true, and fwp degenerates to efficient (merging) fse. *)
let fwp ?(normalusage=true) ?(simp=or_simp) ?(k=1) (mode:formula_mode) (p:Gcl.t) q =
  (*
    Returns (v, fwpms P, fwpaf P).

    Note: fwpms P = Not (wp P true) \/ Not (wlp P false)
      and fwpaf P = Not (wp P true) *)
  dprintf "GCL size: %d" (Gcl.size p);
  let (v,ms,af,_,_) = fwpint ~simp ~k p in
  (* If p is an entire program lifted in Sat mode, ms should be true.
     However, when we use fwp from Uwp, this may not be true, since
     p is a small part of the whole program. *)
  if normalusage && mode = Sat then assert (ms === exp_true);
  let o = match mode with
    | Sat | Validity ->
      Wp.assignments_to_lets v (exp_implies ms (exp_and (exp_not af) q))
    | Foralls ->
      failwith "Foralls not supported yet"
  in
  dprintf "WP size: %d" (ast_size o); o

let fwp_uwp ?simp ?k mode =
  let module Toposort = Graph.Topological.Make(Cfg.AST.G) in
  Wp.build_passified_uwp Toposort.iter (fwp ~normalusage:false ?simp ?k mode)

let fwp_lazyconc ?simp ?k ?cf mode p q =
  let module FWPCONC = Make(VMDelta) in
  FWPCONC.fwp_lazyconc ?simp ?k ?cf mode p q

let fwp_lazyconc_uwp ?simp ?k ?cf mode p q =
  let module FWPCONC = Make(VMDelta) in
  FWPCONC.fwp_lazyconc_uwp ?simp ?k ?cf mode p q
