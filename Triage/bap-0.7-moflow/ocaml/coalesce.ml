(** A module to perform coalescing on CFGs *)

open BatListFull
open Cfg

module D=Debug.Make(struct let name="Coalesce" and default=`NoDebug end)
open D

module type CFG' = sig
  include CFG
  val is_safe_to_coalesce: lang -> bool (* Is the BB safe to coalesce to the start of another BB? True for BBs consisting only of labels, comments, etc. *)
end

module MakeCoalesce (C: CFG') =
struct
  module G = C.G
  module GS = Set.Make (struct
                         type t = G.V.t
                         let compare = Pervasives.compare
                        end)

 (* The function that does the coalescing.
  *  - While doing a DFS we join two nodes n1 and n2 if
  *    a) n1 has only one successor and
  *    b) n2 has n1 as its only predecessor
  *       OR all nodes before n1 have safe stmts (comments, labels, etc.)
  *)
  let coalesce cfg =
   let module CC = Checks.MakeConnectedCheck(C) in
   let () = CC.connected_check cfg "coalesce" in
   let entry_node = C.find_vertex cfg BB_Entry in
   let visited = ref GS.empty in
   let isspecial v = match G.V.label v with BB _ -> false | _ -> true in
   let add_visited v = visited := GS.add v !visited in
   let rec fold_dfs graph init =
     if GS.mem init !visited then graph
     else
       let worklist, graph =
        (* we start with a node init *)
         let rec immediate_succs ?(safe=true) acc node =
           let stmts = C.get_stmts graph node in
           let node_safe = C.is_safe_to_coalesce stmts in
           let safe = safe && node_safe in
           match G.succ graph node with
           | [successor] when not (List.mem successor acc)
               && not (isspecial successor) ->
             (match G.pred graph successor with
             | [] -> failwith "node's successor has no predecessor"
             | [_] when not (isspecial successor) ->
               immediate_succs ~safe (successor::acc) successor
             | multiplepreds when safe ->
               (* node's successor has multiple predecessors.
                  Normally it isn't safe to merge in this case, but if
                  all the nodes we've explored so far are just labels and
                  comments, it is okay. *)
               immediate_succs ~safe (successor::acc) successor
             | multiplepreds ->
               (* The current node or a previous node had a statement unsafe to coalesce with multiple preds *)
               acc
             )
           | _ ->
             acc
         in
        (* let's get the immediate successor nodes that follow the node.
           In this context, immediate means: n1 -> n2 -> n3 -> (n4|n5)
           should return [n3; n2; n1] *)
        let successors = immediate_succs [] init in
        if successors <> [] then (
          (* Now let's coalesce them cleverly *)
          dprintf "Coalescing %s +%s" (C.v2s init) (List.fold_left (fun s v -> s ^ " " ^ (C.v2s v)) "" successors);
          let init_stmts = C.get_stmts graph init in
          let all_stmts = List.map (C.get_stmts graph) successors in
          let big_stmt_block =
            List.fold_left (fun stmts ith_stmt -> C.join_stmts ith_stmt stmts)
              (List.hd all_stmts)
              (List.tl all_stmts)
          in
          (* add the edges to the successors *)
          let successors_of_successors_e = G.succ_e graph (List.hd successors) in
          let newsuccessors = G.succ graph (List.hd successors) in
          let add_edge graph edge =
            dprintf "Adding edge from %s to %s" (C.v2s init) (C.v2s (C.G.E.dst edge));
            let newedge = C.G.E.create init (C.G.E.label edge) (C.G.E.dst edge) in
            C.add_edge_e graph newedge
          in
          let graph = List.fold_left add_edge graph successors_of_successors_e in

          (* Also add edges from any predecessors to init. This only
             happens when there are comments/labels would be coalesced
             before the original targets. *)
          let graph = List.fold_left
            (fun graph succ ->
              List.fold_left (fun graph e ->
                let src = G.E.src e in
                (* Don't re-add nodes being coalesced. *)
                if List.mem src successors || src = init then graph
                else let label = G.E.label e in
                     let newe = G.E.create src label init in
                     dprintf "Adding edge originally from %s pred from %s to %s" (C.v2s succ) (C.v2s src) (C.v2s init);
                     C.add_edge_e graph newe
              ) graph (G.pred_e graph succ)
            ) graph successors
          in

          (* Remove unused successors *)
          let graph = List.fold_left C.remove_vertex graph successors in

          (* Replace the contents of init *)
          let big_stmt_block = C.join_stmts init_stmts big_stmt_block in
          let graph = C.set_stmts graph init big_stmt_block in
          add_visited init;
          (newsuccessors, graph)
        )
        else (
          let successors = G.succ graph init in
          add_visited init;
          (successors, graph)
        )
     in

     List.fold_left fold_dfs graph worklist
   in
   let graph = fold_dfs cfg entry_node in
   graph
end

module AST' =
struct
  include AST
  let remove_redundant_jump p =
    match List.rev p with
    | Ast.Jmp (e, _) as s::tl  when Ast.lab_of_exp e <> None ->
      let str = "Removed redundant jump: " ^ (Pp.ast_stmt_to_string s) in
      let rs = Ast.Comment(str, []) in
      List.rev (rs::tl)
    | _ -> p
  let is_safe_to_coalesce p =
    let is_safe_to_coalesce_stmt = function
      | Ast.Comment _
      | Ast.Label _ -> true
      | _ -> false
    in
    let p = remove_redundant_jump p in
    List.for_all is_safe_to_coalesce_stmt p
end

module SSA' =
struct
  include SSA
  let remove_redundant_jump p =
    match List.rev p with
    | Ssa.Jmp (e, _) as s::tl  when Ssa.val_of_exp e <> None ->
      let str = "Removed redundant jump: " ^ (Pp.ssa_stmt_to_string s) in
      let rs = Ssa.Comment(str, []) in
      List.rev (rs::tl)
    | _ -> p
  let is_safe_to_coalesce p =
    let is_safe_to_coalesce_stmt = function
      | Ssa.Comment _
      | Ssa.Label _ -> true
      | _ -> false
    in
    let p = remove_redundant_jump p in
    List.for_all is_safe_to_coalesce_stmt p
end

module AST_Coalesce = MakeCoalesce(AST')
module SSA_Coalesce = MakeCoalesce(SSA')

let coalesce_ast = AST_Coalesce.coalesce
let coalesce_ssa = SSA_Coalesce.coalesce
