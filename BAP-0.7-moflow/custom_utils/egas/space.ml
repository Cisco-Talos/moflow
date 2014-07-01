module L = List
module Hsh = Hashtbl
module Pr = Printf
(* BAP *)
module Ty = Type
module Ast = Ast
(* self *)
open Common.D (* dprintf *)

module IntSet = Set.Make( 
struct
  let compare = Pervasives.compare
  type t = int
end 
)

(* This module implements data structure that represents state of the execution 
 * tree we are exploring. 
 *)
module MakeSearchSpace(Br:Branches.BRANCH_OUT with type dir_t = bool) = 
struct
  module Exporter = Export.DotExporter
  type node_id = int
  (* Sample file id. *)
  type sample_id = int
  type bad = Unsat | Div of sample_id * node_id 
  type node = 
    (* Branch label, true branch, false branch*)
    | Node of Br.lbl_t * node_id * node_id 
    (* Id of sample file driving execution to this point in the tree. Small
     * leafs represent untraced sample files and can turn into big leafs after
     * expansion. *)
    | SmallLeaf of sample_id 
    (* Big leafs can't grow, they are final *)
    | BigLeaf of sample_id
    (* This path is Unsat or sample file generates a diverging path *)
    | Bad of bad
    (* This path was not yet explored *)
    | Unk
  type hsh_tree = (node_id, node) Hsh.t
  type hsh_s2l = (sample_id, node_id) Hsh.t
  type t = {tree: hsh_tree; root: node_id; sample2leaf: hsh_s2l}

  exception Divergence of node_id

  let bad2str = function
    | Unsat -> "Unsat or flip limit reached"
    | Div(sample_id, id) -> Pr.sprintf "Div(%d, %d)" sample_id id

  let node2str = function
    | Node(lbl, true_id, false_id) -> 
      Pr.sprintf "Node(%s, %d, %d)" (Br.label2str lbl) true_id false_id
    | SmallLeaf(sample_id) -> Pr.sprintf "SmallLeaf(%d)" sample_id
    | BigLeaf(sample_id) -> Pr.sprintf "BigLeaf(%d)" sample_id
    | Bad(bad) -> bad2str bad
    | Unk -> "Unk"
  
  let graph_node2str = function
    | Node(lbl, _, _) -> 
      Pr.sprintf "%s" (Br.label2str lbl) 
    | SmallLeaf(sample_id) -> Pr.sprintf "%d" sample_id
    | BigLeaf(sample_id) -> Pr.sprintf "%d." sample_id
    | Bad(bad) -> bad2str bad
    | Unk -> "Unk"

  let new_id =
    let varcounter = ref 0 in
    (fun () ->
       let n = !varcounter in
       if n = -1 then failwith "new_id: counter wrapped around";
       (varcounter := n+1;
        !varcounter)
    )
 
  (* We want to keep track of how samples map to leaves, so we use these
   * "callbacks" for bookkeeping this info. *)
  let unpack_small_leaf = function
    | SmallLeaf(sample_id) -> Some(sample_id)
    | _ -> None

  let maybe unp elt f = 
    match unp elt with
    | Some(v) -> f v
    | None -> ()

  let on_remove_small_leaf {sample2leaf=s2l} sample_id = 
    Hsh.remove s2l sample_id

  let on_add_small_leaf {sample2leaf=s2l} leaf_id sample_id = 
    Hsh.add s2l sample_id leaf_id

  (* replace, get, create new nodes in the tree *)
  let replace_elt ({tree=tree} as ctx) old_id new_elt = 
    let _ = assert (Hsh.mem tree old_id) in
    let old_elt = Hsh.find tree old_id in
    let _ = maybe unpack_small_leaf old_elt (on_remove_small_leaf ctx) in
    let _ = maybe unpack_small_leaf new_elt (on_add_small_leaf ctx old_id) in
    Hsh.replace tree old_id new_elt

  let get_elt ({tree=tree} ) id = 
    let _ = assert (Hsh.mem tree id) in
    Hsh.find tree id
  
  let new_elt ({tree=tree} as ctx) elt = 
    let id = new_id() in
    Hsh.add tree id elt;
    let _ = maybe unpack_small_leaf elt (on_add_small_leaf ctx id) in
    id
  
  (* insert new nodes of certain kind *)
  let new_unk ctx = new_elt ctx Unk
  let new_small_leaf ctx sample_id = new_elt ctx (SmallLeaf(sample_id))
  let new_big_leaf ctx sample_id = new_elt ctx (BigLeaf(sample_id))
  
  let cps_node ctx lbl dir false_id true_id = 
    let node = 
      if dir then Node(lbl, true_id, false_id)
      else Node(lbl, false_id, true_id)
    in
    let id = new_elt ctx node in
    id
  
  let empty_tree () = 
    let tree = Hsh.create 256 in
    let s2l = Hsh.create 256 in
    {tree=tree; root=(-1); sample2leaf=s2l}

  let tree_from_branches ?(replace_root=false) 
                  ?(small_leaf=true)
                  ({tree=tree;root=root_id} as ctx)
                  sample_id branches = 
    (* Make first path in continuation style *)
    let rec aux f_acc unks branches = 
      match branches with
      | branch::tl ->
        let dir = Br.get_dir branch in
        let lbl = Br.get_label branch in
        let false_id = new_unk ctx in
        let unks = false_id :: unks in
        let f = fun true_id -> f_acc (cps_node ctx lbl dir false_id true_id) in
        aux f unks tl
      | [] -> f_acc, L.rev unks
    in
    (* Make a small/big leaf, depending on whenever a sample file was traced *)
    let new_leaf ctx sample_id = 
      if small_leaf then new_small_leaf ctx sample_id
      else new_big_leaf ctx sample_id 
    in
    let f, unks = aux (fun x->x) [] branches in
    let id = new_leaf ctx sample_id in
    (* Build the tree *)
    let new_root_id = f id in
    let root_id = if replace_root then new_root_id else root_id in
    {ctx with tree=tree; root=root_id}, new_root_id, unks

  let tree_from_leaf sample_id = 
    let ctx = empty_tree () in
    let ctx, _, unks = tree_from_branches ~replace_root:true ctx sample_id [] in
    let _ = assert (unks=[]) in
    ctx

  (* Walk from root to SmallLeaf(sample_id). Throw exception on any divergence
   * from original path. Returns leaf_id, prefix, suffix. leaf_id is used to
   * transplant SmallLeaf with BigLeaf. *)
  let walk_to_leaf ctx sample_id branches = 
    let rec aux prefix suffix id = 
      let node = get_elt ctx id in
      match node, suffix with
      | Node(lbl, true_id, false_id), branch::tl -> 
        if lbl=Br.get_label branch then 
          if Br.get_dir branch then 
            aux (branch::prefix) tl true_id
          else
            aux (branch::prefix) tl false_id
        else
          raise (Divergence(id))
      | SmallLeaf(sample_id'), _ -> 
        if sample_id'=sample_id then 
          id, L.rev prefix, suffix
        else
          raise (Divergence(id))
      (* We expect only Nodes/SmallLeafs on our way down, so anything else is
       * an error *)
      | _, _ -> raise (Divergence(id))
    in
    try 
      let leaf_id, p, s = aux [] branches ctx.root in
      BigLeaf(sample_id), Some(leaf_id, p, s)
    with Divergence(id) ->
      let _ = dprintf "Leaf with sample id:%d diverged @ %d\n" sample_id id in
      Bad(Div(sample_id, id)), None

  let replace_small_leaf ctx small_leaf_id sample_id node = 
    let leaf = get_elt ctx small_leaf_id in
    let _ = assert (leaf = SmallLeaf(sample_id)) in
    let _ = replace_elt ctx small_leaf_id node in
    ctx

  (* Replace small leaf with a tree ending with big leaf *)
  let grow_leaf ctx small_leaf_id sample_id suffix =
    let ctx, sub_root_id, unks = 
      tree_from_branches ~small_leaf:false ctx sample_id suffix in
    let sub_root = get_elt ctx sub_root_id in
    (* Root will be transplanted in place of SmallLeaf. There's no need to keep
     * a second mapping. *)
    let _ = Hsh.remove ctx.tree sub_root_id in
    (* Transplant *)
    let ctx = replace_small_leaf ctx small_leaf_id sample_id sub_root in
    ctx, unks

  (* Branches extend past the leaf which generated them. Split the list in two 
   * parts: branches=prefix+suffix, so that leaf is between prefix and suffix.
   *)
  let split_by_and_grow_leaf ctx sample_id branches = 
    let r = walk_to_leaf ctx sample_id branches in
    match r with
    | BigLeaf(_), Some(small_leaf_id, prefix, suffix) -> 
      (* grow_leaf will replace SmallLeaf by a tree ending with BigLeaf *)
      let ctx, unks = grow_leaf ctx small_leaf_id sample_id suffix in
      ctx, Some(prefix, suffix, unks)
    | Bad(Div(_)) as node, None -> 
      (* FIXME: does this work (test it)? *)
      let _ = assert (Hsh.mem ctx.sample2leaf sample_id) in
      let small_leaf_id = Hsh.find ctx.sample2leaf sample_id in
      let ctx = replace_small_leaf ctx small_leaf_id sample_id node in 
      ctx, None
    | _, _ -> failwith "split_by_and_grow_leaf: unexpected combination"
  
  let replace_unks ({tree=tree} as ctx) unk_ids sample_ids =
    let leaf_or_unsat opt_sample_id = 
      match opt_sample_id with
      | Some(id) -> SmallLeaf(id)
      | None -> Bad(Unsat)
    in
    let replace_unk unk_id opt_sample_id = 
      let unk = get_elt ctx unk_id in
      let _ = assert (unk = Unk) in
      let terminator = leaf_or_unsat opt_sample_id in
      replace_elt ctx unk_id terminator
    in
    L.iter2 replace_unk unk_ids sample_ids;
    ctx

  module E = Exporter
  let export_graph ctx ofn = 
    let common_attributes n = [E.Label(graph_node2str n)] in
    let leaf_attributes = function
      | Node _ -> []
      (* Light green *)
      | SmallLeaf _ -> [E.Color(0x99ff99)]
      (* Light blue *)
      | BigLeaf _ -> [E.Color(0x66ccff)]
      (* Light red for errors *)
      | Bad(Unsat) -> [E.Color(0xff9999)]
      | Bad(Div _) -> [E.Color(0xff4d4d)]
      (* Pink, because we don't expect to see this node *)
      | Unk -> [E.Color(0xff66cc)]
    in
    let get_attrs id = 
      let node = get_elt ctx id in
      let attrs = common_attributes node in
      let attrs = attrs @ leaf_attributes node in
      attrs
    in
    let consume id node g =
      match node with 
      | Node(_, true_id, false_id) -> 
        let attrs = common_attributes node in
        let t_attrs = get_attrs true_id in
        let f_attrs = get_attrs false_id in
        E.add_node g (id, attrs) (true_id, t_attrs) (false_id, f_attrs)
      (* add_node above adds vertices if necessary, so adding these will have
       * no effect anyway *)
      | SmallLeaf(_) | BigLeaf(_) | Bad(_) | Unk -> 
        g
    in
    let g = E.init () in
    let g = Hsh.fold consume ctx.tree g in
    E.save_to_file g ofn

end
