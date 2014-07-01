(** Code for dealing with reachable/unreachable nodes in a CFG. *)

module D = Debug.Make(struct let name = "Reachable" and default=`NoDebug end)

(** Along the lines of Builder.S, but with remove instead of add *)
module type G =
sig
  include Graph.Builder.S

  val remove_vertex : G.t -> G.V.t -> G.t
  val copy_map : G.t -> G.t
  val v2s : G.V.t -> string
end

module type Reach =
sig

  type gt
  type vt
    
  val iter_reachable : (vt -> unit) -> gt -> vt -> unit
  val iter_unreachable : (vt -> unit) -> gt -> vt -> unit
    
  val fold_reachable : (vt -> 'a -> 'a) -> gt -> vt -> 'a -> 'a
  val fold_unreachable : (vt -> 'a -> 'a) -> gt -> vt -> 'a -> 'a
    
  val reachable : gt -> vt -> vt list
  val unreachable : gt -> vt -> vt list
    
  val remove_unreachable : gt -> vt -> gt      
  val remove_unreachable_copy : gt -> vt -> gt      
end

(** Make functions for folding/iterating over reachable or unreachable vertices,
    and for removing them. *)
module Make (BI:G) =
struct

  type gt = BI.G.t
  type vt = BI.G.V.t

  module VS = Set.Make(BI.G.V)

 let iter_reachable f g v =
   let module D = Graph.Traverse.Dfs(BI.G) in
   D.prefix_component f g v

 let fold_reachable f g v a =
   let r = ref a in
   iter_reachable (fun v -> r := f v !r) g v;
   !r

 let fold_unreachable f g v a =
   let h = Hashtbl.create 57 in
   iter_reachable (fun v -> Hashtbl.add h v ()) g v;
   BI.G.fold_vertex (fun v a -> if Hashtbl.mem h v then a else f v a) g a

 let iter_unreachable f g v =
   fold_unreachable (fun v () -> f v) g v ()

 let reachable g v =
   fold_reachable (fun v vs -> v::vs) g v []

 let unreachable g v =
   fold_unreachable (fun v vs -> v::vs) g v []

 (** Remove unreachable implemented by actually removing unreachables *)
 let remove_unreachable_remove g v =
   let u = unreachable g v in
   let count = ref 0 in
   D.dprintf "removing %d" (List.length u);
   let g = List.fold_left (fun a v -> incr count; D.dprintf "removing %s" (BI.v2s v); (*D.dprintf "removed %d" !count;*) BI.remove_vertex a v) g u in
   D.dprintf "removed %d" !count;
   g

 (** Remove unreachable implemented by copying reachables *)
 let remove_unreachable_copy g v =
   (* let u = unreachable g v in *)
   (* let _s = List.fold_left (fun s v -> VS.add v s) VS.empty u in *)
   let newg = BI.copy_map g in

   (* First, copy all vertices to new graph *)
   let add_node n g =
     (* D.dprintf "Adding vertex"; *)
     BI.add_vertex g n
   in
   let newg = fold_reachable add_node g v newg in

   (* Now, copy all edges to new graph *)
   let add_edges n newg =
     (* D.dprintf "Adding edges"; *)
     let add_edge e newg = BI.add_edge_e newg e in
     BI.G.fold_succ_e add_edge g n newg
   in
   let newg = fold_reachable add_edges g v newg in
   newg

 let remove_unreachable = remove_unreachable_remove

end



module SSA = Make(Cfg.SSA)
module AST = Make(Cfg.AST)
