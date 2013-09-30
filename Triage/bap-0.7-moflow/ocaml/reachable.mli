(** Reachability analysis *)

(** Graph signature needed for reachability analysis *)
module type G =
sig
  include Graph.Builder.S
  val remove_vertex : G.t -> G.V.t -> G.t
  val copy_map : G.t -> G.t
  val v2s : G.V.t -> string
end

(** Output signature of reachability analysis *)
module type Reach =
  sig

    (** Graph type. *)
    type gt

    (** Vertex type. *)
    type vt

      (** [iter_reachable f g v] calls [f v'] for every vertex [v'] that is reachable from [v]. *)
    val iter_reachable : (vt -> unit) -> gt -> vt -> unit

      (** [iter_unreachable f g v] calls [f v'] for every vertex [v'] that is unreachable from [v]. *)
    val iter_unreachable : (vt -> unit) -> gt -> vt -> unit

    (** Fold over reachable vertices. *)
    val fold_reachable : (vt -> 'a -> 'a) -> gt -> vt -> 'a -> 'a

    (** Fold over unreachable vertices. *)
    val fold_unreachable : (vt -> 'a -> 'a) -> gt -> vt -> 'a -> 'a

    (** Return a list of reachable vertices. *)
    val reachable : gt -> vt -> vt list
    (** Return a list of unreachable vertices. *)
    val unreachable : gt -> vt -> vt list

    (** Return a new graph with unreachable nodes removed. *)
    val remove_unreachable : gt -> vt -> gt

    (** Same as [remove_unreachable], but implemented
        differently. Creates a new graph and copies all reachable nodes.
        This can be more efficient for large graphs when many vertices
        are unreachable. *)
    val remove_unreachable_copy : gt -> vt -> gt
  end

(** Functor that builds reachability analysis *)
module Make :
  functor (BI : G) ->
    Reach with type gt = BI.G.t and type vt = BI.G.V.t

(** {3 Reachability analyses for Control Flow Graphs} *)

(** Reachability analysis for AST CFGs *)
module AST : (Reach with type gt = Cfg.AST.G.t and type vt = Cfg.AST.G.V.t)

(** Reachability analysis for SSA CFGs *)
module SSA : (Reach with type gt = Cfg.SSA.G.t and type vt = Cfg.SSA.G.V.t)

