(** Dataflow module for use with the ocamlgraph library

    @author Ivan Jager
*)


(** Types of control flow graph that data flow is defined on *)
module type G =
sig
  type t
  module V : Graph.Sig.COMPARABLE
  val pred : t -> V.t -> V.t list
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

(** Data flow direction.  [Forward] dataflow propagates values in the
    same direction as edges in the control flow graph, while
    [Backward] dataflow propagates values in the reverse direction. *)
type direction = Forward | Backward


(** The lattice the dataflow is defined on.  See
    {{:http://en.wikipedia.org/wiki/Meet-semilattice}here} for more
    information.
*)
module type BOUNDED_MEET_SEMILATTICE =
sig

  (** The type of a latice element *)
  type t

  (** Top of the latice *)
  val top : t

  (** The meet operator.
      [meet v1 v2] should form a lattice. In particular,
      [meet v1 Top = meet Top v1 = v1],
      and [meet Bottom _ = meet _ Bottom = Bottom].
  *)
  val meet : t -> t -> t

  (** Equality checking for latice values.  Returns true when the two
      latice elements are the same.  *)
  val equal : t -> t -> bool
end

(** A dataflow problem is defined by a lattice over a graph. *)
module type DATAFLOW =
  sig

    module L : BOUNDED_MEET_SEMILATTICE
    module G : G

    (** The transfer function over node elements, e.g., basic
        blocks. *)
    val transfer_function : G.t -> G.V.t -> L.t -> L.t

    (** The starting node for the analysis. *)
    val s0 : G.t -> G.V.t

    (** The initial lattice value given to node [s0]. All other nodes
        start out with [Top]. *)
    val init : G.t -> L.t

    (** The dataflow direction. *)
    val dir : direction
  end

(** Build a custom dataflow algorithm for the given dataflow problem [D]. *)
module Make :
  functor (D : DATAFLOW) ->
sig
  (** [worklist_iterate g] returns a worklist algorithm for graph [g]
      as a pair of functions [in,out]. [in], when given a node [v],
      computes the lattice value going in to that node, [v]. [out],
      when given a node [v], computes the lattice value exiting
      [v]. *)
  val worklist_iterate : ?init:(D.G.t -> D.L.t) ->
    D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)
end
