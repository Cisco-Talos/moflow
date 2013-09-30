(** Loop unrolling *)

(** The type signature of a loop unrolling function. Count specifies
    the number of times to unroll loops. *)
type unrollf = ?count:int -> Cfg.AST.G.t -> Cfg.AST.G.t

(** The type signature of the mapping from the original nodes to the node in the
    unrolled cfg *)
type node_mapping = (Cfg.AST.G.V.t * Cfg.AST.G.V.t) list

(** Unroll loops identified by Steensgard's loop forest algorithm:

    Steensgaard, B. (1993). Sequentializing Program Dependence Graphs for Irreducible Programs (No. MSR-TR-93-14).
*)
val unroll_loops_steensgard: unrollf

(** Unroll loops identified by structural analysis. The current
    structural analysis implementation is incomplete and may fail
    (raise an exception). *)
val unroll_loops_sa: unrollf

(** Unroll loops using the default loop identification algorithm,
    which is unspecifed and may change. *)
val unroll_loops: unrollf

(** Unroll loops using the default loop identification algorithm,
    which is unspecifed and may change. *)
val unroll_loops_with_mapping: ?count:int -> Cfg.AST.G.t -> Cfg.AST.G.t * node_mapping
