(** Code for removing unreachable nodes in a CFG. *)

(** [prune_unreachable_ast g] returns an AST CFG in which nodes
    unreachable from BB_Entry are removed. *)
val prune_unreachable_ast : Cfg.AST.G.t -> Cfg.AST.G.t

(** Same as [prune_unreachable_ast] but for SSA CFGs. *)
val prune_unreachable_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.t
