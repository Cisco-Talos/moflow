(** Coalesce sequential basic blocks into a single basic block.

    A statement [s] is reorderable if [s] can be swapped with an
    adjacent reorderable statement [s2] without changing the semantics
    of the program.  For example, labels and comments are reorderable
    statements.

    A sequence [seq] of basic blocks [bb1, ..., bbn] is sequential if
    - [seq] is the only path from [bb1] to [bbn] in the control flow graph.
    - The first (closest to bb1) non-reorderable statement in [seq]
      dominates all other non-reorderable statements in [seq].
    - [bbn] postdominates all other basic blocks in [seq].
*)

val coalesce_ast : Cfg.AST.G.t -> Cfg.AST.G.t
(** coalesce_ast [cfg] returns a new AST CFG in which sequential basic
    blocks in [cfg] are coalesced into a single basic block. *)
val coalesce_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.t
(** coalesce_ssa [cfg] returns a new SSA CFG in which sequential basic
    blocks in [cfg] are coalesced into a single basic block. *)
