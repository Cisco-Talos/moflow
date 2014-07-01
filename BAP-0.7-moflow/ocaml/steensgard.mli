(** Steensgard's loop nesting algorithm

    See Steensgaard, B. (1993). Sequentializing Program Dependence
    Graphs for Irreducible Programs (No. MSR-TR-93-14).
*)

(** Loop information for CFGs with vertices of type ['a]. *)
type 'a loopinfo =
  | BB of 'a
  | Other of 'a loopinfo list
  | Loop of 'a loopinfo list (* body of loop *)

val steensgard_ast : Cfg.AST.G.t -> Cfg.AST.G.V.t loopinfo

val steensgard_ssa : Cfg.SSA.G.t -> Cfg.SSA.G.V.t loopinfo
