(** Dependency graphs.

    This module contains utilities for computing dependency graphs over
    graph representations.  Dependency graphs can reflect data
    dependencies (DDG), control dependencies (CDG), or both (PDG).  In
    addition, this module contains some support for usedef chains, and can
    identify undefined variables.
*)

(** {3 Control Dependence Graphs} *)

(** Control dependence module type *)
module type CDG =
sig
  module G :
  sig
    type t
    module V :
    sig
      type t
    end
  end

  (** This function computes control dependencies.
      This implements the algorithm in the Tiger Book p.454 (ML
      version) with the exception we do not add a new node before
      entry. Therefore all nodes are control dependent on BB_Entry.

      Note that BB_Exit will not be control dependent on anything,
      thus a lone node in the graph (you can prune it away if you want
      using other utilities in BAP)

      @return a map from a node to its parents in the CDG tree.

  *)
  val compute_cd : G.t -> G.V.t -> G.V.t list

(** computes the control dependence graph (cdg), which turns the
    result of [compute_cd] below into a graph*)
  val compute_cdg : G.t -> G.t
end

(** Functor to produce control dependence analysis module for a CFG *)
module MakeCDG :
  functor (C : Cfg.CFG) -> CDG with module G = C.G

(** Control dependence graphs for SSA CFGs *)
module CDG_SSA : CDG with module G = Cfg.SSA.G

(** Control dependence graphs for AST CFGs *)
module CDG_AST : CDG with module G = Cfg.AST.G


(** {3 Data Dependence Graphs} *)

(** Data dependence graphs for SSA CFGs *)
module DDG_SSA :
  sig
    (** A statement location is identified by a basic block and the
        nth statement in the block. *)
    type location = Cfg.SSA.G.V.t * int

    (** A set of locations *)
    module SS : Set.S with type elt = location

    (** [compute_dd cfg] returns the tuple [vars,fd,fu]. [vars] is the
        set of variables used by the graph.  [fd] is a hashtbl from vars
        to their definition location.  [fu] is a hashtbl from vars to
        their use locations.  Unlike graphs such as DDG and PDG (below),
        we do not assume that vars are defined on entry and used on
        exit.  *)
    val compute_dd : Cfg.SSA.G.t -> Var.VarSet.t * location Var.VarHash.t * location list Var.VarHash.t

    (** [compute_dds g h] adds a mapping for each data dependency in [g] to [h].

        Deprecated.
    *)
    val compute_dds : Cfg.SSA.G.t -> (location, location) Hashtbl.t -> unit

    (** [compute_ddg g] returns a data dependency graph for the CFG [g]. *)
    val compute_ddg : Cfg.SSA.G.t -> Cfg.SSA.G.t

    (** [stmtlist_to_single_stmt g] returns a CFG equivalent to [g],
        but that has one statement per vertex.  This makes edges in a
        dependency graph more precise. *)
    val stmtlist_to_single_stmt : Cfg.SSA.G.t -> Cfg.SSA.G.t
  end

(** Data dependence graphs for AST CFGs *)
module DDG_AST :
sig
  (** A statement location is identified by a basic block and the
      nth statement in the block. *)
  type location = Cfg.AST.G.V.t * int

  (** A set of locations *)
  module SS : Set.S with type elt = location

  (** [compute_dd cfg] returns the tuple [vars,fd,fu]. [vars] is the
      set of variables used by the graph.  [fd] is a hashtbl from vars
      to their definition location.  [fu] is a hashtbl from vars to
      their use locations.  Unlike graphs such as DDG and PDG (below),
      we do not assume that vars are defined on entry and used on
      exit.  *)
  val compute_dd : Cfg.AST.G.t -> Var.VarSet.t * location Var.VarHash.t * location list Var.VarHash.t

  (** [compute_dds g h] adds a mapping for each data dependency in [g] to [h].

      Deprecated.
  *)
  val compute_dds :
    Cfg.AST.G.t -> (location, location) Hashtbl.t -> unit

  (** [compute_ddg g] returns a data dependency graph for the CFG [g]. *)
  val compute_ddg : Cfg.AST.G.t -> Cfg.AST.G.t
end

(** {3 Program Dependence Graphs}

    Program dependence graphs are the union of the control and data
    dependence graphs.
*)

(** Program dependence graphs for AST CFGs *)
module PDG_AST :
sig
  (** [compute_pdg g] returns the program dependence graph for [g]. *)
  val compute_pdg : Cfg.AST.G.t -> Cfg.AST.G.t
end

(** {3 Use/Def and Def/Use Analyses} *)

(** Use/Def and Def/Use chains on AST CFGs *)
module UseDef_AST :
sig
  (** A statement location is identified by a basic block and the
      nth statement in the block. *)
  type location = Cfg.AST.G.V.t * int

  module LocationType :
  sig
    type t = Undefined | Loc of location
    val compare : 'a -> 'a -> int
    val to_string : t -> string
  end
  module LS : Set.S with type elt = LocationType.t

  (** Given an AST CFG, returns 1) a hash function mapping locations to
      the definitions reaching that location and 2) a function that
      returns the definitions for a (variable, location) pair

      XXX: Aren't these almost the same thing?
*)
  val usedef :
    Cfg.AST.G.t ->
    (location, LS.t Var.VarMap.t) Hashtbl.t *
      (location -> Var.t -> LS.t)

  (** Same as [usedef], but for def use chains.  That is, these
      functions map definitions to their possible uses. *)
  val defuse :
    Cfg.AST.G.t ->
    (location, LS.t Var.VarMap.t) Hashtbl.t *
      (location -> Var.t -> LS.t)


end

(** Various functions relating to variable definitions in AST CFGs *)
module DEFS_AST :
sig
  (** A statement location is identified by a basic block and the
      nth statement in the block. *)
  type location = Cfg.AST.G.V.t * int

  (** Return variables that might be referenced before they are
      defined. The output of this function is a good starting point
      when trying to find inputs of a program. Returns a hash of
      values that are always undefined, and sometimes undefined
      (depending on program path). *)
  val undefined : Cfg.AST.G.t ->
    location Var.VarHash.t * location Var.VarHash.t

  (** Returns a tuple with set of variables that are always undefined,
      and a set of variables that may be undefined. *)
  val undefinedvars : Cfg.AST.G.t -> Var.VarSet.t * Var.VarSet.t

  (** Returns a tuple with a set of variables that are defined, and a set of variables that are referenced but never defined.

      Deprecated.
  *)
  val defs : Cfg.AST.G.t -> Var.VarSet.t * Var.VarSet.t

  (** Return a set of all variables defined or referenced. *)
  val vars : Cfg.AST.G.t -> Var.VarSet.t
end

(** Various functions relating to variable definitions in SSA CFGs *)
module DEFS_SSA : sig
  (** Returns a tuple with a set of variables that are defined, and a set of variables that are referenced but never defined.

      Deprecated.
  *)
  val defs : Cfg.SSA.G.t -> Var.VarSet.t * Var.VarSet.t
end
