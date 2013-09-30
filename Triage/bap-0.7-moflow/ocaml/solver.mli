(** Primary interface to SMT solvers.

    @author Thanassis Avgerinos, Sang Kil Cha, ejs
*)

(* XXX: The pp_sol and model_exp functions are confusing. *)

exception Solver_error of string

(** Output signature for a solver *)
module type Solver_Out = sig
  type set (** Solver context *)
  val is_satisfiable : Ast.exp -> bool (** [is_sat e] returns true iff e is satisfiable. *)
  val is_valid : Ast.exp -> bool (** [is_valid e] returns true iff e is satisfiable *)
    (** Objects wrapping around a solver context *)
  class solver :
  object
    val s : set
    method add_binding : Ast.var -> Ast.exp -> unit (** Add a [Ast.Let] binding to context *)
    method add_constraint : Ast.exp -> unit (** Add boolean constraint to context *)
    method del_binding : Ast.var -> unit (** Delete boolean constraint from context *)
    method is_sat : bool (** true iff context is satisfiable *)
    method is_valid : Ast.exp -> bool (** true iff context is valid *)
    method model_exp : Ast.exp -> Ast.exp (** [model_exp e] evaluates [e] in the current model *)
    method pop : unit (** Pop solver context from context stack *)
    method push : unit (** Push current solver context onto context stack *)
    method pp_sol : string
  end
  val newsolver : unit -> solver (** Create a new solver object *)
end

(* No need to expose these functors for now *)
(* module Make : *)
(*   functor (S : Solver) -> *)
(*     Solver_Out *)

(* module MakeFromExec : *)
(*   functor (S : Smtexec.SOLVER) -> *)
(*     Solver *)

(* It is unclear if we want to expose the set type so that users can
   have direct access to the underlying functions.  Currently we do
   not. *)

module Z3 : Solver_Out (** Z3 direct binding interface *)
module STPExec : Solver_Out (** STP command line interface *)
module STPSMTLIBExec : Solver_Out (** STP command line interface in SMTLIB mode *)
module CVC3Exec : Solver_Out (** CVC3 command line interface *)
module CVC3SMTLIBExec : Solver_Out (** CVC3 command line interface in SMTLIB mode *)
module YICESExec : Solver_Out (** Yices command line interface *)

(** A hashtbl that maps a solver name to a solver creation function.
    Useful for subtyping. *)
val solvers : (string, unit -> STPExec.solver) Hashtbl.t
