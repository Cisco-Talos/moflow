(** Printing formulas *)

module VH = Var.VarHash

module D = Debug.Make(struct let name = "Formulap" and default=`NoDebug end)
open D

open Ast
open Type

(** Returns a list of free variables in the given expression *)
let freevars e =
  let freevis =
    object(self)
      inherit Ast_visitor.nop
      val ctx = VH.create 570
      val found = VH.create 570

      method get_found =
	(* dprintf "found %d freevars" (VH.length found); *)
	List.rev (VH.fold (fun k () a -> k::a) found [])
      method add_dec d =
	if not(VH.mem found d || VH.mem ctx d)
	then VH.add found d ()
	(* else dprintf "Not adding %s." (Pp.var_to_string d) *)

      method visit_exp = function
	| Let(v, e1, e2) ->
	    ignore(Ast_visitor.exp_accept self e1);
	    VH.add ctx v ();
	    ignore(Ast_visitor.exp_accept self e2);
	    VH.remove ctx v;
	    SkipChildren
	| _ ->
	    DoChildren

      method visit_rvar r =
	self#add_dec r;
	DoChildren
    end
  in
  ignore(Ast_visitor.exp_accept freevis e);
  freevis#get_found

class virtual fpp =
object(self)
  method virtual forall : VH.key list -> unit
  method virtual ast_exp : Ast.exp -> unit
  method virtual assert_ast_exp : ?exists:(var list) -> ?foralls:(var list) -> Ast.exp -> unit
  method virtual valid_ast_exp : ?exists:(var list) -> ?foralls:(var list) -> Ast.exp -> unit
  method virtual counterexample : unit
end

class virtual fpp_oc =
object(self)
  inherit fpp
  method virtual close : unit
  method virtual flush : unit
end

(* Naming this type is useful.

   I guess we should/could change the type of fpp_oc too to avoid
this. *)
type fppf = out_channel -> fpp_oc

class virtual stream_fpp =
object(self)
  (** Begin a list of constraints at the start of the formula *)
  method virtual and_start : unit
  (** Add a constraint to a list of constraints *)
  method virtual and_constraint : Ast.exp -> unit
  (** Close constraint at end of the formula

      E.g., print ')' *)
  method virtual and_close_constraint : unit
  (** End a list of constraints at the end of the formula

      E.g., print 'true' *)
  method virtual and_end : unit
  (** Begin a let binding *)
  method virtual let_begin : var -> Ast.exp -> unit
  (** End a let binding *)
  method virtual let_end : var -> unit
  (** Open a new benchmark for a streaming formula, which is assumed
      to use the theory of bitvectors and arrays *)
  method virtual open_stream_benchmark : unit
  (** Close the benchmark *)
  method virtual close_benchmark : unit
  (** Request a counter-example *)
  method virtual counterexample : unit
  (** Declaring a variable consists of calling predeclare_free_var to
      register the name and type, and then calling print_free_var. *)
  method virtual predeclare_free_var : var -> unit
  method virtual print_free_var : var -> unit

  (* Assuming free variables are already present, print the beginning of an assertion that e is satisfiable *)
  method virtual assert_ast_exp_begin : ?exists:(var list) -> ?foralls:(var list) -> unit -> unit
  (* Print the end of the assertion *)
  method virtual assert_ast_exp_end : unit

  (* Assuming free variables are already present, print the beginning of an assertion that e is valid *)
  method virtual valid_ast_exp_begin : ?exists:(var list) -> ?foralls:(var list) -> unit -> unit
  (* Print the end of the assertion *)
  method virtual valid_ast_exp_end : unit
end

class virtual stream_fpp_oc =
object(self)
  inherit stream_fpp
  method virtual close : unit
  method virtual flush : unit
end

type stream_fppf = out_channel -> stream_fpp_oc
