(** A module to perform AST Symbolic Execution *)

(** TODO list:
 *
 *  - Cleanup & make readable
 *)

open Ast
open Big_int_Z
open Big_int_convenience
(*open BatListFull*)
open Type

module VH = Var.VarHash
module VM = Var.VarMap

module D = Debug.Make(struct let name = "Symbeval" and default=`NoDebug end)
open D

(* For now, we'll map every byte. Later it may be better to map larger
   values, but we'd lose precision wrt uninitialized memory. *)
module AddrMap = Map.Make(Int64)

type mem = Ast.exp AddrMap.t * Var.t (* addr -> val + initial name *)

(* Some useful types *)
type addr = int64
type instr = stmt
type varid = Ast.exp
type varval = Symbolic of Ast.exp | ConcreteMem of mem
type label_kind = label
type form_type = Equal | Rename

type ('a,'b) ctx = {
  pred: 'b;
  mutable delta: 'a;
  sigma: (addr, instr) Hashtbl.t;
  lambda: (label_kind, addr) Hashtbl.t;
  pc: addr; (* Should be int *)
}

(* Memory equality

   XXX: This should be in arithmetic.ml, but the evaluator uses an
   internal type AddrMap to represent concrete memories.

   XXX: The semantics of memory equality are undefined.  For now,
   we'll say that an initial memory is undefined. So, unless a
   concrete memory is completely specified (e.g., has values for all
   2^32 mappings), it will not equal another concrete memory unless
   they originated from the same variable. *)
let eq_concrete_mem (m1,v1) (m2,v2) =
  let bool_to_astint = function
    | true -> (bi1, reg_1)
    | false -> (bi0, reg_1)
  in
  (* let print_map m = dprintf "Printing map"; AddrMap.iter (fun k m -> dprintf "%Lx -> %s" k (Pp.ast_exp_to_string m)) m in *)
  (* dprintf "cmpmem: %s %s" (Var.name v1) (Var.name v2); *)
  (* print_map m1; print_map m2; *)
  if v1 <> v2 then bool_to_astint false (* XXX: Technically, we should check to make sure that both
                                           mappings are not completely full and equivalent. *)
  else bool_to_astint (AddrMap.equal (===) m1 m2)

let mem_binop op m1 m2 = match op with
  | EQ -> eq_concrete_mem m1 m2
  | NEQ -> Arithmetic.unop NOT (eq_concrete_mem m1 m2)
  | _ -> failwith ((Pp.binop_to_string op) ^ " undefined for memories")

(* Useful shorthands *)
let empty_mem v = ConcreteMem(AddrMap.empty, v)
let empty_smem v = Symbolic(Var(v))
let val_true = Symbolic exp_true
let val_false = Symbolic exp_false
let is_true_val = function
  | Symbolic e -> full_exp_eq e exp_true
  | ConcreteMem _ -> false
let is_false_val = function
  | Symbolic e -> full_exp_eq e exp_false
  | ConcreteMem _ -> false
let is_symbolic = function
  | Symbolic (Int _) -> false
  | ConcreteMem _ -> false
  | _ -> true
let is_concrete_scalar = function
  | Int _ -> true
  | _ -> false
let is_concrete_mem_or_scalar = function
  | Symbolic(Int _) -> true
  | ConcreteMem _ -> true
  | _ -> false
let is_concrete_mem = function
  | ConcreteMem _ -> true
  | Symbolic _ -> false
let concrete_val_tuple = function
  | Symbolic (Int (v,t)) -> (v,t)
  | Symbolic e ->
      failwith ("expression cannot be evaluated concretely:\n"
		^(Pp.ast_exp_to_string e))
  | _ -> failwith "tried to perform memory operations"

(* Context functions (lookup, update etc) *)
let context_update = VH.replace
let context_copy = VH.copy

(* Unwrapping functions *)
let symb_to_exp = function
  | Symbolic e -> e
  | ConcreteMem _ -> failwith "symb_to_exp called on concrete memory"
let concmem_to_mem = function
  | ConcreteMem m -> m
  | _ -> failwith "not a concrete memory"
let symb_to_string = function
  | Symbolic e -> "Symbolic "^(Pp.ast_exp_to_string e)
  | ConcreteMem m -> "Memory"

(* Converting concrete memory to symbolic *)
let conc2symb memory v =
  pdebug "Concrete to symbolic" ;
  (* FIXME: a better symbolism for uninitialized memories *)
  let init = Var v in
  pdebug "The point of no return" ;
  Symbolic (AddrMap.fold
	      (fun k v m -> Store (m,Int(big_int_of_int64 k,reg_32),v,exp_false,reg_8))
	      memory init)

let varval_to_exp = function
  | Symbolic e -> e
  | ConcreteMem (m,v) -> symb_to_exp (conc2symb m v)

(* Normalize a memory address, setting high bits to 0. *)
let normalize i t = int64_of_big_int (Arithmetic.to_big_int (i,t))

module type MemLookup =
sig

  (** Lookup type *)
  type t

  (** Initial lookup table *)
  val create : unit -> t
  (** Clear the lookup table *)
  val clear : t -> t
  (** Deep copy the lookup table *)
  val copy : t -> t
  (** Print vars *)
  val print_values : t -> unit
  (** Print memories *)
  val print_mem : t -> unit

  (** Look up the value of a variable *)
  val lookup_var : t -> Var.t -> varval
  (** Update the value of a variable. *)
  val update_var : t -> Var.t -> varval -> t
  (** Remove the value for a variable. *)
  val remove_var : t -> Var.t -> t
  (** Update memory *)
  val update_mem : varval -> Ast.exp -> Ast.exp -> Ast.exp -> varval
  (** Lookup memory *)
  val lookup_mem : varval -> Ast.exp -> Ast.exp -> Ast.exp
end

module type EvalTune =
sig
  val eval_symb_let : bool
end

(* Newer, flexible formula type.  The input and output types are
   flexible.  For instance, the output will be type unit for streaming
   formulas. *)
module type FlexibleFormula =
sig
  type t
  type init
  type output
  val init : init -> t 
  val add_to_formula : t -> Ast.exp -> form_type -> t
  val output_formula : t -> output
end

(** Module that handles how Assignments are handled. *)
module type Assign =
  functor (MemL:MemLookup) ->
    functor (Form:FlexibleFormula) ->
sig
  (** Assign a variable. Does not modify the entire context in place. *)
  val assign : Var.t -> varval -> (MemL.t,Form.t) ctx -> (MemL.t,Form.t) ctx
end

module Make(MemL:MemLookup)(Tune:EvalTune)(Assign:Assign)(Form:FlexibleFormula) =
struct

  module MemL=MemL
  module Assign=Assign(MemL)(Form)
  module Form=Form

  (* Evaluator Contexts
     Symbol meanings:                              *
   * pc: the program counter                       *
   * Sigma: mapping the program counter to a stmt  *
   * Lambda: mapping a label to a program counter  *
   * Delta: mapping a variable to a value          *
   * Pred: the path predicate                      *)

  type myctx = (MemL.t,Form.t) ctx

  (* Program halted, with optional halt value, and with given execution context. *)
  exception Halted of varval option * myctx

  (* An unknown label was found *)
  exception UnknownLabel of label_kind

  (* An error occured *)
  exception Error of string * myctx

  (* An assertion failed *)
  exception AssertFailed of myctx

  (* An assumption failed, so the program did not start *)
  exception AssumptionFailed of myctx

  let byte_type = reg_8
  let index_type = reg_32

  (* Lookup functions for basic contexts *)
  let inst_fetch sigma pc =
    Hashtbl.find sigma pc

  let label_decode lambda lab =
    try Hashtbl.find lambda lab
    with Not_found ->
      match lab with
	| Name _ (*-> failwith ("jump to inexistent label "^s)*)
	| Addr _ -> 
	    (* I'd like to print a warning here, but traces rely on this
	       behavior, so it prints a lot of warnings if we leave it
	       on. *)
	    (* wprintf "Unknown label: %s" (Pp.label_to_string lab); *)
	    raise (UnknownLabel lab) (*failwith ("jump to inexistent label "^
					 (Printf.sprintf "%Lx" x)) *)

  let lookup_var        = MemL.lookup_var
  let update_var	= MemL.update_var
  let update_mem        = MemL.update_mem
  let remove_var	= MemL.remove_var
  let lookup_mem        = MemL.lookup_mem
  let assign            = Assign.assign
  let copy              = MemL.copy
  let print_values      = MemL.print_values
  let print_mem         = MemL.print_mem
  let eval_symb_let     = Tune.eval_symb_let
  let add_constraint    = Form.add_to_formula
  let output_formula    = Form.output_formula

  (* Initializers *)
  let create_state form_init =
    let sigma : (addr, instr) Hashtbl.t = Hashtbl.create 5700
    and pc = Int64.zero
    and delta : MemL.t = MemL.create ()
    and lambda : (label_kind, addr) Hashtbl.t = Hashtbl.create 5700 in
    {pred=(Form.init form_init); delta=delta; sigma=sigma; lambda=lambda; pc=pc}

  let initialize_prog state prog_stmts =
    Hashtbl.clear state.sigma ;
    Hashtbl.clear state.lambda ;
    (* Initializing Sigma and Lambda *)
    let pc =
      (List.fold_left
         (fun pc s ->
          Hashtbl.add state.sigma pc s ;
            (match s with
               | Label (lab,_) -> Hashtbl.add state.lambda lab pc
               | _ -> ()
            ) ;
            Int64.succ pc
         ) state.pc prog_stmts) in
    (* Add a halt to the end of the program *)
    Hashtbl.add state.sigma pc (Halt(exp_true, []))

  let cleanup_delta state =
    state.delta <- MemL.clear state.delta

  let build_default_context prog_stmts form_init =
    let state = create_state form_init in
      initialize_prog state prog_stmts ;
      state

  (* Evaluate an expression in a context Delta,Mu *)
  let rec eval_expr delta expr =
    (* Decide whether or not we should evaluate the memory, which can
       lead to exponential blow-up due to substitution. *)
    (* Evaluate an expression e, but only if the evaluation will be
       the same size as the unevaluated form (or smaller). Each
       expression type can assume that subexpression evaluation has
       the same property. *)
    let eval = function
      | Var v ->
	  (* This can clearly result in a large symbolic
	     result. Should we leave Vars? *)
	  lookup_var delta v
      | Int _ as value ->
	  Symbolic value
      | Lab _ as labl ->
	  Symbolic labl
      | Ite(cond,e1,e2) ->
	  let v1 = eval_expr delta e1
	  and v2 = eval_expr delta e2 in
	  (match eval_expr delta cond with
	   | v when is_symbolic v ->
	       Symbolic(Ite(symb_to_exp v, symb_to_exp v1, symb_to_exp v2))
	   | v when is_true_val v ->
	       Symbolic(symb_to_exp v1)
	   | v when is_false_val v ->
	       Symbolic(symb_to_exp v2)
	   | _ ->
	       failwith "not possible"
	  )
      | Extract(h,l,e) ->
	  let v = eval_expr delta e in
	  if is_symbolic v then (
	    Symbolic(Extract(h,l,symb_to_exp v))
	  ) else (
	    let (v,t) = Arithmetic.extract h l (concrete_val_tuple v) in
	    Symbolic(Int(v,t))
	  )
      | Concat(le,re) ->
	  let lv = eval_expr delta le
	  and rv = eval_expr delta re in
	  if is_symbolic lv || is_symbolic rv then (
	    Symbolic(Concat(symb_to_exp lv, symb_to_exp rv))
	  ) else (
	    let lv = concrete_val_tuple lv in
	    let rv = concrete_val_tuple rv in
	    let (v,t) = Arithmetic.concat lv rv in
	    Symbolic(Int(v,t))
	  )
      | BinOp (op,e1,e2) ->
	  (* In the worst case, we will just combine two symbolic
	     expressions. *)
	  let v1 = eval_expr delta e1
	  and v2 = eval_expr delta e2 in
          (match v1, v2 with
          | Symbolic (Int e1'), Symbolic (Int e2') ->
              (* We have two concrete scalars *)
	      let (v,t) = (Arithmetic.binop op e1' e2') in
	      Symbolic (Int(v,t))
          | ConcreteMem m1, ConcreteMem m2 ->
              (* We have two concrete memories. Note that this can
                 only return bool. *)
              Symbolic (Int (mem_binop op m1 m2))
          | _ ->
              (* Something is symbolic *)
	      let e1' = symb_to_exp v1
	      and e2' = symb_to_exp v2 in
	      Symbolic (BinOp (op, e1', e2')))
      | UnOp (op,e) ->
	  (* In the worst case, we will have a symbolic expression. *)
	  let v = eval_expr delta e in
	    if is_symbolic v then
	      Symbolic(UnOp(op, symb_to_exp v))
	    else
	      let e' = concrete_val_tuple v in
	      let (v,t) = Arithmetic.unop op e' in
		Symbolic (Int (v,t))
      | Cast (ct,t,e) ->
	  (* In the worst case, we will have a symbolic expression. *)
	  let v = eval_expr delta e in
	    if is_symbolic v then
	      let e' = symb_to_exp v in
		Symbolic(Cast(ct, t, e'))
	    else
	      let e' = concrete_val_tuple v in
	      let (n',t') = Arithmetic.cast ct e' t in
		Symbolic (Int (n',t'))
      | Let (var,e1,e2) ->
	(* Consider let v=e in e+e+e+e+e+e+e+e+e+e+e+e+e+e+e. If e
	   is not concrete, this could lead to a huge blowup.

           Thus, we have an option, eval_symb_let, that when unset,
           stops us from doing substitution in this case.  *)

	  let v1 = eval_expr delta e1 in
          if eval_symb_let then
	    let delta' = copy delta in (* FIXME: avoid copying *)
	    let delta' = update_var delta' var v1 in
	    let v2 = eval_expr delta' e2 in
            v2
          else (
          (* Partial evaluation is difficult.  If v1 is symbolic, we
             will make no attempt to get rid of the Let binding.
             Instead, we will evaluate e and e', keeping the Let
             binding in place.

             If v1 is concrete, however, we can get rid of the Let
             binding altogether, but only if var is not free in the
             evaluated e'. This can happen if we have let x = 1 in let
             foo = freevar in x.  We would evaluate let foo = freevar
             in x in the context where x is mapped to 1.  However,
             since freevar is a symbolic expression, we would not
             evaluate it further if eval_symb_let = false, and would
             return the evaluation expression let foo = freevar in x.
             However, this is incorrect, because we are removing the
             Let binding for x!  We should really wrap any free
             variable with a Let binding to its current value in the
             context.
          *)

            if is_symbolic v1 then
	      let delta' = copy delta in (* FIXME: avoid copying *)
              (* Remove so we don't expand references to var in delta *)
	      let delta' = remove_var delta' var in
	      let v2 = eval_expr delta' e2 in
              let v1' = varval_to_exp v1 in
              let v2' = varval_to_exp v2 in
              Symbolic(Let(var, v1', v2'))
            else (* v1 is concrete, do substitution *)
	      let delta' = copy delta in (* FIXME: avoid copying *)
	      let delta' = update_var delta' var v1 in
	      let v2 = eval_expr delta' e2 in
              match v2 with
              | Symbolic v2' ->
	        let fvars = Formulap.freevars v2' in
	        let isvar = (fun v -> not (Var.equal v var)) in
	        if List.for_all isvar fvars then
		  (* var is not free! We can get rid of the Let. *)
                  v2
	        else
		(* var is free in e2; we need to add the binding back *)
                  let v1' = varval_to_exp v1 in
		  Symbolic(Let(var, v1', v2'))
              | ConcreteMem _ -> v2)
      | Load (mem,ind,endian,t) ->
	(match t with
	| Reg 8 ->
		 (* This doesn't introduce any blowup on its own. *)
		 let mem = eval_expr delta mem
		 and ind = eval_expr delta ind
		 and endian = eval_expr delta endian in
		 let mem_arr = symb_to_exp ind
		 and endian_exp = symb_to_exp endian in
		   Symbolic (lookup_mem mem mem_arr endian_exp)
	     | Reg _ ->  (* we only care about 32bit *)
		 (* Splitting introduces blowup.  Can we avoid it? *)
		 eval_expr delta (Memory2array.split_loads mem ind t endian)
	     | Array _ ->
		 failwith ("loading array currently unsupported" 
                           ^ (Pp.typ_to_string t))
	     | _ -> failwith "not a loadable type"
	  )
      | Store (mem,ind,value,endian,t) ->
	  let index = symb_to_exp (eval_expr delta ind)
	  and value = symb_to_exp (eval_expr delta value)
	  and endian = symb_to_exp (eval_expr delta endian) in
	    (match t with
	       | Reg 8 ->
		   (* No blowup here. *)
		   let mem = eval_expr delta mem in
		     update_mem mem index value endian
	       | Reg _ ->
		   (* Splitting blowup, but I don't know how to avoid this. *)
		   eval_expr delta
		     (Memory2array.split_writes mem index t endian value)
	       | Array _ ->
		   failwith "storing array currently unsupported"
	       | _ ->
		   failwith "not a storable type"
	    )
      | Unknown _ as u -> Symbolic u (*failwith "unknown value encountered"*)
    in
      eval expr

  (* The statement evaluation is practically a transition: *
   * (Delta,Mu,pc,stmt) -> [(Delta',Mu',pc',stmt')]        *
   * The contexts Sigma, Lambda remain unchanged during    *
   * transitions, but that can be easily modified in case  *
   * we need dynamically-loaded code.                      *)
  let rec eval_stmt ({pred=pred; delta=delta; lambda=lambda; pc=pc} as ctx) stmt =
    let get_label e =
      let v = eval_expr delta e in
	match lab_of_exp (symb_to_exp v) with
	  | None -> failwith ("not a valid label "
                              ^(Pp.ast_exp_to_string (symb_to_exp v)))
	  | Some lab -> label_decode lambda lab
    in
    let next_pc = Int64.succ pc in
    let eval = function
      | Move (v,e,_) ->
	  let ev = eval_expr delta e in
	  [assign v ev ctx]
      | Halt (e, _) ->
	  let e = eval_expr delta e in
	    raise (Halted(Some e, ctx))
      | Jmp (e,_) ->
          [{ctx with pc=get_label e}] ;
      | CJmp (cond,e1,e2,_) ->
          (match eval_expr delta cond with
             | v when is_symbolic v ->
		 (* update the path predicate *)
		 let constr = symb_to_exp v in
		 let pred1' = add_constraint pred constr Equal in
		 let neg_constr = UnOp (NOT,constr) in
		 let pred2' = add_constraint pred neg_constr Equal in
		   (* return two new possible states *)
		   [{ctx with pred=pred1'; pc=get_label e1};
		    {ctx with pred=pred2'; delta=copy delta; pc=get_label e2}]
             | v when is_true_val v ->
		 [{ctx with pc=get_label e1}]
             | v when is_false_val v ->
		 [{ctx with pc=get_label e2}]
             | v -> failwith ("not a boolean condition: " 
                              ^ (Pp.ast_exp_to_string (symb_to_exp v)))
          )
      | Assert (e,_) ->
          (match eval_expr delta e with
             | v when is_symbolic v ->
	       let constr = symb_to_exp v in
	       let pred' = add_constraint pred constr Equal in
	       (*pdebug("Adding assertion: " ^ (Pp.ast_exp_to_string pred')) ;*)
	       [{ctx with pred=pred'; pc=next_pc}]
             | v when is_false_val v ->
               let pred = add_constraint pred exp_false Equal in
	       raise (AssertFailed({ctx with pred = pred}))
             | _ -> [{ctx with pc=next_pc}]
          )
      | Assume (e,_) ->
        (match eval_expr delta e with
        | v when is_true_val v -> [{ctx with pc=next_pc}]
        | v when is_false_val v ->
          raise (AssumptionFailed(ctx))
        | _ -> failwith "Symbolic assumptions are not supported by the symbolic evaluator")
      | Comment _ | Label _ ->
          [{ctx with pc=next_pc}]
      | Special _ as s -> 
          failwith ("Specials not handled yet: "^(Pp.ast_stmt_to_string s))
    in
      eval stmt

(* Performs one evaluation step on the program and returns *
 * a list of all possible follow-up states.                *)
  let eval state =
    try
      let stmt = inst_fetch state.sigma state.pc in
      dprintf "Executing %s" (Pp.ast_stmt_to_string stmt);
      if debug () then (print_values state.delta;
                        print_mem state.delta);
      eval_stmt state stmt
    with Failure str ->
      (prerr_endline ("Evaluation aborted at stmt No-"
                      ^(Int64.to_string state.pc)
                      ^"\nreason: "^str);
       if debug () then (print_values state.delta;
                         print_mem state.delta);
       (* print_endline ("Path predicate: "^(Pp.ast_exp_to_string (output_formula state.pred))); *)
       [])
      | Not_found ->
        (prerr_endline ("Evaluation aborted at stmt No-"
                        ^(Int64.to_string state.pc)
                        ^"\nreason: "^(Printf.sprintf "PC not found: %#Lx" state.pc));
         if debug () then (print_values state.delta;
                           print_mem state.delta);
         (* print_endline ("Path predicate: "^(Pp.ast_exp_to_string (output_formula state.pred))); *)
         [])

  (** Evaluate as long as there is exactly one choice of state.

      @param step This function is called with the evaluator's state
      for each transition.
  *)
  let eval_straightline ?(step = Util.id) state =
    let rec f state =
      match eval state with
      | newstate::[] -> f (step newstate)
      | states -> states
    in
    f state

end

(* Use a hash table as the table *)
module MemVHBackEnd =
struct
  type t = varval VH.t

  let copy delta = VH.copy delta
  let clear delta = VH.clear delta; delta
  let create () = VH.create 5000
  let fold delta f i = VH.fold f delta i

  (** Number of variable locations stored in state *)
  let num_values delta =
    VH.length delta

  (** Number of concrete memory locations stored in state *)
  let num_mem_locs delta =
    (** Number of bindings in map

	XXX: This is inefficient; switch to BatMaps which support cardinality
    *)
    let map_length m =
      AddrMap.fold (fun _ _ c -> c+1) m 0
    in
    VH.fold
      (fun k v count  ->
	 match k,v with
	   | var, ConcreteMem(mem,_) ->
             count + (map_length mem)
	   | _ -> count
      ) delta 0

  let find_var = VH.find

  let update_var a b c =
    VH.replace a b c; a

  let remove_var delta var =
    VH.remove delta var; delta
end

module MemVMBackEnd =
struct
  type t = varval VM.t

  let copy delta = delta
  let create () = VM.empty
  let clear delta = create ()

  let fold delta f i = VM.fold f delta i

  (** Number of variable locations stored in state *)
  let num_values delta =
    VM.fold (fun _ _ n -> n+1) delta 0

  (** Number of concrete memory locations stored in state *)
  let num_mem_locs delta =
    (** Number of bindings in map

	XXX: This is inefficient; switch to BatMaps which support cardinality
    *)
    let map_length m =
      AddrMap.fold (fun _ _ c -> c+1) m 0
    in
    VM.fold
      (fun k v count  ->
	 match k,v with
	   | var, ConcreteMem(mem,_) ->
             count + (map_length mem)
	   | _ -> count
      ) delta 0

  let find_var a b =
    VM.find b a

  let update_var a b c =
    VM.add b c a

  let remove_var delta var =
    VM.remove var delta
end

module type MemBackEnd =
sig
  type t
  val copy : t -> t
  val clear : t -> t
  val create : unit -> t
  val fold : t -> (var -> varval -> 'acc -> 'acc) -> 'acc -> 'acc
  val num_values : t -> int
  val find_var : t -> var -> varval
  val update_var : t -> var -> varval -> t
  val remove_var : t -> var -> t
end

module type Foldable =
sig
  type t
  val fold : t -> (var -> varval -> 'acc -> 'acc) -> 'acc -> 'acc
end

module BuildMemLPrinters(F:Foldable) =
struct
  let print_values delta =
    print_endline "contents of variables" ;
    F.fold
      delta
      (fun k v () ->
  	 match k,v with
  	   | var,Symbolic e ->
               print_endline ((Pp.var_to_string var) ^ " = " ^ (Pp.ast_exp_to_string e))
  	   | _ -> ()
      ) ()

  let print_mem delta =
    print_endline "contents of memories" ;
    F.fold
      delta
      (fun k v () ->
  	 match k,v with
  	   | var, ConcreteMem(mem,_) ->
               print_endline ("memory " ^ (Var.name var)) ;
               AddrMap.iter
  		 (fun i v ->
  		    print_endline((Printf.sprintf "%Lx" i)
  			   ^ " -> " ^ (Pp.ast_exp_to_string v))
  		 )
  		 mem
  	   | _ -> ()
      ) ()

  let print_var delta name =
    F.fold
      delta
      (fun var exp () ->
  	 match exp with
  	   | Symbolic e ->
  	       let varname = Var.name var in
  		 if varname = name then
  		   print_endline (varname ^ " = "
  			   ^ (Pp.ast_exp_to_string e))
  	   | _ -> ()
      ) ()
end

module BuildSymbolicMemL(BE:MemBackEnd) =
struct

  include BE

  let lookup_var delta var =
    try find_var delta var
    with Not_found ->
      match Var.typ var with
	| TMem _
	| Array _ ->
	    empty_mem var
	| Reg _ ->
	    Symbolic(Var var)

  let rec update_mem mu pos value endian =
    (*pdebug "Update mem" ;*)
    match mu with
      | Symbolic m -> Symbolic (Store(m,pos,value,endian,reg_8))
      | ConcreteMem (m,v) ->
	  match pos with
	    | Int(p,t) ->
		ConcreteMem(AddrMap.add (normalize p t) value m, v)
	    | _ -> update_mem (conc2symb m v) pos value endian

  let rec lookup_mem mu index endian =
    (*pdebug "Lookup mem" ;*)
    match mu, index with
      | ConcreteMem(m,v), Int(i,t) ->
	  (try AddrMap.find (normalize i t) m
	   with Not_found ->
	     Load(Var v, index, endian, reg_8)
	       (* FIXME: handle endian and type? *)
	  )
	    (* perhaps we should introduce a symbolic variable *)
      | Symbolic mem, _ -> Load (mem,index,endian,reg_8)
      | ConcreteMem(m,v),_ -> lookup_mem (conc2symb m v) index endian

  include BuildMemLPrinters(BE)

end

module SymbolicMemL = BuildSymbolicMemL(MemVHBackEnd)

module BuildConcreteMemL(BE:MemBackEnd) =
struct

  include BE

  let lookup_var delta var =
    try find_var delta var
    with Not_found ->
      match Var.typ var with
	| TMem _
	| Array _ ->
	    empty_mem var
	| Reg n as t ->
	    Symbolic(Int(bi0, t))

  let rec update_mem mu pos value endian =
    (*pdebug "Update mem" ;*)
    match mu, pos with
      | ConcreteMem (m,v), Int(p,t) ->
	  ConcreteMem(AddrMap.add (normalize p t) value m, v)
      | _ -> failwith "Symbolic memory in concrete evaluation"

  let rec lookup_mem mu index endian =
    (*pdebug "Lookup mem" ;*)
    match mu, index with
      | ConcreteMem(m,v), Int(i,t) ->
	  (try AddrMap.find (normalize i t) m
	   with Not_found ->
             failwith (Printf.sprintf "Uninitialized memory found at %s" (Pp.ast_exp_to_string index))
	  )
      | _ -> failwith "Symbolic memory or address in concrete evaluation"

  include BuildMemLPrinters(BE)
end

module ConcreteMemL = BuildConcreteMemL(MemVHBackEnd)

(* This concrete module will return zero for unknown memory locations,
   rather than raising an exception *)
module BuildConcreteUnknownZeroMemL(BE:MemBackEnd) =
struct
  include BuildConcreteMemL(BE)

  let rec lookup_mem mu index endian =
    (*pdebug "Lookup mem" ;*)
    match mu, index with
    | ConcreteMem(m,v), Int(i,t) ->
      (try AddrMap.find (normalize i t) m
       with Not_found ->
         wprintf "Uninitialized memory found at %s" (Pp.ast_exp_to_string index);
         Int(bi0, reg_32)
      )
    | _ -> failwith "Symbolic memory or address in concrete evaluation"
end

module ConcreteUnknownZeroMemL = BuildConcreteUnknownZeroMemL(MemVHBackEnd)

(** Symbolic assigns are represented as Lets in the formula, except
    for temporaries.  If you use this, you should clear out temporaries
    after executing each instruction. *)
module PredAssign(MemL: MemLookup)(Form: FlexibleFormula) =
struct
  let assign v ev ({delta=delta; pred=pred; pc=pc} as ctx) =
    let expr = symb_to_exp ev in
    let is_worth_storing = (*is_concrete expr &&*)
      Disasm.is_temp v
    in
    let delta', pred' =
      if is_worth_storing then (dprintf "Storing %s in delta" (Var.name v);
                                (MemL.update_var delta v ev, pred))
      else
        let constr = BinOp (EQ, Var v, expr) in
        pdebug ((Var.name v) ^ " = " ^ (Pp.ast_exp_to_string expr)) ;
        (* shouldn't matter because of dsa, but remove any old version anyway *)
        let delta' = MemL.remove_var delta v in 
        (delta', Form.add_to_formula pred constr Rename)
    in
    {ctx with delta=delta'; pred=pred'; pc=Int64.succ pc}
end

(** Symbolic assigns are represented in delta *)
module StdAssign(MemL:MemLookup)(Form:FlexibleFormula) =
struct
  open MemL
  let assign v ev ({delta=delta; pc=pc} as ctx) =
    {ctx with delta=update_var delta v ev; pc=Int64.succ pc}
end

module DontEvalSymbLet =
struct
  let eval_symb_let = false
end

module EvalSymbLet =
struct
  let eval_symb_let = true
end

(** Just build a straightforward expression; does not use Lets *)
module StdForm =
struct
  type t = Ast.exp
  type init = unit
  type output = Ast.exp

  let init () = exp_true

  let add_to_formula formula expression _type =
    BinOp(AND, expression, formula)

  let output_formula e = e
end

(** Uses Lets for assignments, continuation style. *)
module LetBind =
struct
  type t = (Ast.exp -> Ast.exp)
  type init = unit
  type output = Ast.exp

  let init () = (fun e -> e)

  let add_to_formula fbuild expression typ =
    (match expression, typ with
     | _, Equal ->
	 (fun newe -> fbuild (BinOp(AND, expression, newe)))
     | BinOp(EQ, Var v, value), Rename ->
	 (fun newe -> fbuild (Let(v, value, newe)))
     | _ -> failwith "internal error: adding malformed constraint to formula"
    )

  let output_formula bindings = bindings exp_true
end

(** Uses Lets for assignments *)
module LetBindOld =
struct
  type f = And of Ast.exp | Let of (Var.t * Ast.exp)
  type t = f list
  type init = unit
  type output = Ast.exp

  let init () = []

  let add_to_formula bindings expression typ =
    (match expression, typ with
      | _, Equal ->
          (And expression) :: bindings
      | BinOp(EQ, Var v, value), Rename ->
          (Let (v,value)) :: bindings
   | _ -> failwith "internal error: adding malformed constraint to formula"
    )

  let output_formula bindings =
    let rec create_formula acc = function
      | [] -> acc
      | (And e1)::rest ->
          let acc = BinOp(AND, e1, acc) in
            create_formula acc rest
      | (Let (v,e))::rest ->
          let acc = Ast.Let(v, e, acc) in
            create_formula acc rest
    in
      create_formula exp_true bindings
end

module FlexibleFormulaConverterToStream(Form:FlexibleFormula with type init=unit with type output = Ast.exp) = 
struct
  type fp = Formulap.fpp_oc
  type t = {printer : Formulap.fpp_oc; form_t : Form.t}
  type init = Formulap.fpp_oc
  type output = unit

  let init printer = {printer=printer; form_t=Form.init ()}

  let add_to_formula ({printer=printer; form_t=form_t} as record) exp typ = 
    {record with form_t = Form.add_to_formula form_t exp typ}

  let output_formula {printer=printer; form_t=form_t} =
    let formula = Form.output_formula form_t in
    let mem_hash = Memory2array.create_state () in
    let formula = Memory2array.coerce_exp_state mem_hash formula in
    let foralls = [] in
    let () = printer#assert_ast_exp ~foralls formula in
    let () = printer#counterexample in
    printer#close
end

module StdFormFakeStream = FlexibleFormulaConverterToStream(StdForm)
module LetBindFakeStream = FlexibleFormulaConverterToStream(LetBind)
module LetBindOldFakeStream = FlexibleFormulaConverterToStream(LetBindOld)

(** Print let formulas in a streaming fashion.  For example, given a trace:
    1. x = 5
    2. assert (x = 5)
    3. y = 10

    The formula (for smtlib) would look like:
    (and
    (let x = 5 in
    (x = 5)
    (let y = 10 in
    (true))))
*)
module LetBindStreamSat =
struct
  type t = {formula_printer : Formulap.stream_fpp_oc;
            formula_filename : string;
            free_var_printer : Formulap.stream_fpp_oc;
            free_var_filename : string;
            close_funs : (unit -> unit) Stack.t;
            mutable free_vars : Var.VarSet.t;
            mutable defined_vars : Var.VarSet.t}
  type init = string * Formulap.stream_fppf
  type output = unit

  let init (filename,(make_printer:Formulap.stream_fppf)) =
    (* Create and start a stack of functions to close parens of operations *)
    let close_funs = Stack.create () in
    let free_var_printer = make_printer (open_out filename) in
    let tempfilename, tempoc = Filename.open_temp_file "letbindstream" "tmp" in
    let formula_printer = make_printer tempoc in
    free_var_printer#open_stream_benchmark;
    Stack.push (fun () -> formula_printer#close_benchmark) close_funs;
    Stack.push (fun () -> formula_printer#counterexample) close_funs;
    formula_printer#assert_ast_exp_begin ();
    Stack.push (fun () -> formula_printer#assert_ast_exp_end) close_funs;
    formula_printer#and_start;
    {formula_printer=formula_printer; free_var_printer=free_var_printer;
     formula_filename=tempfilename; free_var_filename=filename;
     close_funs=close_funs;
     free_vars=Var.VarSet.empty; defined_vars=Var.VarSet.empty}

  let add_to_formula ({formula_printer=formula_printer;
                       free_var_printer=free_var_printer;
                       close_funs=close_funs} as record) expression typ =
    (match expression, typ with
      | _, Equal ->
        formula_printer#and_constraint expression;
        Stack.push (fun () -> formula_printer#and_close_constraint) close_funs;
        record
      | BinOp(EQ, Var v, value), Rename ->
        let fp_list = Formulap.freevars value in
        let fp =
          List.fold_left (fun s e -> Var.VarSet.add e s) Var.VarSet.empty fp_list
        in
        record.defined_vars <- Var.VarSet.add v record.defined_vars;
        let fp = Var.VarSet.diff fp record.defined_vars in
        Var.VarSet.iter formula_printer#predeclare_free_var fp;
        Var.VarSet.iter free_var_printer#predeclare_free_var fp;
        record.free_vars <- Var.VarSet.union record.free_vars fp;
	formula_printer#let_begin v value;
        Stack.push (fun () -> formula_printer#let_end v) close_funs;
        {record with close_funs=close_funs}
      | _ -> failwith "internal error: adding malformed constraint to formula"
    )

  let output_formula {formula_printer; free_var_printer;
                      formula_filename; free_var_filename;
                      close_funs; free_vars} =
    formula_printer#and_end;
    Stack.iter (fun f -> f()) close_funs;
    (* Free vars go at the begining of the file but we don't know all of them
       until the end of the trace.  So we print them out to a seperate file
       and combine these at the end. *)
    Var.VarSet.iter free_var_printer#print_free_var free_vars;
    formula_printer#flush; formula_printer#close;
    free_var_printer#flush; free_var_printer#close;
    Hacks.append_file formula_filename free_var_filename
end


module Symbolic = Make(SymbolicMemL)(DontEvalSymbLet)(StdAssign)(StdForm)
module SymbolicSlowMap = Make(BuildSymbolicMemL(MemVMBackEnd))(EvalSymbLet)(StdAssign)(StdForm)
module SymbolicSlow = Make(SymbolicMemL)(EvalSymbLet)(StdAssign)(StdForm)

module Concrete = Make(ConcreteUnknownZeroMemL)(EvalSymbLet)(StdAssign)(StdForm)

(** Execute a program concretely *)
let concretely_execute ?s ?(i=[]) p =
  let rec step ctx =
    let s = try Concrete.inst_fetch ctx.sigma ctx.pc
      with Not_found ->
        failwith (Printf.sprintf "Fetching instruction %#Lx failed; you probably need to add a halt to the end of your program" ctx.pc)
    in
    dprintf "Executing: %s" (Pp.ast_stmt_to_string s);
    let nextctxs, haltvalue, finished = try Concrete.eval ctx, None, false with
        Concrete.Halted (v, ctx) -> [ctx], v, true
    in
    match finished, nextctxs with
    | true, [c] -> c, haltvalue
    | false, [next] -> step next
    | _, _ -> failwith "step"
  in
  let ctx = Concrete.build_default_context p () in
  (* Evaluate initialization statements *)
  let ctx = List.fold_left (fun ctx s ->
			      dprintf "Init %s" (Pp.ast_stmt_to_string s);
			      match Concrete.eval_stmt ctx s with
			      | nctx::[] -> nctx
			      | _ -> failwith "Expected one context"
			   ) ctx i
  in
  let ctx = match s with
    | Some(s) -> {ctx with pc = Concrete.label_decode ctx.lambda (Addr s)}
    | None ->
      (* Explicitly set pc to 0, since executing any init statements
         will (unintentionally) increment pc. *)
      {ctx with pc = 0L}
  in
  let ctx, v = step ctx in
  Concrete.print_values ctx.delta;
  Concrete.print_mem ctx.delta;
  (match v with
  | Some(Symbolic v) -> Printf.printf "result: %s\n" (Pp.ast_exp_to_string v)
  | _ -> Printf.printf "no result\n");
  ctx

let eval_expr = Symbolic.eval_expr

(*
================================================================================
= Forward symbolic exec. 
================================================================================
*)

let new_unk_byte = 
  let unk_counter = ref 0 in
  (fun () -> 
    let n = !unk_counter in
    if n = -1 then failwith "new_unk_byte: counter wrapped around";
    unk_counter := n+1;
    let idx = string_of_int n in
    Ast.Var(Var.newvar ("unk_" ^ idx) Ast.reg_8)
  )

module AlwaysEvalLet =
struct
  let eval_symb_let = true
end
(** Deprecated name *)
module SlowEval = AlwaysEvalLet

module type SnapMemLookup =
sig

  exception MemSymbolicWrite
  (* FIXME: we need to include all of this? *)
  (** Lookup type *)
  type t

  (* Set the snapshot reading function. This is ugly. *)
  val init_read_byte : (int64 -> int) -> unit
  (** Initial lookup table *)
  val create : unit -> t
  (** Clear the lookup table *)
  val clear : t -> unit
  (** Deep copy the lookup table *)
  val copy : t -> t
  (** Print vars *)
  val print_values : t -> unit
  (** Print memories *)
  val print_mem : t -> unit

  (** Look up the value of a variable *)
  val lookup_var : t -> Var.t -> varval
  (** Update the value of a variable. *)
  val update_var : t -> Var.t -> varval -> t
  (** Remove the value for a variable. *)
  val remove_var : t -> Var.t -> t
  (** Update memory *)
  val update_mem : varval -> Ast.exp -> Ast.exp -> Ast.exp -> varval
  (** Lookup memory *)
  val lookup_mem : varval -> Ast.exp -> Ast.exp -> Ast.exp

end

(* Use a hash table as the table *)
module MemVHBackEnd' =
struct
  type t = varval VH.t

  let copy delta = VH.copy delta
  let clear delta = VH.clear delta
  let create () = VH.create 5000

  let print_values delta =
    pdebug "contents of variables" ;
    VH.iter
      (fun k v ->
  	 match k,v with
  	   | var,Symbolic e ->
               pdebug ((Pp.var_to_string var) ^ " = " ^ (Pp.ast_exp_to_string e))
  	   | _ -> ()
      ) delta

  let print_mem delta =
    pdebug "contents of memories" ;
    VH.iter
      (fun k v ->
  	 match k,v with
  	   | var, ConcreteMem(mem,_) ->
               pdebug ("memory " ^ (Var.name var)) ;
               AddrMap.iter
  		 (fun i v ->
  		    pdebug((Printf.sprintf "%Lx" i)
  			   ^ " -> " ^ (Pp.ast_exp_to_string v))
  		 )
  		 mem
  	   | _ -> ()
      ) delta

  let print_var delta name =
    VH.iter
      (fun var exp ->
  	 match exp with
  	   | Symbolic e ->
  	       let varname = Var.name var in
  		 if varname = name then
  		   pdebug (varname ^ " = "
  			   ^ (Pp.ast_exp_to_string e))
  	   | _ -> ()
      ) delta

  (** Number of variable locations stored in state *)
  let num_values delta =
    VH.length delta

  (** Number of concrete memory locations stored in state *)
  let num_mem_locs delta =
    (** Number of bindings in map

	XXX: This is inefficient; switch to BatMaps which support cardinality
    *)
    let map_length m =
      AddrMap.fold (fun _ _ c -> c+1) m 0
    in
    VH.fold
      (fun k v count  ->
	 match k,v with
	   | var, ConcreteMem(mem,_) ->
             count + (map_length mem)
	   | _ -> count
      ) delta 0

  let update_var a b c = VH.replace a b c; a

  let remove_var delta var = VH.remove delta var; delta
end

(** Uses Lets for assignments, continuation style. *)
module LetBind' =
struct
  type t = (Ast.exp -> Ast.exp)

  let true_formula = (fun e -> e)

  let add_to_formula fbuild expression typ =
    (match expression, typ with
     | _, Equal ->
	 (fun newe -> fbuild (BinOp(AND, expression, newe)))
     | BinOp(EQ, Var v, value), Rename ->
	 (fun newe -> fbuild (Let(v, value, newe)))
     | _ -> failwith "internal error: adding malformed constraint to formula"
    )

  let output_formula bindings = bindings exp_true
end

module SnapSymbolicMemL =
struct

  include MemVHBackEnd'
  module SM = SymbolicMemL

  exception MemSymbolicWrite

  (* FIXME: ;_; *)
  let read_byte = ref (fun x->1)
  
  let init_read_byte f = 
    read_byte := f
  
  let read_typed va t = 
    let rd i = 
      let i = Int64.of_int i in
      !read_byte (Int64.add va i)
    in
    let n = match t with
      | Type.Reg 8 -> 1
      | Type.Reg 16 -> 2
      | Type.Reg 32 -> 4
      | _ -> failwith "read_typed: unsupported type"
    in
    let res = ref 0L in
    for i = 0 to (n-1) do
      let byte = rd i in
      res := Int64.logor !res (Int64.shift_left (Int64.of_int byte) (8*i))
    done;
    (* let _ = Printf.printf "read_typed, va=%Lx, res=%Lx\n" va !res in *)
    Big_int_Z.big_int_of_int64 !res

  let lookup_var delta var =
    try VH.find delta var
    with Not_found ->
      match Var.typ var with
      | TMem _
      | Array _ -> empty_mem var
      | Reg _ -> Symbolic(Var var)

  let conc2symb memory v = conc2symb

  let rec update_mem mu pos value endian =
    match mu with
    | Symbolic m -> failwith "update_mem: symbolic mem. not allowed"
    | ConcreteMem (m,v) ->
      match pos with
	    | Int(p,t) -> ConcreteMem(AddrMap.add (normalize p t) value m, v)
	    | _ -> 
        raise MemSymbolicWrite

  (* All reads are split into byte reads. *)
  let rec lookup_mem mu index endian =
    match mu, index with
    | ConcreteMem(m,v), Int(i, t) ->
      let norm_addr = (normalize i t) in
      (try AddrMap.find norm_addr m
       with Not_found ->
        (try 
          let b = read_typed norm_addr Ast.reg_8 in
          Ast.Int(b, Ast.reg_8)
        (* FIXME?: we return new var on each read. Do we want to remember which
         * unknown was read from where? *)
        with Not_found -> new_unk_byte ()
        )
      )
    | Symbolic mem, _ -> failwith "lookup_mem: symbolic mem"
    | ConcreteMem(m,v),_ -> new_unk_byte ()
end

module type Formula =
sig
  type t
  val true_formula : t
  val add_to_formula : t -> Ast.exp -> form_type -> t
    (* FIXME *)
  val output_formula : t -> Ast.exp
end

module type SnapAssign =
  functor (MemL:SnapMemLookup) ->
    functor (Form:Formula) ->
sig
  (** Assign a variable. Does not modify the entire context in place. *)
  val assign : Var.t -> varval -> MemL.t -> Form.t -> MemL.t * Form.t
end

module type Fetch =
sig
  type t
  exception UnmappedVa

  val make_empty_ctx : unit -> t
  val fill_ctx : t -> (int64 -> int) -> t
  val inst_fetch : t -> int64 -> Ast.stmt list * int64

end

module SnapshotFetch =
struct
  
  module Pr = Printf

  type t = int64 -> int
  exception UnmappedVa

  let make_empty_ctx () = 
    let f va = 
      let _ = Pr.printf "Can't fetch byte @ %Lx" va in
      raise UnmappedVa
    in
    f

  let fill_ctx report_bad read_byte = 
    let f va =
      try read_byte va
      with Not_found -> report_bad va 
    in
    f

  let inst_fetch read_byte va = 
    (* let _ = Pr.printf "inst_fetch va=%Lx\n" va in *) 
    let f i = Char.chr (read_byte (Int64.add va (Int64.of_int i))) in
    let bytes = Array.init 16 f in
    let arch = Libbfd.Bfd_arch_i386 in
    let stmts, size = Asmir.byte_insn_to_bap arch va bytes in
    stmts, Int64.add va size

end

(*
 *************************************************************
 * Forward taint tracking
 *************************************************************
 *)
module Tracker =
struct
  
  open Ast
  open Big_int_convenience
  open Type
  module Dis = Disasm_i386 

  module D = Debug.Make(struct let name = "Tracker" and default=`NoDebug end)
  open D

  (* Represent a location, which is either a variable (register) or a
     memory address. *)
  module Loc = struct
    type t = V of Var.t | M of Big_int_Z.big_int

    let compare mv1 mv2 =
      match (mv1,mv2) with
      | ((V x),(V y)) -> compare x y
      | ((M x),(M y)) -> Big_int_Z.compare_big_int x y
      | ((V _),(M _)) -> -1
      | ((M _),(V _)) -> 1

    let to_string = function
      | V(v) -> Printf.sprintf "V(%s)" (Pp.var_to_string v)
      | M(i) -> Printf.sprintf "M[%s]" (~% i)
  end

  module LocSet = Set.Make(Loc)
  module VH = Var.VarHash

  let eFLAGS_LIST = [Dis.cf; Dis.pf; Dis.af; Dis.zf; Dis.sf; Dis.oF]
  let eFLAGS_LOCS = List.map (fun fl -> Loc.V fl) eFLAGS_LIST
  let eFLAGS_SET = List.fold_left (fun a fl -> LocSet.add fl a) LocSet.empty
                    eFLAGS_LOCS

  let print_locset vars =
    Printf.printf "  [+] Cardinality of Set: %d\n" (LocSet.cardinal vars);
    LocSet.iter (fun k -> match k with
    | Loc.V(x) -> Printf.printf "   [-] Var name: %s\n" (Pp.var_to_string x)
    | Loc.M(x) -> Printf.printf "   [-] Addr: %s\n" (Util.big_int_to_hex x)) vars

  type nice = TaintedWrite | TaintedJmp | Boring

  let empty = LocSet.empty

  (* Extract variables from exp. *)
  let extract_vars exp =
    let s = ref LocSet.empty in
    (* We don't care about tainted flags. *)
    let add l = 
      if LocSet.mem l eFLAGS_SET then ()
      else s := LocSet.add l !s 
    in
    let v = object(self)
      inherit Ast_visitor.nop
      method visit_rvar v = add (Loc.V v); DoChildren
      method visit_exp = function
        | _ -> DoChildren
    end in
    ignore(Ast_visitor.exp_accept v exp);
    !s

  let forward_taint eval_expr stmts locset =
    let flag = ref Boring in
    let vars = ref locset in
    let non_empty_inter ls = LocSet.inter !vars ls <> LocSet.empty in
    let has_common exp = non_empty_inter (extract_vars exp) in
    let exp2mloc exp = 
      let idx = eval_expr exp in 
      match idx with
      | Ast.Int(i,_) -> Some(Loc.M i);
      | _ -> None
    in
    let add_evaluated idxe vars =
      match exp2mloc idxe with
      | Some(mloc) -> LocSet.add mloc vars
      | None -> vars
    in
    (* We allow tainted indices. For x = arr[i], if i is tainted, then x is too.*)
    let extract_from_idx idxe =
      let s = extract_vars idxe in
      match exp2mloc idxe with
      | Some(mloc) -> LocSet.add mloc s
      | None -> s
    in
    let propagate stmt = 
      match stmt with
      | Move(_, Store(_,idxe,v,_,_), _) ->
        let _ = if has_common idxe then (flag := TaintedWrite) else () in
        if has_common v then (
          vars := add_evaluated idxe !vars;
        )
        else (
          match exp2mloc idxe with
          | Some(mloc) -> vars := LocSet.remove mloc !vars;
          | None -> ()
        )
      | Move(x, exp, _) ->
        let evars = 
          (match exp with
          | Load(_,idxe,_,_) -> extract_from_idx idxe 
          | Store(_,_,_,_,_) -> failwith "forward_taint: unexpected Store()"
          | _ -> 
            if LocSet.mem (Loc.V x) eFLAGS_SET then LocSet.empty
            else extract_vars exp
          ) 
        in
        if non_empty_inter evars then (
          vars := LocSet.add (Loc.V x) !vars;
        )
        else (
          vars := (LocSet.remove (Loc.V x) !vars);
        )
      | Jmp(e,_) -> if has_common e then (flag := TaintedJmp;) else () 
      | CJmp(_,et,ef,_) ->
          let hc e = has_common e in
          if (hc et) || (hc ef) then (flag := TaintedJmp;) else ()  
      | _ -> ();
    in
    List.iter (fun stmt ->
      (
      (* let fill = String.make depth ' ' in *)
      (* let _ = Printf.printf "%s st: %s\n" fill (Pp.ast_stmt_to_string stmt) in *)
      (* Modifies 'vars'. *)
      let _ = propagate stmt in
      (* let _ = print_locset !vars in *)
      ()
      );
    ) stmts;
    !flag, !vars


  (* Find yyy in first mov xxx, [yyy]. yyy is the initial taint source. *)
  let initial_taint t =
    let rec find_fault = function
      | (Move(_, Load(_,idxe,_,_), _) as mov)::tl -> 
        let _ = Printf.printf "stmt: %s\n" (Pp.ast_stmt_to_string mov) in
        extract_vars idxe
      | _::tl -> find_fault tl
      | [] -> failwith "Can't find initial taint."
    in
    let loc = find_fault t in
    print_locset loc;
    loc

end

module MakeForward(Fch: Fetch)(MemL: SnapMemLookup)(Tune: EvalTune)
                (Assign: SnapAssign) (Form: Formula) =
struct

  module L = List
  module Hsh = Hashtbl
  module Pr = Printf
  (* BAP *)
  module Ty = Type
  module Asm = Asmir
  module Utl = Util
  module Dis = Disasm_i386 (* Reg vars are defined here. *)
  module VH = Var.VarHash

  module Fch=Fch
  module MemL=MemL
  module Assign=Assign(MemL)(Form)
  module Form=Form
  module STPE = Solver.STPExec
  module M2a = Memory2array
  module Trk = Tracker (* FIXME: module param? *)

  (* Evaluator Contexts
     Symbol meanings:                              
   * Sigma: mapping the program counter to a stmt  
   * Lambda: mapping a label to a program counter  
   * Delta: mapping a variable to a value          
   * Pred: the path predicate                      
   * ip: instruction pointer
   * fctx: opaque context for offline inst fetcher 
   * taint: set of tainted vars and memory locations
   *)
  
  (* IP can be local to sigma (Pc), absolute (Va) or undefined (BadIp) *)
  type instruction_pointer = Pc of addr | Va of addr | BadIp
  (* map: var->dsa_var, rmap: dsa_var->var *)
  type dsa_info = {map: Var.t VH.t; rmap: Var.t VH.t}
  (* eval_stmt always returns two contexts for CJmps. Use this to quickly
   * decide which branch is satisfiable, without querying a solver. *)
  type was_taken = Taken | NotTaken

  type ('a,'b) ctx = {
    sigma: (addr, instr) Hashtbl.t;
    lambda: (label_kind, addr) Hashtbl.t;
    delta: 'a;
    pred: 'b;
    ip: instruction_pointer;
    dsa: dsa_info;
    fctx: Fch.t;
    taint: Trk.LocSet.t;
    taken: was_taken option;
  }


  let lookup_var        = MemL.lookup_var
  let update_var	      = MemL.update_var
  let update_mem        = MemL.update_mem
  let lookup_mem        = MemL.lookup_mem
  let assign            = Assign.assign
  let copy              = MemL.copy
  let print_values      = MemL.print_values
  let print_mem         = MemL.print_mem
  let eval_symb_let     = Tune.eval_symb_let
  let add_constraint    = Form.add_to_formula
  let output_formula    = Form.output_formula
  let is_sat            = STPE.is_satisfiable
  let mem2arr           = M2a.coerce_exp

  let byte_type = reg_8
  let index_type = reg_32

  let inc = Int64.succ 
  let dec = Int64.pred
  let add = Int64.add
  let i64 = Int64.of_int
  let add2 x = inc (inc x)

  type myctx = (MemL.t,Form.t) ctx

  (* Exceptions *)
  exception ExcState of string * addr
  (* Program halted, with optional halt value, and with given execution 
   * context. *)
  exception Halted of varval option * myctx
  (* An unknown label was found *)
  exception UnknownLabel of label_kind
  (* An assertion failed *)
  exception AssertFailed of myctx


  type nice = TaintedJump | TaintedWrite
  type inst = Inst of addr * Ast.stmt list 
  type err_reason = MaxBranch | MaxInst | Unsat | FalsePath | SymbLab | 
                    SymbWrite | BadEip
  type reason = Err of err_reason | Succ of nice
  type tree = Step of inst list * tree | Branch of inst list * tree * tree 
              | Leaf of reason 
  
  exception SymbolicLabel 
  exception Success of nice 

  let insts2str insts =
    let pred_asm = function Ty.Asm _ -> true | _ -> false in
    let unpack_asm = function 
        | Ty.Asm asm -> asm
        | _ -> failwith "unpack_asm: not a Type.Asm"
    in
    let lbl2asm = function
      | Ast.Label(_,atts) -> 
        (try 
          let asm = L.find pred_asm atts in unpack_asm asm
        with Not_found -> "Can't find @asm attr"
        )
      | _ -> "ERROR: not a label"
    in
    (* let pred_cmt = function Ast.Comment _ -> true | _ -> false in *)
    (* let to_str stmts = *) 
    (*   let f acc st = (Pp.ast_stmt_to_string st) :: acc in *)
    (*   let strs = L.rev (L.fold_left f [] stmts) in *)
    (*   let s = String.concat "\n" strs in *)
    (*   s *)
    (* in *)
    let pr acc = function
      | Inst(va, stmts) ->
        let asm = 
          try let st = L.hd stmts in lbl2asm st
          with Not_found -> "ERROR: can't find a label"
        in
        let vas = Pr.sprintf "0x%Lx" va in
        (vas, asm)::acc
    in
    let wrap_in_html pairs =
      let row acc (vas, asm) = 
        let t = "<tr><td align=\"left\">" ^ vas in
        let t = t ^ "</td><td align=\"left\">" ^ asm in
        let t = t ^ "</td></tr>" in
        t::acc
      in
      let pre = "<table border=\"0\" cellborder=\"0\" cellpadding=\"3\">" in
      let fin = "</table>" in
      let rows = L.fold_left row [pre] pairs in
      let rows = L.rev (fin::rows) in
      let table = String.concat "" rows in
      table
    in
    let pairs = L.rev (L.fold_left pr [] insts) in
    let html = wrap_in_html pairs in
    html
  
  let er2str = function
    | MaxBranch -> "Max branches"
    | MaxInst -> "Max instructions"
    | Unsat -> "Unsat path"
    | FalsePath -> "False path"
    | SymbLab -> "Symbolic jump"
    | SymbWrite -> "Untainted symb. write"
    | BadEip -> "Unmapped EIP"

  let nice2str = function
    | TaintedJump -> "Tainted jump"
    | TaintedWrite -> "Tainted write"

  let rsn2str = function
    | Err er -> er2str er
    | Succ(nice) -> nice2str nice

  let dump_stmts depth msg stmts = 
    let pre = String.make depth ' ' in
    let is_lbl = function
      | Ast.Label _ -> true
      | _ -> false
    in
    let pr stmt = 
      let s = Pp.ast_stmt_to_string stmt in
      let lbl = if is_lbl stmt then "(label)" else "()" in
      Pr.printf "%s %s %s: %s\n" pre msg lbl s
    in
    let _ = L.map pr stmts in
    ()

  (* Initializers *)
  let create_state () =
    let n = 1024 in
    let map : Var.t VH.t = VH.create n in
    let rmap : Var.t VH.t = VH.create n in
    let dsa = {map=map; rmap=rmap} 
    and sigma : (addr, instr) Hashtbl.t = Hashtbl.create n
    and ip = BadIp
    and delta : MemL.t = MemL.create ()
    and lambda : (label_kind, addr) Hashtbl.t = Hashtbl.create n 
    and fctx = Fch.make_empty_ctx () in
    {pred=Form.true_formula; delta=delta; sigma=sigma; lambda=lambda; 
    ip=ip; fctx=fctx; taint=Trk.LocSet.empty; dsa=dsa; taken=None}

  (* Evaluate an expression in a context Delta,Mu *)
  let rec eval_expr delta expr =
    (* Decide whether or not we should evaluate the memory, which can
       lead to exponential blow-up due to substitution. *)
    (* Evaluate an expression e, but only if the evaluation will be
       the same size as the unevaluated form (or smaller). Each
       expression type can assume that subexpression evaluation has
       the same property. *)
    let eval = function
      | Var v ->
        (* This can clearly result in a large symbolic result. Should we leave 
         * Vars? *)
        lookup_var delta v
      | Int _ as value ->
        Symbolic value
      | Lab _ as labl ->
        Symbolic labl
      | Ite(cond,e1,e2) ->
        let v1 = eval_expr delta e1
        and v2 = eval_expr delta e2 in
        (match eval_expr delta cond with
         | v when is_symbolic v ->
             Symbolic(Ite(symb_to_exp v, symb_to_exp v1, symb_to_exp v2))
         | v when is_true_val v ->
             Symbolic(symb_to_exp v1)
         | v when is_false_val v ->
             Symbolic(symb_to_exp v2)
         | _ ->
             failwith "not possible"
        )
      | Extract(h,l,e) ->
        let v = eval_expr delta e in
        if is_symbolic v then (
          Symbolic(Extract(h,l,symb_to_exp v))
        ) else (
          let (v,t) = Arithmetic.extract h l (concrete_val_tuple v) in
          Symbolic(Int(v,t))
        )
      | Concat(le,re) ->
        let lv = eval_expr delta le
        and rv = eval_expr delta re in
        if is_symbolic lv || is_symbolic rv then (
          Symbolic(Concat(symb_to_exp lv, symb_to_exp rv))
        ) else (
          let lv = concrete_val_tuple lv in
          let rv = concrete_val_tuple rv in
          let (v,t) = Arithmetic.concat lv rv in
          Symbolic(Int(v,t))
        )
      | BinOp (op,e1,e2) ->
        (* In the worst case, we will just combine two symbolic
           expressions. *)
        let v1 = eval_expr delta e1
        and v2 = eval_expr delta e2 in
        (match v1, v2 with
        | Symbolic (Int e1'), Symbolic (Int e2') ->
          (* We have two concrete scalars *)
          let (v,t) = (Arithmetic.binop op e1' e2') in
          Symbolic (Int(v,t))
        | ConcreteMem m1, ConcreteMem m2 ->
          (* We have two concrete memories. Note that this can
             only return bool. *)
          Symbolic (Int (mem_binop op m1 m2))
        | _ ->
            (* Something is symbolic *)
          let e1' = symb_to_exp v1
          and e2' = symb_to_exp v2 in
          Symbolic (BinOp (op, e1', e2')))
      | UnOp (op,e) ->
        (* In the worst case, we will have a symbolic expression. *)
        let v = eval_expr delta e in
          if is_symbolic v then
            Symbolic(UnOp(op, symb_to_exp v))
          else
            let e' = concrete_val_tuple v in
            let (v,t) = Arithmetic.unop op e' in
            Symbolic (Int (v,t))
      | Cast (ct,t,e) ->
        (* In the worst case, we will have a symbolic expression. *)
        let v = eval_expr delta e in
          if is_symbolic v then
            let e' = symb_to_exp v in
            Symbolic(Cast(ct, t, e'))
          else
            let e' = concrete_val_tuple v in
            let (n',t') = Arithmetic.cast ct e' t in
            Symbolic (Int (n',t'))
      | Let (var,e1,e2) as l ->
        (* Consider let v=e in e+e+e+e+e+e+e+e+e+e+e+e+e+e+e. If e
           is not concrete, this could lead to a huge blowup.

           So, if e is symbolic, we won't attempt to evaluate the expression
           further at all. *)
        let v1 = eval_expr delta e1 in
        if is_symbolic v1 && not eval_symb_let then 
          Symbolic(l)
        else
          let delta' = copy delta in (* FIXME: avoid copying *)
          let delta' = update_var delta' var v1 in
          let v2 = eval_expr delta' e2 in
          (* So, this is a little subtle.  Consider what happens if
             we have let x = 1 in let foo = freevar in x. We would
             evaluate let foo = freevar in x in the context where x
             is mapped to 1.  However, since freevar is a symbolic
             expression, we would not evaluate it further, and would
             return the evaluation expression let foo = freevar in
             x.  However, this is incorrect, because we are removing
             the Let binding for x!  We should really wrap any free
             variable with a Let binding to its current value in the
             context.

             Unfortunately, the way that lookup_var is implemented
             does not make it easy to know whether a variable is
             really defined or not.  (In traces, we return 0
             whenever we see an unknown variable, for instance.) So,
             as a stopgap measure, if var is free in v2, we return
             the original expression. *)
          (match v2 with
          | Symbolic v2' ->
            let fvars = Formulap.freevars v2' in
            let isvar = (fun v -> not (Var.equal v var)) in
            (* var is not free! We are good to go *)
            if List.for_all isvar fvars then v2
            (* var is still free; we can't use the evaluated version *)
            else Symbolic(l)
          | _ -> v2
          )
      | Load (mem,ind,endian,t) ->
        (match t with
        | Reg 8 ->
          (* This doesn't introduce any blowup on its own. *)
          let mem = eval_expr delta mem
          and ind = eval_expr delta ind
          and endian = eval_expr delta endian in
          let mem_arr = symb_to_exp ind
          and endian_exp = symb_to_exp endian in
          let r = lookup_mem mem mem_arr endian_exp in
          Symbolic (r)
        | Reg _ ->  (* we only care about 32bit *)
          (* Splitting introduces blowup.  Can we avoid it? *)
          eval_expr delta (Memory2array.split_loads mem ind t endian)
        | Array _ ->
          failwith ("loading array currently unsupported" ^ (Pp.typ_to_string t))
        | _ -> failwith "not a loadable type"
        )
      | Store (mem,ind,value,endian,t) ->
        let index = symb_to_exp (eval_expr delta ind)
        and value = symb_to_exp (eval_expr delta value)
        and endian = symb_to_exp (eval_expr delta endian) in
        (match t with
          (* No blowup here. *)
         | Reg 8 ->
           let mem = eval_expr delta mem in
           update_mem mem index value endian
         | Reg _ ->
           (* Splitting blowup, but I don't know how to avoid this. *)
           eval_expr delta (Memory2array.split_writes mem index t endian value)
         | Array _ ->
           failwith "storing array currently unsupported"
         | _ ->
           failwith "not a storable type"
        )
      | Unknown _ as u -> Symbolic u (*failwith "unknown value encountered"*)
    in
      eval expr

  (** 
   * Taken from traces.ml ;_;
   * Convert a stmt to DSA form

      @param s The AST program to convert to DSA form
      @param h The map from original var names to most recent dsa var
      @param rh The map from a dsa var back to the original var
      @return The DSA'ified statement

      @note Does not use DSA for 'mem'.
  *)
  let to_dsa_stmt s h rh =
    let dsa_ctr = ref 0 in
    let is_mem (Var.V(_,var,t)) =
      (String.length var >= 3) &&
        (String.sub var 0 3 = "mem") &&
      (match t with
      | TMem _
      | Array _ -> true
      | Reg _ -> false)
    in
    let is_symbolic (Var.V(_,s,_)) =
      try
        String.sub s 0 5 = "symb_"
      with _ -> false
    in
    let new_name (Var.V(_,s,t) as v) =
      if is_mem v then v
      else if is_symbolic v then v
      else (
        dsa_ctr := !dsa_ctr + 1;
        assert (!dsa_ctr <> 0);
        let s = Printf.sprintf "dsa_%s_%d" s !dsa_ctr in
        Var.newvar s t
        )
    in
    let replace_var v =
      let newv = new_name v in
      VH.add h v newv;
      VH.add rh newv v;
      newv
    in
    (* Rename all assigned vars *)
    let av = object(self)
      inherit Ast_visitor.nop
      method visit_avar v =
        ChangeTo (replace_var v)

      method visit_rvar v =
        try
          ChangeTo (VH.find h v)
        with Not_found ->
          dprintf "Unable to find %s during DSA: probably an input" (Var.name v);
          let nv = replace_var v in
          ChangeTo nv

      method visit_uvar v =
        VH.remove h v;
        DoChildren

    end
    in
    Ast_visitor.stmt_accept av s

  let cleanup_delta state =
    MemL.clear state

  let fetch_at_va fctx va = fst (Fch.inst_fetch fctx va) 
  let nxt_va fctx va = snd (Fch.inst_fetch fctx va) 
  
  (* Fetch from sigma. *)
  let inst_fetch sigma pc =
    try Hashtbl.find sigma pc
    with Not_found ->
      let s = Pr.sprintf "%Lx" pc in
      failwith ("Can't fetch @ pc=" ^ s)
  
  (* Deep context copy *)
  let clone ctx = 
    let sigma = Hsh.copy ctx.sigma in
    let lambda = Hsh.copy ctx.lambda in
    let delta = copy ctx.delta in
    (* XXX: No need to clone pred *)
    let pred = ctx.pred in 
    let dsa = {map=VH.copy ctx.dsa.map; rmap=VH.copy ctx.dsa.rmap} in
    (* No need to clone fctx, because it's global *)
    let nc = {pred=pred; delta=delta; sigma=sigma; lambda=lambda; ip=ctx.ip;
              fctx=ctx.fctx; taint=ctx.taint;dsa=dsa; taken=ctx.taken} in
    nc
  
  let fetch_ip sigma ip = 
    match ip with
    | Pc(pc) -> inst_fetch sigma pc
    | _ -> failwith "fetch_ip: bad ip type"

  let is_conc_mem = function
    | ConcreteMem _ -> true
    | _ -> false
  
  let taint_cast = function
    | Trk.Boring -> failwith "taint_cast: unexpected Boring flag"
    | Trk.TaintedJmp -> TaintedJump
    | Trk.TaintedWrite -> TaintedWrite
  
  let is_label = function
    | Ast.Label(_,_) -> true
    | _ -> false

  let rec eval_and_taint_stmt 
    ({dsa=dsa;taint=taint;sigma=sigma;pred=pred;delta=delta;ip=ip} as ctx) =
    (* Get label's ip. *)
    let swap_pc0_to_va ip lab = 
      match ip, lab with
      | Pc(0L), Addr addr -> Va(addr)
      | Pc(0L), _ -> failwith "swap_pc0_to_va: unexpected label type"
      | _, _ -> ip
    in
    let get_label ({lambda=lambda;delta=delta}) e =
      let label2ip lambda lab =
        try Pc(Hashtbl.find lambda lab)
        with Not_found -> 
          (match lab with
          | Addr addr -> Va(addr)
          | Name s -> failwith ("label2ip: can't decode " ^ s)
          )
      in
      let v = eval_expr delta e in
      match lab_of_exp (symb_to_exp v) with
      | None -> raise SymbolicLabel
      | Some lab -> 
        let ip' = label2ip lambda lab in
        swap_pc0_to_va ip' lab
    in
    let inc_ip = function
      | Pc(pc) -> 
          let pc' = inc pc in
          if Hashtbl.mem sigma pc' then Pc(pc') else BadIp
      | _ -> failwith "inc_ip: illegal ip type to increment"
    in
    (* Does ip point to last instruction in sigma and is this instruction a
     * label? If so, next ip should be BadIp, since executing a label has no
     * effect. This is necessary so that cjmps are not split into two nodes
     * in the resulting graph. *)
    let is_last_and_label ip = 
      match ip with
      | Pc(_) -> (
        let ip' = inc_ip ip in
        (match ip, ip' with
        | Pc(pc), BadIp -> 
            let stmt = inst_fetch sigma pc in
            is_label stmt
        | _, _ -> false
        )
      )
      | _ -> false
    in
    let fix_last_label ({ip=ip} as ctx) = 
      if is_last_and_label ip then {ctx with ip=BadIp} else ctx
    in
    let is_taken = function
      | v when is_true_val v -> Some(Taken), Some(NotTaken)
      | v when is_false_val v -> Some(NotTaken), Some(Taken)
      | _ -> None, None
    in
    let ip' = inc_ip ip in
    let eval = function
      | Move (v,e,_) ->
        let ev = eval_expr delta e in
        let (delta', pred') = assign v ev delta pred in
        [{ctx with delta=delta'; pred=pred'; ip=ip'}]
      | Halt (e, _) ->
        let e = eval_expr delta e in
        raise (Halted(Some e, ctx))
      | Jmp (e,_) ->
        let jmp_ip = get_label ctx e in
        [{ctx with ip=jmp_ip}]
      | CJmp (cond,e1,e2,_) ->
        (match eval_expr delta cond with
         (* Always return 2 states, we will try to eval cond at higher level
          * in forward_exec/run. *)
         | v when not (is_conc_mem v) ->
           (* update the path predicate *)
           let constr = symb_to_exp v in
           let pred1 = add_constraint pred constr Equal in
           let neg_constr = UnOp (NOT,constr) in
           let pred2 = add_constraint pred neg_constr Equal in
           (* return two new possible states *)
           let ip1 = get_label ctx e1 in
           let ip2 = get_label ctx e2 in
           let tkn1, tkn2 = is_taken v in
           [{ctx with pred=pred1; ip=ip1; taken=tkn1}; 
           {ctx with pred=pred2; ip=ip2; taken=tkn2}]
         | v -> failwith ("not a boolean condition: " ^ 
                 (Pp.ast_exp_to_string (symb_to_exp v)))
        )
      | Assert (e,_) ->
        (match eval_expr delta e with
           | v when is_symbolic v ->
             let constr = symb_to_exp v in
             let pred' = add_constraint pred constr Equal in
             (*pdebug("Adding assertion: " ^ (Pp.ast_exp_to_string pred')) ;*)
             [{ctx with pred=pred'; ip=ip'}]
           | v when is_false_val v ->
             raise (AssertFailed ctx)
           | _ -> [{ctx with ip=ip'}]
        )
      | Comment _ | Label _ ->
        [{ctx with ip=ip'}]
      | Special _ as s -> 
        failwith ("Specials not handled yet: "^(Pp.ast_stmt_to_string s))
      | Assume _ -> failwith "Symexec can't handle Assume()"
    in
    let pure_eval_expr exp = 
      let ev = eval_expr (copy ctx.delta) exp in
      symb_to_exp ev
    in 
    let stmt = fetch_ip sigma ip in
    let stmt = to_dsa_stmt stmt dsa.map dsa.rmap in
    (* let _ = dump_stmts 1 "ds:" [stmt] in *)
    (* let _ = print_values ctx.delta in *)
    (* Niceness flag, set of tainted vars/memory addresses. *)
    let nflag, taint = Trk.forward_taint pure_eval_expr [stmt] taint in
    let _ = if nflag <> Trk.Boring then raise (Success(taint_cast nflag)) 
            else () in
    let states = eval stmt in
    let states = L.map (fun ctx -> {ctx with taint=taint}) states in
    let states = L.map fix_last_label states in
    states

  let enumerate l =
    let (--) i j = 
      let rec aux n acc =
        if n < i then acc else aux (n-1) (n :: acc)
      in aux j [] 
    in
    let n = L.length l in
    let r = 0--(n-1) in
    L.combine l r 

  let forward_exec start_va reg_vals taint read_byte jd id =
    let make_block ({sigma=sigma;lambda=lambda;ip=ip} as ctx) stmts = 
      (* Setup fresh (sigma, lambda) pair. *)
      let set_sigma_lambda sigma lambda stmts =
        let unwrap_labels enu_stmts = 
          let pred = function
            | Ast.Label(lab,_), _ -> true
            | _, _ -> false
          in
          let unwrap = function
            | Ast.Label(lab,_) -> lab
            | _ -> failwith "unwrap: not possible"
          in
          let labs = L.filter pred enu_stmts in
          L.map (fun (s,pc) -> (unwrap s, pc)) labs
        in
        let upd h k v = Hsh.add h k v in
        let _ = Hsh.clear lambda in
        let _ = Hsh.clear sigma in
        (* let _ = dump_stmts 1 "stmts:" stmts in *)
        let enu_stmts = enumerate stmts in
        let enu_stmts = L.map (fun (s,i) -> (s, i64 i)) enu_stmts in
        let _ = L.map (fun (s,pc) -> upd sigma pc s) enu_stmts in
        let labels = unwrap_labels enu_stmts in
        let _ = L.map (fun (lab,pc) -> upd lambda lab pc) labels in
        (sigma, lambda)
      in
      let sigma, lambda = set_sigma_lambda sigma lambda stmts in
      {ctx with ip=Pc(0L);sigma=sigma;lambda=lambda;} 
    in
    let run_block ctx = 
      let rec run_sigma ctx = 
        let ctxs = eval_and_taint_stmt ctx in
        match ctxs with
        | [{ip=Pc(_)} as ctx] -> run_sigma ctx
        (* Bail on BadIp or Va address. We need to refill sigma in these cases*)
        | [{ip=_} as ctx] -> [ctx] 
        | [ctx1;ctx2] as ctxs -> ctxs
        | _ -> failwith "run_sigma: unexpected number of states"
      in
      match ctx.ip with
      | Pc(_) -> run_sigma ctx
      | _ -> failwith "run_sigma: invalid ip type"
    in
    let is_jmp stmts = 
      let pred = function 
        | Ast.Jmp _ -> true
        | _ -> false
      in
      try let _ = L.find pred stmts in true
      with Not_found -> false
    in
    let how_to_continue cur_va ({fctx=fctx;ip=ip}) =
      match ip with 
      (* Current VA is not changed, we continue to execute a piece of BIL
       * code with cjmps inside. *)
      | Pc(_) -> false, cur_va
      (* We either jumped to the beginning of looping BIL, or to a new
       * instruction. *)
      | Va(va) -> true, va 
      (* We executed all BIL instructions, time to fetch new VA. *)
      | BadIp -> true, nxt_va fctx cur_va 
    in
    let bb2step insts t = Step(L.rev insts, t) in
    let return_leaf insts lf = if insts=[] then lf else bb2step insts lf in
    (* insts - current block (inst list) *)
    (* FIXME: run just the BIL stmts, decouple BIL <-> asm mapping *)
    let rec run depth cur_va insts ctx jd id = 
      let jd_, id_ = jd-1, id-1 in
      if jd=0 then (
        Leaf(Err(MaxBranch))
      )
      else if id=0 then (
        return_leaf insts (Leaf(Err(MaxInst)))
      )
      else(
        (* We assume constructs like rep movsb jump to their beginnings after
         * each iteration (there are no 'inner' loops inside the lifted BIL) *)
        let need_new_block, cur_va = how_to_continue cur_va ctx in
        let stmts = fetch_at_va ctx.fctx cur_va in
        (* let _ = dump_stmts 1 ":" stmts in *)
        let inst = Inst(cur_va, stmts) in
        let insts = inst::insts in
        let ctx = 
          if need_new_block then ( make_block ctx stmts )
          else ( ctx ) 
        in
        try (
          let states = run_block ctx in
          match states with
          | [ctx] -> 
              let f_ret, insts =
                (if is_jmp stmts then (fun t -> bb2step insts t), []
                else Utl.id, insts)
              in
              let t = run depth cur_va insts ctx jd id_ in
              f_ret t
          | [ctx1;ctx2] ->
            (* Deep copy *)
            let run_cloned cur_va ctx jd id = 
              let run_it () = run (depth+1) cur_va [] (clone ctx) jd id  in
              match ctx.taken with
              | Some(Taken) -> run_it () 
              | Some(NotTaken) -> Leaf(Err(FalsePath))
              | None -> 
                let formula = output_formula ctx.pred in
                (* Convert TMems to arrays. *)
                let formula = mem2arr formula in
                if is_sat formula then run_it ()
                else (Leaf(Err(Unsat)))
            in
            let t = run_cloned cur_va ctx1 jd_ id_ in
            let f = run_cloned cur_va ctx2 jd_ id_ in
            Branch(L.rev insts, t, f)
          | _ -> failwith "run: impossible number of result states"
        )
        with 
        | Success(nice) -> 
          return_leaf insts (Leaf(Succ(nice)))
        | SymbolicLabel -> (* We can't decode a jmp's label *)
          return_leaf insts (Leaf(Err(SymbLab)))
        | MemL.MemSymbolicWrite ->
          return_leaf insts (Leaf(Err(SymbWrite)))
        | Fch.UnmappedVa ->
          return_leaf insts (Leaf(Err(BadEip)))
      )
    in
    let init_delta_pred delta pred var_vals = 
      let store (delta, pred) (var, value) =
        let delta, pred = assign var value delta pred in
        (delta, pred)
      in
      L.fold_left store (delta, pred) var_vals 
    in
    let to_dsa (Var.V(_,s,t) as v) map rmap =
      try VH.find map v
      with Not_found -> (
        let s = Pr.sprintf "initial_dsa_%s" s in
        let nv = Var.newvar s t in
        VH.add map v nv;
        VH.add rmap nv v;
        nv
      )
    in
    (* taint set contains registers that need to be converted to DSA, otherwise
     * taint tracking won't work. *)
    let dsa_taint f_conv taint =
      (* This will convert tainted vars to dsa. Memory addrs aren't touched.*)
      let dsa = function
        | Trk.Loc.V v -> Trk.Loc.V(f_conv v)
        | mv -> mv
      in
      let locs = Trk.LocSet.elements taint in
      let locs = L.map dsa locs in
      let add s e = Trk.LocSet.add e s in
      L.fold_left add Trk.LocSet.empty locs 
    in
    let _ = MemL.init_read_byte read_byte in
    let ctx = create_state () in
    let fctx = Fch.fill_ctx ctx.fctx read_byte in
    let ctx = {ctx with ip=Va(start_va);fctx=fctx} in

    (* Convert regs to DSA before storing them in delta *)
    let dsa_in_ctx r = to_dsa r ctx.dsa.map ctx.dsa.rmap in
    let reg_vals = L.map (fun (r,v) -> (dsa_in_ctx r, v)) reg_vals in
    let taint = dsa_taint dsa_in_ctx taint in

    let empty_mem = ConcreteMem(AddrMap.empty, Dis.mem) in
    let var_vals = (Dis.mem, empty_mem)::reg_vals in
    let delta, pred = init_delta_pred ctx.delta ctx.pred var_vals in
    let ctx = {ctx with delta=delta; pred=pred; taint=taint} in
    (* jd - jump depth, id - inst depth *)
    (* depth, insts, ctx, max jmp depth, max inst depth *)
    run 0 start_va [] ctx jd id

(* FIXME: move to a separate module. *)

    (* representation of a node -- must be hashable *)
    module Node = struct
       type t = int * tree
       let compare (id1, _) (id2, _) = Pervasives.compare id1 id2
       let hash (id, _) = Hashtbl.hash id
       let equal x y = (fst x) = (fst y)
    end

    (* representation of an edge -- must be comparable *)
    module Edge = struct
       type t = string
       let compare = Pervasives.compare
       let equal = (=)
       let default = ""

       type label = string
       let create vs lbl vd = (vs, lbl, vd)
       let label e = e
    end

    (* a functional/persistent graph *)
    module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

    (* more modules available, e.g. graph traversal with depth-first-search *)
    module D = Graph.Traverse.Dfs(G)

    (* module for creating dot-files *)
    module Dot = Graph.Graphviz.Dot(struct
       include G (* use the graph module from above *)
       (* Light blue for edges. *)
       let edge_attributes (a, e, b) = [`Label e; `Color 0x99ccff]
       let default_edge_attributes _ = []
       let get_subgraph _ = None
       let vertex_attributes (_, t) = 
         let txt, shape, fc = 
           match t with 
           | Step(insts, _)
           | Branch(insts, _, _) ->
              let txt = insts2str insts in
              let shape = `Record in
              let fc = 0xffffff in
              (txt, shape, fc)
           (* Light red for errors. *)
           | Leaf (Err(_) as r) -> (rsn2str r, `Ellipse, 0xff9999)
           (* Light green for success. *)
           | Leaf (Succ(_) as r) -> (rsn2str r, `Ellipse, 0x99ff99)
         in
         (* let txt = String.escaped txt in *) 
         [`Shape shape; `Label txt; `Fillcolor fc; `Fontname "Courier";
          `Style `Filled]

       let vertex_name (id,_) = string_of_int id
       let default_vertex_attributes _ = []
       let graph_attributes _ = []
    end)
  
   let to_dot root ofn = 
    let id = ref 0 in
    let pick_lbl r lbl = 
      let err = "error" in
      let scs = "success" in
      match r with
      | Leaf(Err(Unsat)) | Leaf(Err(FalsePath)) -> lbl ^ ", " ^ err
      | Leaf(Err(_)) -> err
      | Leaf(Succ(_)) -> scs
      | _ -> lbl
    in
    let rec aux g r =
      let v = (!id, r) in
      id:=!id+1;
      match r with
      | Step(insts, c) -> 
        let v',g = aux g c in
        let lbl = pick_lbl c "jmp" in
        let e = Edge.create v lbl v' in
        let g = G.add_edge_e g e in
        v, g
      | Branch(insts, t, f) -> 
        let vt,g = aux g t in
        let vf,g = aux g f in
        let lblt = pick_lbl t "true" in
        let lblf = pick_lbl f "false" in
        let et = Edge.create v lblt vt in
        let ef = Edge.create v lblf vf in
        let g = G.add_edge_e g et in
        let g = G.add_edge_e g ef in
        v, g
      | Leaf rsn -> 
        v, g
    in
    let g = G.empty in
    let _,g = aux g root in
    let file = open_out_bin ofn in
    let () = Dot.output_graph file g in
    let _ = close_out file in
    ()
end
