(*pp camlp4o pa_macro.cmo *)

(* A module to convert AST formulas to expressions for SMT Solvers.

    @author Thanassis Avgerinos, Sang Kil Cha, ejs
*)

open Ast
open Ast_convenience
open Big_int_convenience
open Printf
open Type
open Var

module D = Debug.Make(struct let name = "Solver" and default=`NoDebug end)
open D

exception Solver_error of string;;

let serror s = raise (Solver_error(s))

(* move to helpers *)
let byte_array_to_string ar =
  let bitlength = Array.length ar in
  let s = String.create bitlength in
  Array.iteri (fun i c -> s.[bitlength - i - 1] <- (char_of_int (c+0x30))) ar;
  s

(* get bit width from the Int exp *)
let get_bits = Typecheck.bits_of_width

let timeout = ref 0

(** The interface for defining a new solver *)
module type Solver_In =
  sig
    type set (** Solver context *)
    type exp (** Solver expression *)
    type model (** Solver model *)
    val name : string (** Name of solver *)
    val mk_set : unit -> set (** Function to create solver context *)
    val del_set : set -> unit (** Function to free solver context *)
    val convert : set -> Ast.exp -> exp (** Function to convert BAP expression to solver expression *)
    val add_binding : set -> Ast.var -> exp -> unit (** Function to add a [Ast.Let] binding *)
    val del_binding : set -> Ast.var -> unit (** Function to delete [Ast.Let] binding *)
    val add_constraint : set -> exp -> unit (** Function to add boolean constraint *)
    val is_sat : set -> bool (** [is_sat ctx] returns true iff [ctx] is satisfiable *)
    val get_model : set -> model (** Get a model that satisfies the current context *)
    val model_exp : set -> model -> Ast.exp -> Ast.exp (** [model_exp ctx model e] evaluates the BAP expression [e] in the satisfying model [model] *)
    val push : set -> unit (** [push ctx] saves the current context in [ctx] to the context stack *)
    val pop : set -> unit (** [pop ctx] pops the context stack and stores the result in [ctx] *)
    val pp_sol : set -> string (* What is this? *)
    val pp_exp : set -> exp -> string (** Convert a solver expresion to a string *)
  end

module type Solver_Out = sig
  type set
  val is_satisfiable : Ast.exp -> bool
  val is_valid : Ast.exp -> bool
  class solver :
  object
    val s : set
    method add_binding : Ast.var -> Ast.exp -> unit
    method add_constraint : Ast.exp -> unit
    method del_binding : Ast.var -> unit
    method is_sat : bool
    method is_valid : Ast.exp -> bool
    method model_exp : Ast.exp -> Ast.exp
    method pop : unit
    method pp_sol : string
    method push : unit
  end
  val newsolver : unit -> solver
end


module NullSolver = struct
  type set = unit
  type exp = unit
  type model = unit

  let name = "Null Solver"
  let abort _ = failwith "Solver Unimplemented"
  let mk_set = abort
  and del_set = abort
  and convert = abort
  and add_binding = abort
  and del_binding = abort
  and add_constraint = abort
  and is_sat = abort
  and get_model = abort
  and model_exp = abort
  and push = abort
  and pop = abort
  and pp_sol = abort
  and pp_exp = abort

end

(* A STP InterFace module *)
(*module SIF =
struct

  open Stpvc

  type context = vc

  type set = {
    ctx : context;
    (*vars : (var_decl * expr) VarHash.t;*)
  }

  type exp = Stpvc.exp

  let name = "STP"

  let mk_set () =
    let ctx = create_validity_checker () in
    {ctx = ctx}

  let del_set {ctx = ctx} =
    Libstp.vc_Destroy ctx

  (* Converting AST binop expression to STP expressions *)
  let convert_binop ctx e1 e2 typ = function
    | PLUS   -> e_bvplus ctx (get_bits typ) e1 e2
    | MINUS  -> e_bvminus ctx (get_bits typ) e1 e2
    | TIMES  -> e_bvmult ctx (get_bits typ) e1 e2
    | EQ     -> e_eq ctx e1 e2
    | _ -> failwith "unsupported"

  (* Converting AST binop expression to STP expressions *)
  let rec convert ({ctx=ctx} as set) = function
    | Var v ->
        let typ = bitvector_t ctx (get_bits (Var.typ v)) in
        let var = e_var ctx (Var.name v) typ in
          var
    | Int (n, t) ->
        e_bv_of_int64 ctx (get_bits t) n
    | BinOp (op, e1, e2) ->
        let typ = Typecheck.infer_ast e1 in
        let e1 = convert set e1 in
        let e2 = convert set e2 in
          convert_binop ctx e1 e2 typ op
    | _ -> failwith "unsupported expression"

  (*Printing an STP formula *)
  let pp_exp _ formula =
    dprintf "%s" (to_string formula)

  let add_constraint {ctx=ctx} = do_assert ctx

  let is_true {ctx = ctx} = query ctx (e_false ctx)

  (* Printing an STP solution *)
  let pp_sol {ctx=ctx} =
    match query ctx (e_false ctx) with
      | false ->
          dprintf "satisfiable";
          Libstp.vc_printCounterExample ctx;
          print_newline ()
      | true ->
          dprintf "unsatisfiable"

end
*)

module Make(S: Solver_In) =
struct

  type set = S.set

  (* let create_solver = S.mk_set *)
  (* let conv_formula = S.convert *)
  (* let add_constraint = S.add_constraint *)
  (* let pp_sol = S.pp_sol *)
  (* let is_true = S.is_true *)
  (* let destroy_solver = S.del_set *)

  let is_satisfiable formula =
    let ctx = S.mk_set () in
    let formula = S.convert ctx formula in
    dprintf "is_sat: %s" (S.pp_exp ctx formula); 
    S.add_constraint ctx formula;
    let answer = S.is_sat ctx in
    dprintf "result: %s" (S.pp_sol ctx); 
    S.del_set ctx;
    answer

  let is_valid formula =
    let ctx = S.mk_set () in
    (* (not formula) is satisfiable if and only if formula is
       not valid. *)
    let e = UnOp(NOT, formula) in
    (* dprintf "bap: %s" (Pp.ast_exp_to_string e); *)
    let formula = S.convert ctx e in
    (* dprintf "is_valid: %s" (S.pp_exp ctx formula); *)
    S.add_constraint ctx formula;
    let answer = S.is_sat ctx in
    (* dprintf "result: %s" (S.pp_sol ctx); (\* print out the solution *)
    S.del_set ctx;
    not answer

  class solver = object(self)
    (** Solver instance *)
    val s =
      (* Auto-destruct *)
      let s' = S.mk_set () in
      Gc.finalise (fun set ->
                      dprintf "Automatically deleting solver set";
                      S.del_set set) s';
      s'

    (* (\** Access solver info. *\) *)
    (* method debugs = s *)
    (* debugs makes subtyping break. *)

    (** Push a copy of the constraints onto the constraint stack *)
    method push =
      S.push s

    (** Restore the top of the constraint stack *)
    method pop =
      S.pop s

    (** Add a constraint *)
    method add_constraint c =
      S.add_constraint s (S.convert s c)

    (** Add a binding *)
    method add_binding v c =
      S.add_binding s v (S.convert s c)

    (** Remove a binding *)
    method del_binding v =
      S.del_binding s v

    (** Check if set of constraints is consistent/satisfiable *)
    method is_sat =
      S.is_sat s

  (** Check if the set of constraints imply that a formula is
      true (valid).

      In other words, check if the formula is valid under the set of
      constraints.*)
    method is_valid f =
      self#push;
      (* Assumptions => NOT f is unsatisfiable IFF Assumptions -> f is valid *)
      self#add_constraint (UnOp(NOT, f));
      let r = self#is_sat in
      self#pop;
      not r

    (** Find value of e in satisfying model *)
    method model_exp e =
      S.model_exp s (S.get_model s) e

    method pp_sol =
      S.pp_sol s
  end

  let newsolver () = new solver

end

IFDEF WITH_Z3_BINDINGS THEN

(* A Z3 InterFace module *)
module ZIF =
struct

  open Z3

  type exp = ast

  type varmap = exp VarHash.t

  type model = Z3.model

  type set = {
    ctx : context;
    mutable vars : varmap;
    vars_stack : varmap Stack.t;
  }

  let name = "Z3"

  let mk_set () =
    let cfg = mk_config () in
    set_param_value cfg "SOFT_TIMEOUT" (string_of_int !timeout);
      (*set_param_value cfg "MODEL_ON_TIMEOUT" "true";*)
    set_param_value cfg "MODEL" "true";
    let ctx = mk_context cfg in
    {ctx = ctx; vars=VarHash.create 256; vars_stack = Stack.create ()}

  let del_set {ctx=ctx; vars=vars; vars_stack=vs} =
    del_context ctx
    (* Stack.clear vs; *)
    (* VarHash.clear vars *)

  let bool_to_bv ctx cond invert =
    let booltyp = mk_bv_sort ctx 1 in
    let tr = mk_int ctx 1 booltyp in
    let fl = mk_int ctx 0 booltyp in
    if invert then mk_ite ctx cond tr fl
    else mk_ite ctx cond fl tr

  (* Converting AST binop expression to Z3 expressions *)
  let convert_binop ctx e1 e2 =
    function
    | PLUS    -> mk_bvadd ctx e1 e2
    | MINUS   -> mk_bvsub ctx e1 e2
    | TIMES   -> mk_bvmul ctx e1 e2
    | DIVIDE  -> mk_bvudiv ctx e1 e2
    | SDIVIDE -> mk_bvsdiv ctx e1 e2
    | MOD     -> mk_bvurem ctx e1 e2
    | SMOD    -> failwith "smod goes to bvsrem or bvsmod?"
    | LSHIFT  -> mk_bvshl ctx e1 e2
    | RSHIFT  -> mk_bvlshr ctx e1 e2
    | ARSHIFT -> mk_bvashr ctx e1 e2
    | AND     -> mk_bvand ctx e1 e2
    | OR      -> mk_bvor ctx e1 e2
    | XOR     -> mk_bvxor ctx e1 e2
    | EQ      -> bool_to_bv ctx (mk_eq ctx e1 e2) true
    | NEQ     -> bool_to_bv ctx (mk_eq ctx e1 e2) false
    | LT      -> bool_to_bv ctx (mk_bvult ctx e1 e2) true
    | LE      -> bool_to_bv ctx (mk_bvule ctx e1 e2) true
    | SLT     -> bool_to_bv ctx (mk_bvslt ctx e1 e2) true
    | SLE     -> bool_to_bv ctx (mk_bvsle ctx e1 e2) true

  let convert_unop {ctx = ctx} e =
    function
    | NOT -> mk_bvnot ctx e
    | NEG -> mk_bvneg ctx e

  let convert_cast {ctx = ctx} e (t,tnew) =
    (*dprintf "increasing bit size from %d to %d" (get_bits t) (get_bits tnew);*)
    function
      | _ when tnew = t -> e
      | CAST_UNSIGNED -> mk_zero_ext ctx (get_bits tnew - get_bits t) e
      | CAST_SIGNED -> mk_sign_ext ctx (get_bits tnew - get_bits t) e
      | CAST_HIGH -> mk_extract ctx (get_bits t-1) (get_bits t - get_bits tnew) e
      | CAST_LOW -> mk_extract ctx (get_bits tnew - 1) 0 e

  let rec convert_type ({ctx=ctx} as set) = function
    | Reg n -> mk_bv_sort ctx n
    | Array(((Reg i) as it), ((Reg v) as vt)) ->
        mk_array_sort ctx (convert_type set it) (convert_type set vt)
    | _ -> failwith "Invalid type"

  (* Converting AST expression to Z3 expressions *)
  let rec convert ({ctx=ctx; vars=vars} as set) =
    function
    | Var (Var.V(id,s,typ) as v) ->
        (try VarHash.find vars v
         with Not_found ->
           (* The name here must be unique for Z3 to consider two
              variables to be unique.

              Another option would be to create a printer context, and
              use that to ensure we generate unique variable names.
           *)
           let name = mk_string_symbol ctx (Printf.sprintf "%s_%d" s id) in
           let typ = convert_type set typ in
           let var = mk_const ctx name typ in
           (*dprintf "VAR: %s" (ast_to_string ctx var);*)
           VarHash.replace vars v var;
           var)
    | Int (n, t) ->
        let typ = mk_bv_sort ctx (get_bits t) in
          mk_numeral ctx (Big_int_Z.string_of_big_int n) typ
    | BinOp (op, e1, e2) ->
        let e1 = convert set e1 in
        let e2 = convert set e2 in
          convert_binop ctx e1 e2 op
    | Cast (ctype, t, e) ->
        let typ = Typecheck.infer_ast e in
        let e = convert set e in
          convert_cast set e (typ,t) ctype
    | UnOp (op, e) ->
        let e = convert set e in
          convert_unop set e op
    | Load(earr, eidx, _, _) ->
        (* We don't care about endianness since arrays use unit sized
           operations.  We also don't care about types, because the type
           is encoded in the array type.  We assume everything type
           checks. *)
        let earr = convert set earr in
        let eidx = convert set eidx in
        mk_select ctx earr eidx
    | Store(earr, eidx, eval, _, _) ->
        (* See above note about ignored params *)
        let earr = convert set earr in
        let eidx = convert set eidx in
        let eval = convert set eval in
        mk_store ctx earr eidx eval
    | Let(v, e, e') ->
        let e = convert set e in
        VarHash.add vars v e; (* Extend context *)
        let e' = convert set e' in
        VarHash.remove vars v; (* Unextend *)
        e'
    | e -> failwith ("unsupported expression: "^(Pp.ast_exp_to_string e))

  let add_binding {vars=vars} v e =
    (* let e = convert set e in *)
    VarHash.add vars v e (* Extend context *)

  let del_binding {vars=vars} v =
    VarHash.remove vars v

(*  let maybe_false {ctx = ctx} cond =
    let negcond = match sort_to_string ctx (get_sort ctx cond) with
      | "bool" -> mk_not ctx cond
      (*| "bv"   -> mk_bvnot ctx cond*)
      | _ -> failwith "omg?"
    in
    push ctx;
    assert_cnstr ctx negcond;
    let answer = match check ctx with
      | L_TRUE -> true
      | L_FALSE -> false
      | _ -> failwith "Unable to determine validity"
    in
    Printf.printf "chk2\n"; flush stdout;
    pop ctx 1;
    Printf.printf "chk3\n"; flush stdout;
    answer
*)

  let add_constraint {ctx=ctx} formula =
    let typ = mk_bv_sort ctx 1 in
    let tr = mk_int ctx 1 typ in
    let formula = mk_eq ctx formula tr in
    (* dprintf "adding constraint %s" (ast_to_string ctx formula); *)
    assert_cnstr ctx formula

  let push {ctx = ctx; vars = vars; vars_stack = vs} =
    Stack.push (VarHash.copy vars) vs;
    push ctx

  let pop context =
    pop context.ctx 1;
    context.vars <- Stack.pop context.vars_stack

  (* let maybe_true ({ctx = ctx} as set) cond = *)
  (*   push set; *)
  (*   let cond = convert set cond in *)
  (*   add_constraint set cond; *)
  (*   let answer = match check ctx with *)
  (*     | L_TRUE -> true *)
  (*     | L_FALSE -> false *)
  (*     | _ -> failwith "Unable to determine validity" *)
  (*   in *)
  (*   pop set; *)
  (*   answer *)

  let get_concrete_model {ctx = ctx} =
    let sat, model = check_and_get_model ctx in
    (* Arrange for the model to be freed *)
    Gc.finalise
      (fun model ->
         dprintf "Freeing z3 model";
         del_model ctx model) model;
    if sat <> L_TRUE then
      failwith "Failed to obtain model"
    else
      model

  let get_concrete_value {ctx=ctx} model expr =
    match eval ctx model expr with
      | true, v ->
          (
            match get_ast_kind ctx v with
              | NUMERAL_AST -> get_numeral_uint ctx v
              | APP_AST -> wprintf "Found an unbound variable, using 0"; (true, 0)
              | VAR_AST -> failwith "bound variable!"
              | QUANTIFIER_AST -> failwith "variable had a quantifier type"
              | UNKNOWN_AST -> failwith "unknown type"
          )
      | _ -> failwith "Failed to get the concrete value!"

  (* let get_bool_value ({ctx = ctx} as set) cond = *)
  (*   (\* Can we do it any other way??? *\) *)
  (*   let model = get_concrete_model set in *)
  (*   (\*dprintf "model: %s" (model_to_string ctx model);*\) *)
  (*   (\*pp_exp set cond;*\) *)
  (*   let answer = match eval ctx model cond with *)
  (*     | true, v when is_eq_ast ctx v (mk_int ctx 1 (mk_bv_sort ctx 1)) -> Some true *)
  (*     | true, v when is_eq_ast ctx v (mk_int ctx 0 (mk_bv_sort ctx 1)) -> Some false *)
  (*     | true, v -> *)
  (*         (\* pp_exp set v; *\) *)
  (*         dprintf "neither true nor false"; *)
  (*         None *)
  (*     | _ -> failwith "Eval call failed" *)
  (*   in *)
  (*   del_model ctx model; *)
  (*   answer *)

  let get_values ({ctx = ctx; vars = vars} as set) =
    let model = get_concrete_model set in
    let values = VarHash.fold
      (fun var value acc ->
         (var, snd(get_concrete_value set model value))::acc
      ) vars []
    in
    del_model ctx model;
    values

  (* let get_model ({ctx = ctx;} as set) form = *)
  (*   push set; *)
  (*   let form = convert set form in *)
  (*   add_constraint set form; *)
  (*   let answer = get_values set in *)
  (*   pop set; *)
  (*   answer *)

  (* Sat testing *)
  let is_sat {ctx=ctx} =
    check ctx = L_TRUE

  let get_model =
    get_concrete_model

  let model_exp ctx model e =
    let newe = convert ctx e in
    match get_concrete_value ctx model newe with
    | true, i -> Ast.Int(biconst i, Typecheck.infer_ast e)
    | _ -> serror ("Unable to find evaluation of " ^ (Pp.ast_exp_to_string e) ^ " in model")

  (* Printing a Z3 solution *)
  let pp_sol {ctx=ctx; vars=vars} =
    let r,m = check_and_get_model ctx in
    let s = match r with
      | (L_TRUE) ->
          Printf.sprintf "satisfiable: %s\n" (Z3.model_to_string ctx m)
      | (L_FALSE) ->
          "unsatisfiable"
      | (L_UNDEF) ->
          "unknown"
    in
    del_model ctx m;
    s

  (* Printing a Z3 formula *)
  let pp_exp {ctx=ctx} formula =
    (ast_to_string ctx formula)

end
module Z3 = Make(ZIF)

  ELSE
module Z3 = Make(NullSolver)
  END;;

(** Build a solver module from a Smtexec module *)
module MakeFromExec(S:Smtexec.SOLVER) =
struct
  let name = S.solvername

  type varmap = Ast.exp VarHash.t

  type set = {
    mutable forms : (Ast.exp list);
    forms_stack : (Ast.exp list) Stack.t;
    mutable vars : varmap;
    vars_stack : varmap Stack.t;
  }
  type exp = Ast.exp
  type model = unit

  let mk_set () =
    {forms = []; forms_stack = Stack.create (); vars=VarHash.create 256; vars_stack = Stack.create ()}

  let del_set _ = ()

  let rec convert =
    let v vars = object(self)
      inherit Ast_visitor.nop
      method visit_exp = function
        | Var v ->
          let newv = (try VarHash.find vars v
            with Not_found ->
              (Var v)) in
          ChangeToAndDoChildren newv
        | e -> DoChildren
    end in
    (fun {vars=vars} e ->
      Ast_visitor.exp_accept (v vars) e)

  let add_binding {vars=vars} v e =
    (* let e = convert set e in *)
    VarHash.add vars v e (* Extend context *)

  let del_binding {vars=vars} v =
    VarHash.remove vars v

  let add_constraint ctx e =
    ctx.forms <- e :: ctx.forms

  let is_sat {forms=forms} =
    let f = List.fold_left
      (fun f e -> binop AND f e)
      exp_true
      forms
    in
    (* If NOT e is invalid, then e is satisfiable *)
    let validf = unop NOT f in
    match S.check_exp_validity validf with
    | Smtexec.Invalid _ -> true
    | Smtexec.Valid -> false
    | Smtexec.SmtError(s) -> failwith ("is_sat, SMTError: " ^ s)
    | Smtexec.Timeout -> failwith "is_sat, SMTError: timeout"

  let get_model _ = failwith "unimplemented"
  let model_exp _ = failwith "unimplemented"

  let push {forms=forms; forms_stack=forms_stack; vars=vars; vars_stack = vs} =
    Stack.push (VarHash.copy vars) vs;
    Stack.push forms forms_stack

  let pop ctx =
    let () = ctx.vars <- Stack.pop ctx.vars_stack in
    ctx.forms <- Stack.pop ctx.forms_stack

  let pp_sol _ =
    "unimplemented"
  let pp_exp _ = Pp.ast_exp_to_string
end

module STPExec = Make(MakeFromExec(Smtexec.STP))
module STPSMTLIBExec = Make(MakeFromExec(Smtexec.STPSMTLIB))
module CVC3Exec = Make(MakeFromExec(Smtexec.CVC3))
module CVC3SMTLIBExec = Make(MakeFromExec(Smtexec.CVC3SMTLIB))
module YICESExec = Make(MakeFromExec(Smtexec.YICES))

(*module STP = Make(SIF)*)

let solvers = Hashtbl.create 10 ;;
List.iter (fun (n,s) -> Hashtbl.add solvers n s)
  (
    ("stp", STPExec.newsolver)
    ::("stp_smtlib", STPSMTLIBExec.newsolver)
    ::("cvc3", CVC3Exec.newsolver)
    ::("cvc3_smtlib", CVC3SMTLIBExec.newsolver)
    ::("yices", YICESExec.newsolver)
    ::("z3", Z3.newsolver)
    ::[]
  )


(* XXX: Move me to unit test *)
let memtest =
  let mem = newvar "mem" (TMem reg_32) in
  (* let store = Store(Var(mem), Int(42L, reg_32), Int(42L, reg_32), exp_false, reg_32) in *)
  let load = Load(Var(mem), Int(biconst 42, reg_32), exp_false, reg_32) in
  let test = BinOp(EQ, load, Int(biconst 42, reg_32)) in
  Memory2array.coerce_exp test

let stupidtest =
  let x = Var(newvar "x" reg_8) in
  BinOp(EQ, x, Int(bi2, reg_8))

let test1 =
  let b2 =  Var(newvar "x2" reg_8) in
  let b3 =  Var(newvar "x3" reg_8) in
  let b4 =  Var(newvar "x4" reg_8) in
  let result1 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT,
                                            Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(biconst 24,reg_32)),
                                      BinOp(OR,
                                            Int(biconst64 0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT,
                                                        Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(bi8,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(biconst 24,reg_32)),
                          Int(biconst 0xff,reg_32)))),
          Int(biconst 24,reg_32))
  in
  let result2 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(biconst 24,reg_32)),
                                      BinOp(OR, Int(biconst64 0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(biconst 8,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(biconst 16,reg_32)),
                          Int(biconst 0xff,reg_32)))),
          Int(biconst 16,reg_32))
  in
  let result3 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(biconst 24,reg_32)),
                                      BinOp(OR, Int(biconst64 0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(bi8,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(bi8,reg_32)),
                          Int(biconst 0xff,reg_32)))),
          Int(bi8,reg_32))
  in
  let result4 =
    Cast(CAST_UNSIGNED,
         reg_32,
         Cast(CAST_LOW,
              reg_8,
              BinOp(AND,
                    BinOp(OR,
                          BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                Int(biconst 24,reg_32)),
                          BinOp(OR, Int(biconst64 0x410000L,reg_32),
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                            Int(bi8,reg_32)),
                                      Cast(CAST_UNSIGNED, reg_32, b2)))),
                    Int(biconst 0xff,reg_32))))
  in
  let left1 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT,
                                            Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(biconst 24,reg_32)),
                                      BinOp(OR, Int(biconst64 0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(bi8,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(biconst 24,reg_32)),
                          Int(biconst 0xff,reg_32)))),
          Int(biconst 24,reg_32))
  in
  let left2 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT,
                                            Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(biconst 24,reg_32)),
                                      BinOp(OR, Int(biconst64 0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(bi8,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(biconst 16,reg_32)),
                          Int(biconst 0xff,reg_32)))),
          Int(biconst 16,reg_32))
  in
  let left3 =
    BinOp(LSHIFT,
          Cast(CAST_UNSIGNED,
               reg_32,
               Cast(CAST_LOW,
                    reg_8,
                    BinOp(AND,
                          BinOp(RSHIFT,
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                            Int(biconst 24,reg_32)),
                                      BinOp(OR, Int(biconst64 0x410000L,reg_32),
                                            BinOp(OR,
                                                  BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                                        Int(bi8,reg_32)),
                                                  Cast(CAST_UNSIGNED, reg_32, b2)))),
                                Int(bi8,reg_32)),
                          Int(biconst 0xff,reg_32)))),
          Int(bi8,reg_32))
  in
  let left4 =
    Cast(CAST_UNSIGNED,
         reg_32,
         Cast(CAST_LOW,
              reg_8,
              BinOp(AND,
                    BinOp(OR,
                          BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b4),
                                Int(biconst 24,reg_32)),
                          BinOp(OR, Int(biconst64 0x410000L,reg_32),
                                BinOp(OR,
                                      BinOp(LSHIFT, Cast(CAST_UNSIGNED, reg_32, b3),
                                            Int(bi8,reg_32)),
                                               Cast(CAST_UNSIGNED, reg_32, b2)))),
                    Int(biconst 0xff,reg_32))))
  in
  let final =
    BinOp(LT,
          BinOp(OR, result1, BinOp(OR, result2, BinOp(OR, result3, result4))),
          BinOp(MINUS,
                BinOp(OR,
                      left1,
                      BinOp(OR,
                            left2,
                            BinOp(OR, left3, left4))),
                Int(biconst64 0x400000L,reg_32)))
  in
  final

(* BinOp(EQ,
                  Int(0L, reg_32),
                  BinOp(AND,
                        Var(newvar "X" reg_32),
                        Int(0L, reg_32)))
            *)

let time solver solve test =
  let do_many f x =
    for i = 0 to 500 do
      f x
    done
  in
  let t = Unix.gettimeofday() in
  do_many solve test;
  let t' = Unix.gettimeofday() in
  Printf.eprintf "%s\t-- time: %f\n" solver (t' -. t)

(* class z3_solver formula = *)
(*   object(s) *)

(*     val c = *)
(*       let solver = Z3.create_solver () in *)
(*       let form = Z3.conv_formula solver formula in *)
(*       (\*ZIF.pp_exp solver form;*\) *)
(*       Z3.add_constraint solver form; *)
(*       (\*ZIF.is_valid solver;*\) *)
(*       solver *)

(*     method is_sat cond = *)
(*       (\*ZIF.pp_exp c cond;*\) *)
(*       ZIF.maybe_true c cond *)

(*     method pp_sol () = *)
(*       Z3.pp_sol c; *)

(*     method get_values () = *)
(*       ZIF.get_values c *)

(*     method get_model form = *)
(*       ZIF.get_model c form *)

(*     method add constr = *)
(*       let constr = Z3.conv_formula c constr in *)
(*       (\*ZIF.pp_exp c constr;*\) *)
(*       Z3.add_constraint c constr *)
(*       (\*ZIF.is_valid c*\) *)

(*     method destroy = *)
(*       Z3.destroy_solver c *)

(*   end *)

let z3_testcases () =
  let s = new Z3.solver in
  s#add_constraint test1;
  let b = s#is_sat in
  dprintf "IS SAT: %b\n" b;
  dprintf "Solution: %s" (s#pp_sol);

  let s = new Z3.solver in
  s#add_constraint memtest;
  dprintf "Solution: %s" (s#pp_sol);

  let s = new Z3.solver in
  let x = newvar "x" reg_32 in
  s#add_constraint (BinOp(LE, Var(x), Int(biconst 42, reg_32)));
  dprintf "solution: %s" (s#pp_sol);
  s#push;
  s#add_constraint (BinOp(EQ, Var(x), Int(biconst 42, reg_32)));
  dprintf "solution level 2: %s" (s#pp_sol);
  s#pop;
  dprintf "solution level 1: %s" (s#pp_sol);

  (* Test validity checking *)
  let s = new Z3.solver in
  let twox = BinOp(TIMES, Var(x), Int(bi2, reg_32)) in
  s#add_constraint (BinOp(SLE, Int(bi0, reg_32), twox));
  let e = BinOp(SLE, Int(bi0, reg_32), Var(x)) in
  dprintf "if 2x > 0, are all x > 0? %b" (s#is_valid e);
  dprintf "Solution: %s" s#pp_sol
