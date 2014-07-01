(**
   Experimental module for performing LLVM code generation.

   $Id: llvm_codegen.ml 7215 2013-03-02 03:20:35Z edmcman $

   See http://llvm.org/docs/tutorial/OCamlLangImpl3.html for tutorial.
*)

open Ast
open Big_int_convenience
module CA = Cfg.AST
module D = Debug.Make(struct let name = "Llvm_codegen" and default=`NoDebug end)
open D
open Llvm
open Llvm_bitwriter
open Llvm_executionengine
open Llvm_scalar_opts
open Type

module VH = Var.VarHash

(** Context for conversion functions *)
type ctx = {
  globalvars: llvalue VH.t;
  vars: llvalue VH.t;
  letvars: llvalue VH.t;
  mutable allocbb: llbasicblock option
           }

let new_ctx () = { globalvars = VH.create 100;
                   vars=VH.create 100;
                   letvars=VH.create 100;
                   allocbb=None
                 }

type varuse = Store | Load

type memimpl = Real (** Use "real" memory; unsafe *)
               | Func (** Use memory functions, one byte at a time *)
               | FuncMulti (** Use memory functions, multiple bytes at a time *)

let addr_type = Reg 64

let string_to_memimpl s = match s with
  | "Real" -> Some Real
  | "Func" -> Some Func
  | "FuncMulti" -> Some FuncMulti
  | _ -> None

(** Topological sort for CFG conversion *)
module Toposort = Graph.Topological.Make(CA.G)

class codegen ?(opts=true) memimpl =

  let () = ignore (initialize_native_target ()) in
  let context = global_context () in
  let the_module = create_module context "BAP code generator" in
  let builder = builder context in
  let destroy obj = dispose_module the_module in
  (* XXX: Garbage collect execengine *)
  let execengine = ExecutionEngine.create the_module in
  let the_fpm = PassManager.create_function the_module in
  (* Set up the optimizer pipeline.  Start with registering info about
   * how the target lays out data structures. *)
  let () = Llvm_target.TargetData.add (ExecutionEngine.target_data execengine) the_fpm in

  let () = if opts then (

    (* Aggregate to scalar opts *)
    let () = add_scalar_repl_aggregation the_fpm in

    (* Promote allocas to registers *)
    let () = add_memory_to_register_promotion the_fpm in

    (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
    let () = add_instruction_combination the_fpm in

    (* reassociate expressions. *)
    let () = add_reassociation the_fpm in

    (* Eliminate Common SubExpressions. *)
    let () = add_gvn the_fpm in

    (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
    add_cfg_simplification the_fpm)
  in
  let () = ignore (PassManager.initialize the_fpm) in

  (* Find assert function to be called on Assert statements *)
  let assertf = match lookup_function "fake_assert" the_module with
    | None ->
      declare_function "fake_assert" (function_type (void_type context) [| i32_type context |]) the_module
    | Some x -> x
  in
  (* XXX: Right way to do C++ name mangling? *)
  let setm = match lookup_function "set_memory" the_module with
    | None ->
      declare_function "set_memory" (function_type (void_type context) [| i64_type context; i8_type context |]) the_module
    | Some x -> x
  in
  let setmmulti = match lookup_function "set_memory_multi" the_module with
    | None ->
      declare_function "set_memory_multi" (function_type (void_type context) [| i64_type context; pointer_type (i8_type context); i32_type context |]) the_module
    | Some x -> x
  in
  let getm = match lookup_function "get_memory" the_module with
    | None ->
      declare_function "get_memory" (function_type (i8_type context) [| i64_type context |]) the_module
    | Some x -> x
  in
  let getmmulti = match lookup_function "get_memory_multi" the_module with
    | None ->
      declare_function "get_memory_multi" (function_type (void_type context) [| i64_type context; pointer_type (i8_type context); i32_type context |]) the_module
    | Some x -> x
  in

object(self)

  initializer Gc.finalise destroy self

  val ctx = new_ctx ()

  val memimpl = memimpl

  method convert_type = function
    | Reg n -> integer_type context n
    | _ -> failwith "No idea how to handle memories yet"

  (** Convert a var to its global LLVM pointer *)
  method private convert_global_var (Var.V(_, s, t) as v) =
    try VH.find ctx.globalvars v
    with Not_found ->
      let init = self#convert_exp (Int(bi0, t)) in
      let a = define_global s init the_module in
      VH.add ctx.globalvars v a;
      a

  method private convert_temp_var (Var.V(_, s, t) as v) =
    try VH.find ctx.vars v
    with Not_found ->
      let a = self#in_alloc
        (lazy (let a = build_alloca (self#convert_type t) s builder in
               a)) in
      VH.add ctx.vars v a;
      a

  method private convert_let_var (Var.V(_, s, t) as v) =
    VH.find ctx.letvars v

  (** Convert a Var to its LLVM pointer *)
  method convert_var (Var.V(_, s, t) as v) =
    if not (Disasm.is_temp v) then self#convert_global_var v
    else self#convert_temp_var v

  (** Compile LLVM code to evaluate an Ast.exp. *)
  method convert_exp e =
    (* dprintf "converting e: %s" (Pp.ast_exp_to_string e); *)
    match e with
    | Ite _ as ite -> self#convert_exp (Ast_convenience.rm_ite ite)
    | Extract _ as e -> self#convert_exp (Ast_convenience.rm_extract e)
    | Concat _ as c -> self#convert_exp (Ast_convenience.rm_concat c)
    (* Let for integer types *)
    | Let(v, e, e') when Typecheck.is_integer_type (Var.typ v)
        && Typecheck.is_integer_type (Typecheck.infer_ast e') ->
      let () = VH.add ctx.letvars v (self#convert_exp e) in
      let save = self#convert_exp e' in
      VH.remove ctx.letvars v;
      save
    (* Let for memory bindings that return integer type (Loads) needs to be flattened *)
    | Let(v, e, e') as bige when Typecheck.is_mem_type (Var.typ v)
        && Typecheck.is_integer_type (Typecheck.infer_ast e') ->
      self#convert_exp (Flatten_mem.flatten_loads bige)
    | Int(i, t) ->
      let lt = self#convert_type t in
      (try const_of_int64 lt (Big_int_Z.int64_of_big_int i) (*signed?*) false
       with Failure _ -> const_int_of_string lt (Big_int_Z.string_of_big_int i) 10)
    | Cast(ct, tto, e) ->
      let tto' = self#convert_type tto in
      let e' = self#convert_exp e in
      (match ct with
      | CAST_UNSIGNED -> build_zext e' tto' "cast_unsigned" builder
      | CAST_SIGNED -> build_sext e' tto' "cast_signed" builder
      | CAST_HIGH ->
        (* HHHLLLLLL

           If want to get the H bits, we need to shift right by W-H *)
        let t = Typecheck.infer_ast e in
        let amount = Typecheck.bits_of_width t -
          Typecheck.bits_of_width tto in
        let () = assert (amount >= 0) in
        let amount = self#convert_exp (Int(biconst amount, t)) in
        let shifted = build_lshr e' amount "high_to_low" builder in
        build_trunc shifted tto' "cast_high" builder
      | CAST_LOW -> build_trunc e' tto' "cast_low" builder)
    | UnOp(uop, e) ->
      let e' = self#convert_exp e in
      (match uop with
      | NEG ->
        let zero = self#convert_exp (Int(bi0, Typecheck.infer_ast e)) in
        (* LLVM has no neg *)
        build_sub zero e' "neg" builder
      | NOT ->
        let negone = self#convert_exp (Int(bim1, Typecheck.infer_ast e)) in
        (* LLVM has no bitwise not *)
        build_xor negone e' "bwnot" builder)
    | BinOp(bop, e1, e2) ->
      let le1 = self#convert_exp e1 in
      let le2 = self#convert_exp e2 in
      let bf = match bop with
        | PLUS -> build_add
        | MINUS -> build_sub
        | TIMES -> build_mul
        | DIVIDE -> build_udiv
        | SDIVIDE -> build_sdiv
        | MOD -> build_urem
        | SMOD -> build_srem
        | LSHIFT -> build_shl
        | RSHIFT -> build_lshr
        | ARSHIFT -> build_ashr
        | AND -> build_and
        | OR -> build_or
        | XOR -> build_xor
        | EQ -> build_icmp (Llvm.Icmp.Eq)
        | NEQ -> build_icmp (Llvm.Icmp.Ne)
        | LT -> build_icmp (Llvm.Icmp.Ult)
        | LE -> build_icmp (Llvm.Icmp.Ule)
        | SLT -> build_icmp (Llvm.Icmp.Slt)
        | SLE -> build_icmp (Llvm.Icmp.Sle)
      in
      bf le1 le2 (Pp.binop_to_string bop^"_tmp") builder
    | Var(v) ->
      (try self#convert_let_var v
       with Not_found ->
         let mem = self#convert_var v in
         build_load mem ("load_var_"^(Var.name v)) builder)
    (* How do we handle this properly? *)
    | Ast.Load(Var m, idx, e, t) when memimpl = Real (*&& m = (Var Asmir.x86_mem) *) ->
      let idx' = self#convert_exp idx in
      let idxt = pointer_type (self#convert_type t) in
      let idx'' = build_inttoptr idx' idxt "load_address" builder in
      build_load idx'' "load" builder
    | Ast.Load(Var m, idx, e, t) when memimpl = Func ->
      let idx' = self#convert_exp (Cast(CAST_UNSIGNED, reg_64, idx)) in
      build_call getm [| idx' |] "getm_result" builder
    | Ast.Load(Var m, idx, e, t) when memimpl = FuncMulti ->
      (* Alloc a buffer, call getmmulti, and then load from the buffer. *)
      let t' = self#convert_type t in
      let idx' = self#convert_exp (Cast(CAST_UNSIGNED, reg_64, idx)) in
      let alloc = build_alloca t' "multi_load_buf" builder in
      let alloc' = build_bitcast alloc (pointer_type (i8_type context)) "array_convert" builder in
      let nbytes = self#convert_exp (Int(biconst (Typecheck.bytes_of_width t), reg_32)) in
      ignore(build_call getmmulti [| idx'; alloc'; nbytes |] "" builder);
      build_load alloc "load_multiload" builder
    | Ast.Store _ -> failwith "Stores are not proper expressions"
    | Ast.Unknown (_, t) -> undef (self#convert_type t)
    | e -> failwith (Printf.sprintf "Unsupported expression %s" (Pp.ast_exp_to_string e))

  (* (\** Create an anonymous function to compute e *\) *)
  (* method convert_exp_helper e = *)
  (*   let t = Typecheck.infer_ast e in *)
  (*   let lt = self#convert_type t in *)
  (*   let f = declare_function "" (function_type lt [||]) the_module in *)
  (*   let bb = append_block context "entry" f in *)
  (*   position_at_end bb builder; *)
  (*   let ret = self#convert_exp e in *)
  (*   ignore(build_ret ret builder); *)
  (*   Llvm_analysis.assert_valid_function f; *)
  (*   f *)

  (** Convert a single straight-line statement *)
  method private convert_straightline_stmt s = 
    (* dprintf "Converting stmt %s" (Pp.ast_stmt_to_string s); *)
    match s with
    | Label _ -> ()
    | (Jmp _ | CJmp _ | Special _) as s ->
      failwith (Printf.sprintf "convert_straightline_stmt: Non-straightline statement type %s" (Pp.ast_stmt_to_string s))
    | Assert(e, _) ->
      let e' = self#convert_exp e in
      (* Convert to 32-bit to call fake_assert *)
      let c = build_zext e' (i32_type context) "cond" builder in
      ignore(build_call assertf [| c |] "" builder)
    | Halt(e, _) ->
      ignore(build_ret (self#convert_exp e) builder)
    | Move(v, e, _) when Typecheck.is_integer_type (Var.typ v) ->
      let exp = self#convert_exp e in
      let mem = self#convert_var v in
      ignore(build_store exp mem builder)
    (* Simple memory write we understand *)
    (* XXX: How do we make sure this is a write to "the big global
       memory"? *)
    | Move(mv, Ast.Store(Var m,i,v,e,t), _) when Typecheck.is_mem_type (Var.typ mv) && memimpl = Real ->
      let idx' = self#convert_exp i in
      let idxt = pointer_type (self#convert_type t) in
      let idx'' = build_inttoptr idx' idxt "load_address" builder in
      let v' = self#convert_exp v in
      ignore(build_store v' idx'' builder)
    | Move(mv, Ast.Store(Var m,i,v,e,t), _) when Typecheck.is_mem_type (Var.typ mv) && memimpl = Func ->
      assert (t = reg_8);
      let idx' = self#convert_exp (Cast(CAST_UNSIGNED, reg_64, i)) in
      let v' = self#convert_exp v in
      ignore(build_call setm [| idx'; v' |] "" builder);
    | Move(mv, Ast.Store(Var m,i,v,e,t), _) when Typecheck.is_mem_type (Var.typ mv) && memimpl = FuncMulti ->
      (* We are going to create a stack slot, copy the value there,
         and then call setmmulti. Hopefully LLVM's optimizers will get
         rid of this copy... *)
      let t' = self#convert_type t in
      let idx' = self#convert_exp (Cast(CAST_UNSIGNED, reg_64, i)) in
      let v' = self#convert_exp v in
      let alloc = build_alloca t' "multi_store_buf" builder in
      ignore(build_store v' alloc builder);
      let alloc' = build_bitcast alloc (pointer_type (i8_type context)) "array_convert" builder in
      let nbytes = self#convert_exp (Int(biconst (Typecheck.bytes_of_width t), reg_32)) in
      ignore(build_call setmmulti [| idx'; alloc'; nbytes |] "" builder);
    | Move(mv, Ast.Store(Var m,i,v,e,t), _) when Typecheck.is_mem_type (Var.typ mv) ->
      failwith "Unhandled memory store"
    (* XXX: How do we make sure this is a write to "the big global
       memory"? *)
    (* A complicated memory write we need to simplify *)
    | Move(v, _, _) as s when Typecheck.is_mem_type (Var.typ v) ->
      List.iter self#convert_straightline_stmt (Flatten_mem.flatten_stores s)
    | Comment _ -> ()
    | _ -> failwith "convert_straightline_stmt: Unimplemented"

  (** Convert straight-line code (multiple statements) *)
  method private convert_straightline p =
    List.iter self#convert_straightline_stmt p;

  method convert_straightline_f p =
    let p, halte = match Ast_convenience.last_meaningful_stmt p with
      | Halt (e, _) -> p, e
      | _ -> List.rev (Halt (exp_true, []) :: List.rev p), exp_true
    in
    let rt = self#convert_type (Typecheck.infer_ast halte) in
    let f = self#anon_fun ~t:rt () in
    self#convert_straightline p;
    Llvm_analysis.assert_valid_function f;
    (* Optimize until fixed point *)
    while PassManager.run_function f the_fpm do () done;
    f

  method private convert_jump_statement ijf rt s bb succs = match succs, s with
  (* If there are no successors, return now. *)
  | _, Some (Halt(e, _)) ->
    ignore(build_ret (self#convert_exp e) builder)
  | [], o ->
    (* wprintf "Program exit without returning a value"; *)
    if rt = void_type context then ignore(build_ret_void builder)
    (* If no halt, we'll say undefined *)
    else ignore(build_ret (undef rt) builder)
  (* Indirect jump *)
  | _, Some (Jmp(e, _)) when Ast.lab_of_exp e = None ->
    ijf e
  (* Unconditional branch *)
  | [x], _ -> ignore(build_br x builder)
  (* Conditional branch. The first element is the true branch. *)
  | [x;y], Some(CJmp(e, _, _, _)) ->
    let e' = self#convert_exp e in
    ignore(build_cond_br e' x y builder)
  (* BB_Indirect is allowed to have multiple successors *)
  | _ when (CA.G.V.label bb) = Cfg.BB_Indirect -> ignore(build_unreachable builder)
  | _ -> failwith (Printf.sprintf "convert_jump_statement: Invalid CFG @%s" (Cfg_ast.v2s bb))

  (** Create a BB for handling indirect jumps *)
  method private create_indirect_handler =
    let addr = build_alloca (self#convert_type addr_type) "jump_addr" builder in
    (* Append a new BB to the function *)
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    (* Abort BB *)
    let abort_bb = append_block context "invalid_address" the_function in
    ignore(position_at_end abort_bb builder);
    ignore(build_call assertf [| const_null (i32_type context) |] "" builder);
    ignore(build_unreachable builder);

    (* Switch BB *)
    let switch_bb = append_block context "switch" the_function in
    ignore(position_at_end switch_bb builder);
    let addr_load = build_load addr "read_addr" builder in
    let switch = build_switch addr_load abort_bb 100 builder in
    (* Move builder to original point *)
    ignore(position_at_end start_bb builder);

    (* Jump handler *)
    (fun e ->
      (* move llvalue to addr *)
      let llvalue = self#convert_exp (Cast(CAST_UNSIGNED, addr_type, e)) in
      ignore(build_store llvalue addr builder);
      ignore(build_br switch_bb builder)),

    (* Add new case *)
    (fun llvalue llbb ->
      add_case switch llvalue llbb)

(** Convert a cfg to llvm IR. *)
  method convert_cfg cfg =

    let halt_type =
      let t = ref None in
      let v = object(self)
        inherit Ast_visitor.nop
        method visit_stmt s =
          match s, !t with
          | Halt(e, _), None ->
            t := Some(Typecheck.infer_ast e);
            SkipChildren
          | Halt(e, _), Some t when (Typecheck.infer_ast e) = t ->
            SkipChildren
          | Halt(e, _), Some t when (Typecheck.infer_ast e) <> t ->
            failwith "Program is not well typed: multiple return value types"
          | _ ->
            SkipChildren
      end
      in
      ignore(Ast_visitor.cfg_accept v cfg);
      (* Haskell functors would be nice :( *)
      match !t with
      | Some x -> self#convert_type x
      | None -> void_type context
    in
    dprintf "return type: %s" (string_of_lltype halt_type);

    (* Create a function for this cfg *)
    let f = self#anon_fun ~t:halt_type () in

    (* Create an indirect jump handler *)
    let build_ind_jump, add_label = self#create_indirect_handler in

    (* Map bb to llvm bb *)
    let module H = Hashtbl.Make(CA.G.V) in
    let h = H.create 1000 in
    let get_llvmbb bb =
      try H.find h bb
      with Not_found ->
        let llvmbb = append_block context (Cfg_ast.v2s bb) f in
        H.add h bb llvmbb;
        llvmbb
    in

    (* Add labels to indirect jump *)
    let add_labels bb stmts =
      List.iter (function
        | Label (l, _) ->
          (match exp_of_lab l with
          | Int _ as e ->
            add_label (self#convert_exp e) (get_llvmbb bb)
          | _ -> ())
        | _ -> ()) stmts
    in

    let firstbb = ref true in
    Toposort.iter (fun bb ->

      (* Create a new LLVM bb for this bb *)
      let llvmbb = get_llvmbb bb in

      (* If this is the entry node, we are building the "entry" block,
         and need to branch to llvmbb. *)
      if (!firstbb || CA.G.V.label bb = Cfg.BB_Entry) then (
        (* If one of these is true, both had better be true. *)
        assert (!firstbb && CA.G.V.label bb = Cfg.BB_Entry);

        ignore(build_br llvmbb builder);
        firstbb := false);

      (* Go to new bb *)
      position_at_end llvmbb builder;

      (* Find the successor bbs. If there are two, make sure the true
         branch is first in the list. *)

      let succbbs =
        List.map CA.G.E.dst
          (match CA.G.succ_e cfg bb with
          | l when (CA.G.V.label bb) = Cfg.BB_Indirect -> l
          | [] -> []
          | [x] -> [x]
          | [x;y] when CA.G.E.label x = Some true
                  && CA.G.E.label y = Some false -> [x;y]
          | [y;x] when CA.G.E.label x = Some true
                  && CA.G.E.label y = Some false -> [x;y]
          | _ -> failwith "invalid cfg")
      in
      let succllvmbbs = List.map get_llvmbb succbbs in

      let stmts = CA.get_stmts cfg bb in
      add_labels bb stmts;
      let cfstmt, straightline_stmts = match List.rev stmts with
        | (Halt _ as j::tl) | (Jmp _ as j::tl) | (CJmp _ as j::tl) -> Some(j), tl
        | _ -> None, stmts
      in

      (* Convert the straightline statements *)
      self#convert_straightline straightline_stmts;
      (* Convert the jump *)
      ignore(self#convert_jump_statement build_ind_jump halt_type cfstmt bb succllvmbbs);

    ) cfg;
    if debug() then self#dump;
    Llvm_analysis.assert_valid_function f;
    (* Optimize until fixed point.  Ehh that didn't work so well. *)
    let countdown = ref 5 in
    while !countdown != 0 && PassManager.run_function f the_fpm do dprintf "Optimizing"; decr countdown; done;
    f


  (** Execute lazy value l in the allocbb, and then return to the end
      of the bb the builder was in. *)
  method private in_alloc l =
    match ctx.allocbb with
    | None -> failwith "in_alloc: Called with no allocbb set"
    | Some(allocbb) ->
      let save = insertion_block builder in
      (* In the beginning *)
      let () = position_builder (instr_begin allocbb) builder in
      let r = Lazy.force l in
      let () = position_at_end save builder in
      r

  method private anon_fun ?(t = void_type context) () =
    let f = declare_function "" (function_type t [||]) the_module in
    let allocbb = append_block context "allocs" f in
    let () = ctx.allocbb <- Some allocbb in
    let bb = append_block context "entry" f in
    (* Add branch from allocbb to entry bb *)
    position_at_end allocbb builder;
    ignore(build_br bb builder);
    position_at_end bb builder;
    f

  method dump =
    dump_module the_module

  val set_value_h = VH.create 1000
  method private set_value v c =
    if Disasm.is_temp v then failwith "Temporary variables cannot be set";
    let build_setter () =
      let f = declare_function "" (function_type (void_type context) [| self#convert_type (Var.typ v) |]) the_module in
      let bb = append_block context "setbb" f in
      position_at_end bb builder;
      let p = param f 0 in
      ignore(build_store p (self#convert_var v) builder);
      ignore(build_ret_void builder);
      Llvm_analysis.assert_valid_function f;
      if debug() then self#dump;
      f
    in
    let setf = try VH.find set_value_h v
      with Not_found ->
        let newf = build_setter () in
        let newf = function
          | Int(va, _) ->
            ignore(ExecutionEngine.run_function newf
                     (* XXX: Fix me for > int64 *)
                     [|GenericValue.of_int64 (self#convert_type (Var.typ v))
                         (Big_int_Z.int64_of_big_int va)|] execengine)
          | _ -> failwith "Expected a constant"
        in
        VH.add set_value_h v newf;
        newf
    in
    setf c

  val get_value_h = VH.create 1000
  method private get_value v =
    if Disasm.is_temp v then failwith "Temporary variables cannot be get";
    let build_getter () =
      let f = declare_function "" (function_type (self#convert_type (Var.typ v)) [| |]) the_module in
      let bb = append_block context "getbb" f in
      position_at_end bb builder;
      (* let p = param f 0 in *)
      let l = build_load (self#convert_var v) "get" builder in
      ignore(build_ret l builder);
      (* ignore(build_store p (self#convert_var v) builder); *)
      (* ignore(build_ret_void builder); *)
      Llvm_analysis.assert_valid_function f;
      (* self#dump; *)
      f
    in
    let getf = try VH.find get_value_h v
      with Not_found ->
        let newf = build_getter () in
        let newf () =
          let r = ExecutionEngine.run_function newf [||] execengine in
          self#bap_of_generic (Var.typ v) r
        in
        VH.add get_value_h v newf;
        newf
    in
    getf ()

  method private initialize_contexts ctx =
    List.iter (fun (k,v) -> self#set_value k v) ctx

  method eval_fun ?ctx f =
    (match ctx with
    | None -> ()
    | Some x -> self#initialize_contexts x);
    let r = ExecutionEngine.run_function f [||] execengine in
    assert ((classify_type (type_of f)) = TypeKind.Pointer);
    let et = element_type (type_of f) in
    assert ((classify_type et) = TypeKind.Function);
    let rt = return_type et in
    match classify_type rt with
    | TypeKind.Integer ->
      self#bap_of_generic (Reg (integer_bitwidth rt)) r
    | o ->
      exp_true

  method private bap_of_generic t g =
    (match t with
    | Reg n when n > 64 -> failwith "bap_of_generic: Only supports Reg smaller than 64 bits"
    | _ -> ());
    let i64 = GenericValue.as_int64 g in
    Int(Big_int_Z.big_int_of_int64 i64, t)

  method output_bitcode oc =
    output_bitcode oc the_module

end
