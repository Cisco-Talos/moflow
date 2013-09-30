open Ast
open Big_int_convenience
open OUnit
open Pcre
open Test_common
open Type
open Utils_common

let test_file = "C/test";;
let g_il = "g.il";;
let stp_out = "stp_out.stp";;

(* Auto select solver based on which one is available *)

let solver = Smtexec.YICES.si;;
let check = let module SC = SolverCheck(Smtexec.YICES) in
            SC.check_solver_path

(* Assert false goes to BB_Error, not BB_Exit *)
let error_setup () =
  let () = check () in
  let m2actx = Memory2array.create_state () in
  let prog = [
    CJmp(BinOp(EQ, Var Disasm_i386.eax, Int(bi0, Reg 32)), Lab("L1"), Lab("L2"), []);
    Ast.Label(Name("L1"), []);
    Move(Disasm_i386.eax, Int(biconst 41, Reg 32), []);
    Jmp(Lab("end"), []);
    Ast.Label(Name("L2"), []);
    Move(Disasm_i386.eax, Int(biconst 42, Reg 32), []);
    Assert(exp_false, []);
    Ast.Label(Name("end"), []);
  ] in
  typecheck prog;
  let cfg = Cfg_ast.of_prog prog in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  (* Make sure this actually uses BB_Error! *)
  assert (Cfg.AST.G.mem_vertex cfg (Cfg.AST.G.V.create Cfg.BB_Error));
  cfg, m2actx;;

let basic_setup () =
  let () = check () in
  let m2actx = Memory2array.create_state () in
  let prog = [
    CJmp(BinOp(EQ, Var Disasm_i386.eax, Int(bi0, Reg 32)), Lab("L1"), Lab("L2"), []);
    Ast.Label(Name("L1"), []);
    Move(Disasm_i386.eax, Int(biconst 41, Reg 32), []);
    Jmp(Lab("end"), []);
    Ast.Label(Name("L2"), []);
    Move(Disasm_i386.eax, Int(biconst 42, Reg 32), []);
    Ast.Label(Name("end"), []);
  ] in
  typecheck prog;
  let cfg = Cfg_ast.of_prog prog in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  cfg, m2actx;;

let basic_validity_setup () =
  let () = check () in
  let m2actx = Memory2array.create_state () in
  let prog = [
    CJmp(BinOp(EQ, Var Disasm_i386.eax, Int(bi1, Reg 32)), Lab("L1"), Lab("L2"), []);
    CJmp(exp_false, Lab("L1"), Lab("L2"), []);
    Ast.Label(Name("L1"), []);
    Move(Disasm_i386.ebx, Int(biconst 2, Reg 32), []);
    Jmp(Lab("end"), []);
    Ast.Label(Name("L2"), []);
    Move(Disasm_i386.ebx, BinOp(TIMES, Var Disasm_i386.eax, Int(bi2, Reg 32)), []);
    Ast.Label(Name("end"), []);
  ] in
  typecheck prog;
  let cfg = Cfg_ast.of_prog prog in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  cfg, m2actx;;

(* A very basic test of assumption.  The goal of this test is to make
   sure that symbeval_search handles assumption failures correctly. *)
let assume_setup () =
  let () = check () in
  let m2actx = Memory2array.create_state () in
  let prog = [
    CJmp(BinOp(EQ, Var Disasm_i386.eax, Int(bi0, reg_32)), Lab("L1"), Lab("L2"), []);
    Ast.Label(Name("L1"), []);
    Jmp(Lab("end"), []);
    Ast.Label(Name("L2"), []);
    Ast.Assume(exp_false, []);
    Ast.Label(Name("end"), []);
  ] in
  typecheck prog;
  let cfg = Cfg_ast.of_prog prog in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  cfg, m2actx;;

let c_setup () =
  let () = check () in
  let m2actx = Memory2array.create_state () in
  let prog = Asmir.open_program test_file in
  let ranges = Func_boundary.get_function_ranges prog in
  let (start_g, end_g) = find_fun ~msg:" in C_setup" ranges "g" in
  let g_ir = Asmir.asmprogram_to_bap_range prog start_g end_g in
  let g_ir = Hacks.ret_to_jmp g_ir in
  (* ret_to_jmp introduces a TMem, so mem2array must come after! *)
  let g_ir = Memory2array.coerce_prog_state m2actx g_ir in
  let g_cfg = Cfg_ast.of_prog g_ir in
  let g_cfg = Prune_unreachable.prune_unreachable_ast g_cfg in
  let g_cfg = Unroll.unroll_loops g_cfg in
  let g_cfg = Hacks.remove_cycles g_cfg in
  typecheck g_ir;
  g_cfg, m2actx;;


let sat_test testname post stp_result (name,vc) (g_cfg, m2actx) =
  let post = Memory2array.coerce_exp_state m2actx post in
  print_endline ("Testing "^testname^" with "^name^" VC algorithm");
  let vcout, foralls = Vc.vc_astcfg vc Vc.default_options g_cfg post in
  let foralls = List.map (Memory2array.coerce_rvar_state m2actx) foralls in
  let pp = ((solver#printer) :> Formulap.fppf) in
  let oc = open_out stp_out in
  let p = pp oc in
  p#assert_ast_exp ~foralls vcout;
  p#counterexample;
  p#close;
  (let r = solver#solve_formula_file stp_out in
   if (r <> stp_result) then (
     assert_failure ("Predicate solution for " ^ name
                     ^ " was not "
                     ^(Smtexec.result_to_string stp_result)
                     ^" but "^(Smtexec.result_to_string r))))
;;

let valid_test testname post stp_result (name,vc) (g_cfg, m2actx) =
  let post = Memory2array.coerce_exp_state m2actx post in
  print_endline ("Testing "^testname^" with "^name^" VC algorithm");
  let options = {Vc.default_options with Vc.mode = Validity} in
  let vcout, foralls = Vc.vc_astcfg vc options g_cfg post in
  let foralls = List.map (Memory2array.coerce_rvar_state m2actx) foralls in
  let pp = ((solver#printer) :> Formulap.fppf) in
  let oc = open_out stp_out in
  let p = pp oc in
  p#valid_ast_exp ~foralls vcout;
  p#counterexample;
  p#close;
  (let r = solver#solve_formula_file stp_out in
   if (r <> stp_result) then (
     assert_failure ("Predicate solution for " ^ name
                     ^ " was not "
                     ^(Smtexec.result_to_string stp_result)
                     ^" but "^(Smtexec.result_to_string r))))
;;


let predicate_stp_tear_down _ = 
  rm_and_ignore_list [g_il ; stp_out];;

let fold_vcs (s,f) =
  List.map (fun ((vcname,_) as vc) ->
    s^"_"^vcname >:: f vc
  ) Vc.pred_vclist

let suite = "Predicate" >:::
    fold_vcs ("predicate_basic_solve_test",
      (fun vc -> bracket
	 basic_setup
	 (sat_test "basic_solve" (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 42, Reg 32))) (Smtexec.Invalid None) vc)
	 predicate_stp_tear_down))
  @
    fold_vcs ("predicate_basic_unsolve_test",
      (fun vc -> bracket
         basic_setup
         (sat_test "basic_unsolve"  (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 2, Reg 32))) (Smtexec.Valid) vc)
         predicate_stp_tear_down))
  @
    fold_vcs ("predicate_basic_validity_test",
      (fun vc -> bracket
         basic_validity_setup
         (valid_test "basic_validity_test"  (BinOp(EQ, Var Disasm_i386.ebx, BinOp(TIMES, Int(biconst 2, Reg 32), Var Disasm_i386.eax))) (Smtexec.Valid) vc)
         predicate_stp_tear_down))
  @
    fold_vcs ("predicate_assume_validity_test",
      (fun vc -> bracket
         assume_setup
         (valid_test "assume_validity_test" exp_true (Smtexec.Valid) vc)
         predicate_stp_tear_down))
  @
    fold_vcs ("predicate_error_solve_test",
      (fun vc -> bracket
         error_setup
         (sat_test "error_solve" (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 41, Reg 32))) (Smtexec.Invalid None) vc)
         predicate_stp_tear_down))
  @
    fold_vcs ("predicate_error_unsolve_test",
      (fun vc -> bracket
         error_setup
         (sat_test "error_unsolve"  (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 42, Reg 32))) (Smtexec.Valid) vc)
         predicate_stp_tear_down))
  @
    fold_vcs ("predicate_C_solve_test",
      (fun vc -> bracket
         c_setup
         (sat_test "C_solve" (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 42, Reg 32))) (Smtexec.Invalid None) vc)
         predicate_stp_tear_down))
  @
    fold_vcs ("predicate_C_unsolve_test",
      (fun vc -> bracket
         c_setup
         (sat_test "C_unsolve" (BinOp(EQ, Var Disasm_i386.eax, Int(biconst 43, Reg 32))) (Smtexec.Valid) vc)
         predicate_stp_tear_down))
