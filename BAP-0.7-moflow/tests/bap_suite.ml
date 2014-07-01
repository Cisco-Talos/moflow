open OUnit
open Test_common


let short_tests = [
  Il_suite.suite;
  Var_suite.suite;
  Ast_suite.suite;
  Disasm_i386_suite.suite;
  Asmir_suite.suite;
  Eval_suite.suite;
  Traces_suite.suite;
  Predicate_suite.suite;
  Arithmetic_suite.suite;
  Dominator_suite.suite;
  Unroll_suite.suite;
];;

let bap_tests = ("BAP" >::: short_tests);;

let _ =
  let results = run_test_tt_main ~arg_specs:speclist bap_tests in
  Format.printf "%s\n" "";
  summarize_results results
;;
