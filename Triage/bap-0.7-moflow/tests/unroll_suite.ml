open Ast
open Big_int_convenience
module C = Cfg.AST
open OUnit
open Utils_common

let basic_nested () =
  let p = Asmir.open_program "C/unroll" in
  let unroll n =
    let cfg = match get_functions ~unroll:n ~names:["main";"_main"] p with
      | [(_,_, Some x)] -> x
      | _ -> assert_failure "Could not find unrolled main"
    in
    let exiT = C.G.V.create Cfg.BB_Exit in
    let stmts = C.get_stmts cfg exiT in
    let stmts = BatList.append stmts [Halt(exp_true, [])] in
    Cfg_ast.to_prog (C.set_stmts cfg exiT stmts)
  in
  let main_9 = unroll 9
  and main_10 = unroll 10 in
  (* The program should work when loops are unrolled 10 times, but no fewer *)
  let ctx = Symbeval.concretely_execute main_10 in
  assert_equal ~msg:"The output of the program should be 100" (Symbeval.Concrete.lookup_var ctx.Symbeval.delta Disasm_i386.eax) (Symbeval.Symbolic(Int(bi 100, reg_32)));
  try ignore(Symbeval.concretely_execute main_9);
      assert_failure "Unrolling <10 times should produce an error"
  with Symbeval.Concrete.AssertFailed _ -> ()

(* Make sure we get an error for irreducible loops *)
let irreducible unrollf () =
  let p, _ = Parser.program_from_file "IL/unroll/irreducible.il" in
  let cfg = Cfg_ast.of_prog p in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  try ignore(unrollf cfg);
      assert_failure "Unrolling an irreducible loop should fail"
  with Failure _ -> ()

(* Make sure we don't get an error for reducible loops *)
let reducible unrollf () =
  let p, _ = Parser.program_from_file "IL/unroll/reducible.il" in
  let cfg = Cfg_ast.of_prog p in
  let cfg = Prune_unreachable.prune_unreachable_ast cfg in
  ignore(unrollf cfg)

let suite = "Unroll" >:::
  [
    "basic_nested" >:: basic_nested;
    "irreducible_sa" >:: irreducible Unroll.unroll_loops_sa;
    "reducible_sa" >:: reducible Unroll.unroll_loops_sa;
    "irreducible_steensgard" >:: irreducible Unroll.unroll_loops_steensgard;
    "reducible_steensgard" >:: reducible Unroll.unroll_loops_steensgard;
  ]
