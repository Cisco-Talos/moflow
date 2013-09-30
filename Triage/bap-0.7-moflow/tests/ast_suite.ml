open OUnit

(*let exp_var = Ast.Exp(Var.newvar(reg_32));;*)

let is_true = Ast.full_exp_eq Ast.exp_true
let is_false = Ast.full_exp_eq Ast.exp_false

let test_truth_id _ = 
  let s = "Ast.exp_true does not evaluate to true!" in
  assert_bool s	(is_true Ast.exp_true);;


let test_false_id _ = 
  let s = "Ast.exp_true does not evaluate to true!" in
  assert_bool s (not(is_false Ast.exp_true));;


let suite = "Ast" >::: 
  [
	"test_truth_id" >:: test_truth_id;
	"test_false_id" >:: test_false_id;
  ]

