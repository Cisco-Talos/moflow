open OUnit

let var1 = Var.newvar "var1" (Type.Reg(8));;
let var2 = Var.newvar "var2" (Type.Reg(8));;

let test_identity _ =
  assert_equal ~cmp:Var.equal ~msg:"var1 does not equal var1!" var1 var1;
  assert_equal ~cmp:Var.equal ~msg:"var2 does not equal var2!" var2 var2;;

let test_inequality2 _ = 
  assert_bool "var1 is equal to var2!" (not(Var.equal var1 var2));;

let suite = "Var" >:::
  [
	"test_identity" >:: test_identity;
	"test_inequality" >:: 
	  (fun () -> 
		if(Var.equal var1 var2) 
		then
		  assert_failure "var1 is equal to var2!"
		else 
		  (* Dummy assert for truth *)
		  assert_equal 0 0);
	  "test_inequality2" >:: test_inequality2;
  ]
