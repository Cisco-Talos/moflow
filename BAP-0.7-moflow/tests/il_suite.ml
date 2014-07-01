open OUnit


let test_input_fails _ =
  assert_raises (Arg.Bad "No input specified") (fun _ -> Input.get_program());;

let test_input_nop () = 
  let p = Asmir.open_program "./asm/nop" in
  (* Silence floating point warnings for tests *)
  let _ = if (Asmir.get_print_warning()) then Asmir.set_print_warning(false) in
  let stmts = Asmir.asmprogram_to_bap p in
  match stmts with
	| [] -> 
	  let s = 
		"Asmir.asmprogram_to_bap of Asmir.open_program \"./asm/nop\" returned \
         an empty list!" in
	  assert_failure s
	| stmt::stmts ->
	  (* XXX Skip for now; the intention is for this to find where the nop
		 should be and verify a nop is there *)
	  skip_if 
		true
		("First stmt of program ./asm/nop is "^(Pp.ast_stmt_to_string stmt))


let suite = "IL" >::: 
  [
	"test_input_fails" >:: test_input_fails;
	(* "test_input_nop" >:: test_input_nop; *)
  ]
