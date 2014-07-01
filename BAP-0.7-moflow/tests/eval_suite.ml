open OUnit
open Pcre
open Ast
open Test_common
open Big_int
open Big_int_convenience

let test_file = "C/test";;
let il_file = "C/test.il";;

(** Lift C/test and convert it to bap.  Then inject "halt true" after the return
    in the main function. Print this out to the file test.il. *)
let concrete_eval_setup _ =
  let out = open_out il_file in
  let pp = new Pp.pp_oc out in
  let prog = Asmir.open_program test_file in
  let ranges = Func_boundary.get_function_ranges prog in
  let (start_addr,_) = find_funs ranges ["main"; "_main"] in
  (* Silence floating point warnings for tests *)
  let _ = if (Asmir.get_print_warning()) then Asmir.set_print_warning(false) in
  let ir = Asmir.asmprogram_to_bap prog in
  let outir = inject_stmt ir start_addr "ret" halt_stmt in 
  pp#ast_program outir;
  pp#close;
  (ranges, start_addr);;


(** Open the file test.il and run two concrete executions.  The first verifies
    running from main results in the desired value (42 = 0x2aL).  The second
    concrete execution changes the value on the stack to 43 (i), starts
    the execution at the "call <g>" assembly instruction in main, and verifies
	that the result is -1. *)
let concrete_eval_test (ranges, s) = 
  (* i represents the change on the stack to the "wrong" value for function g *)
  let i =
    let a,_ = Parser.exp_from_string "R_ESP:u32" in
    let e,_ = Parser.exp_from_string "43:u32" in
    let t = Typecheck.infer_ast e in
    let m = match Parser.exp_from_string "mem:?u32" with
      | Var(v), _ -> v
      | _ -> assert false
    in
    let s = Move(m, Store(Var(m), a, e, exp_false, t), []) in
    [s]
  in
  let prog,_ = Parser.program_from_file il_file in
  let ctx1 = Symbeval.concretely_execute ~s prog in
  let eax1 = biconst 0x2a in
  let (start_addr,end_addr) = find_funs ranges ["main"; "_main"] in
  let main_prog = Test_common.find_prog_chunk prog start_addr end_addr in
  let s = find_call main_prog in
  let ctx2 = Symbeval.concretely_execute ~s ~i prog in
  let eax2 = Arithmetic.to_big_int(bim1,Type.Reg(32)) in
  let msg = "from check_functions" in
  typecheck prog;
  (try check_functions msg ranges ["main"; "g"]
   with _ -> check_functions msg ranges ["_main"; "_g"]);
  check_eax ctx1 eax1;
  check_eax ctx2 eax2;;


let concrete_eval_tear_down _ = rm_and_ignore il_file;;


let suite = "Eval" >:::
  [
	"concrete_eval_test" >::
	  (bracket concrete_eval_setup concrete_eval_test concrete_eval_tear_down);
  ]
