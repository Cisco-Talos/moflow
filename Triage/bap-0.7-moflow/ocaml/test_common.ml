(** Functions used by BAP library and tests *)

open Ast
open Big_int_Z
open Big_int_convenience
open OUnit
open Pcre
open Type
open BatListFull

exception RangeNotFound of int64 * int64

let leave_files = ref false;;
let speclist = ["-leave-files", Arg.Set leave_files, 
				"Don't remove files after test";];;


(** General system functions **)
let check_file file =
  if not(Sys.file_exists file) then skip_if true
    ("File "^file^" does not exist; Skipping this test!");;


let mkdir_and_ignore path = try Unix.mkdir path 0o640 with _ -> ();;


let rm_and_ignore path =
  if(!leave_files) then () 
  else ((try if (Sys.is_directory(path)) then Unix.rmdir path with _ -> ());
		try Sys.remove path with _ -> ());;


let rec rm_and_ignore_list paths =
  match paths with
  | [] -> ()
  | p::ps -> rm_and_ignore p; rm_and_ignore_list ps;;


(** STP helpers **)
let stp = "stp";;

let does_stp_work () =
  if (Smtexec.STP.check_exp_validity exp_true) = Smtexec.Valid then
    true
  else false

module SolverCheck(S:Smtexec.SOLVER) = struct
  let check_solver_path () =
    if S.in_path() = false then
      skip_if true ("Skipping test. "^S.solvername^" is not in PATH");;
end
let check_stp_path =
  let module SC = SolverCheck(Smtexec.STP) in SC.check_solver_path

(** pin helpers **)
let pin_path = (*ref "../pin/";;*)
  let path = try Sys.getenv("PIN_HOME") with Not_found -> "../pin/" in
  ref path;;
let pin = "pin";;
let gentrace_path = "../pintraces/obj-ia32/";;
let gentrace = "gentrace.so";;
let pin_out_suffix = "-bap-pin-test.out";;


let rec find_pin_out files tag =
  match files with
  | [] -> assert_failure 
    ("Could not find a file with suffix "^tag^pin_out_suffix^" Did pin fail?")
  | f::fs -> if (pmatch ~pat:(tag^pin_out_suffix) f) then f else find_pin_out fs tag;;


let check_pin_setup _ =
  (* Only do this if we are running in an x64 environment *)
  let cat_arg = "/proc/sys/kernel/yama/ptrace_scope" in
  let foutput char_stream = 
    (match (Stream.next char_stream) with
    | '0' -> ()
    | _ -> skip_if true
      (cat_arg^
	 " must contain 0 for pin to work.  As root, please execute $ echo 0 > "
       ^cat_arg))
  in
  (* Check if ptrace_scope has been turned off *)
  if (Sys.file_exists cat_arg) 
  then assert_command ~foutput ~verbose:true "cat" [cat_arg] else ();
  (* Check environment variable for path to pin *)
  let env_pin_path = try Sys.getenv "PIN_HOME" with _ -> "" in
  if (env_pin_path <> "") then pin_path := env_pin_path;
  check_file(!pin_path^pin);
  check_file(gentrace_path^gentrace);
;;


(** Common functions across multipule tests **)
let rec find_funs ?(msg="") ranges names = match ranges with
  | [] -> assert_failure ("Could not find functions "^msg)
  | (n,s,e)::rs -> if (List.mem n names) then (s,e) else find_funs ~msg rs names;;

let rec find_fun ?(msg="") ranges name = find_funs ~msg ranges [name]

let rec find_call prog = 
  match prog with
  | [] -> assert_failure "Could not find a call in the given function"
  | p::ps ->
    match p with
    | Ast.Label(Addr(a), [Asm(asm)]) -> 
      if (pmatch ~pat:"call" asm) then a else find_call ps
    | _ -> find_call ps;;


(* Return list of statments between start_addr and end_addr *)
let inject_stmt prog start_addr asm stmt = 
  let rec inject_stmt_k stmts starta asm_str inj_stmt k =
    match stmts with
    | [] -> 
      assert_failure ("Could not find asmembly instruction "^asm_str^" in main")
    | s::[] -> 
      assert_failure ("Could not find asmembly instruction "^asm_str^" in main")
	(* Match for the addr and label at the same time *)
    | s::l::ss -> 
      match starta with
      | Some(a) -> (match s with
	| Ast.Label(Type.Addr(addr),attrs) -> 
	  if (addr = a) then inject_stmt_k ss None asm_str inj_stmt (l::s::k)
	  else inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
	| _ -> inject_stmt_k (l::ss) starta asm_str inj_stmt (s::k)
      )
	  (* We are inside the desired block; find asm_str and inject inj_stmt *)
      | None -> (match s with
	| Ast.Label(Type.Addr(addr),attrs) -> 
	  (match attrs with
	  | [Type.Asm(asm)] ->
	    if (pmatch ~pat:asm_str asm) then (List.rev k)@(s::l::inj_stmt::ss)
	    else inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
	  | _ -> inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
	  )
	| _ -> inject_stmt_k (l::ss) starta asm_str inj_stmt (s::k)
      )
  in
  inject_stmt_k prog (Some(start_addr)) asm stmt [];;


let halt_stmt = Halt(exp_true,[]);;


let check_bigint_answer e correct = 
  match e with
  | Int(int,_) -> if (int <>% correct)
    then 
      assert_failure 
	("Final value in EAX " ^ (string_of_big_int int) 
	 ^ " does not equal correct value "
	 ^ (string_of_big_int correct))
    else ()
  | _ -> assert_failure ("Final value in EAX is not an Ast.Int!");;


let check_eax ctx eax =
  Var.VarHash.iter 
    (fun k v ->
      match v with
      | Symbeval.Symbolic e when k = Disasm_i386.eax ->
	check_bigint_answer e eax
      | _ -> ()
    ) ctx.Symbeval.delta;;


let check_functions msg ranges names =
  ignore(List.map (find_fun ~msg ranges) names);;

let typecheck p = Typecheck.typecheck_prog p;;

(* Return list of statments between start_addr and end_addr *)
let find_prog_chunk prog start_addr end_addr = 
  let rec find_prog_chunk_k prog starta enda k =
    match prog, starta with
    (* Even if we don't hit the end address, return what we had so far *)
    | [], None -> k
    | [], _ -> raise (RangeNotFound (start_addr, end_addr))
    | p::ps, Some a ->
      (match p with
      | Ast.Label(Addr(addr),attrs) when addr = a -> 
	(* If this is the start address we are looking for begin recording 
	   with accumulator k.  Set starta to None so that we know we are in
	   the desired range *)
	find_prog_chunk_k ps None enda (p::k)
      | _ -> find_prog_chunk_k ps starta enda k)
    | p::ps, None ->
      (* Indicates we are inside desired block; return through end_addr *)
      (match p with
      | Ast.Label(Addr(addr),attrs) when addr = enda -> 
        k
      | _ -> find_prog_chunk_k ps starta enda (p::k))
  in
  List.rev (find_prog_chunk_k prog (Some start_addr) end_addr [])

let summarize r =
  match r with 
  | RError(p,s) ->
	Format.printf "Error: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RFailure (p,s) ->
	Format.printf "Failure: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RSkip (p,s) -> 
	Format.printf "Skiped: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RTodo (p,s) -> 
	Format.printf "Todo: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RSuccess p -> ();;

let rec summarize_results res =
  match res with
  | [] -> None
  | r::rs -> summarize r; summarize_results rs;;

let backwards_taint prog =
  let prog = Traces.concrete prog in

  (* Flatten memory *)
  let prog = Flatten_mem.flatten_mem_program prog in

  (* Try to identify fault *)
  let fault_location = Traces_backtaint.identify_fault_location prog in
  Traces_backtaint.backwards_taint prog fault_location
