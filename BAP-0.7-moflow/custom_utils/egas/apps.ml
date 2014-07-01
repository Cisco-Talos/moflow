module Ux = Unix
module Arr = Array
module Cm = Common

(* We can have only 1 child at any moment. *)
let cHILD_PID = ref None

let bAP_DIR = "/home/p/bap-0.7" 
(* file to taint, trace filename, app, params.
 * params:string array*)
let make_pin_cmd sample_fn trace_fn app params = 
  let p1 = [|"pin"; "-t"; bAP_DIR^"/pintraces/obj-ia32/gentrace.so"; 
            "-taint_files"; sample_fn; "-o"; trace_fn; "--"; app; |] in
  Arr.append p1 params

(* params:string array *) 
let make_pin_cov_cmd bb_fn exn_fn visited_bb_file app params = 
  let p1 = [|"pin"; "-t"; bAP_DIR^"/pintraces/obj-ia32/gentrace.so"; "-bb-file";
            bb_fn; "-exn-file"; exn_fn; "-visited-bb-file"; visited_bb_file; 
            "--"; app; |] in
  Arr.append p1 params

(* file with trace, output file with concretized trace. 
 * Concretize, remove dead code. *)
let make_iltrans_cmd trace_fn ctrace_fn = [|"iltrans"; "-serializedtrace"; 
  trace_fn; "-trace-concrete-subst"; "-trace-dce"; "-pp-ast"; ctrace_fn; |]

let spawn_process prog args =
  let _ = assert (Sys.os_type = "Unix") in
  let pid = Ux.create_process prog args Ux.stdin Ux.stdout Ux.stderr in
  cHILD_PID := Some(pid);
  let (dead_pid, status) = Ux.wait () in
  let _ = assert (pid = dead_pid) in
  match status with
  | Ux.WEXITED n -> n
  | _ -> failwith "spawn_process: unexpected status"

(* arr - array of arguments *)
let run_cmd arr =
  let n = Arr.length arr in
  let _ = assert (n>1) in
  let prog = Arr.get arr 0 in
  let err = spawn_process prog arr in
  match err with
  | 0 -> true
  | _ -> 
    let cmd = Cm.arr_to_str arr in
    failwith ("Cmd failed:\n"^cmd^"\n")

let kill_child () = 
  match !cHILD_PID with
  | Some(pid) -> Ux.kill pid Sys.sigkill 
  | None -> ()

let concretize_trace trace_fn ctrace_fn = 
  let arr = make_iltrans_cmd trace_fn ctrace_fn in
  let _ = run_cmd arr in
  ()

let trace_sample sample_fn trace_fn app params =
  let arr = make_pin_cmd sample_fn trace_fn app params in
  let _ = run_cmd arr in
  ()

let trace_coverage bb_fn exn_fn visited_bb_file app params = 
  let arr = make_pin_cov_cmd bb_fn exn_fn visited_bb_file app params in
  let _ = run_cmd arr in
  ()
