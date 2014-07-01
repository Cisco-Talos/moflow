module L = List
module Pr = Printf
module Hsh = Hashtbl
module Ux = Unix
module Sys = Sys
module Arr = Array
(* BAP *)
module Ast = Ast
module Ty = Type
module Tr = Traces
module U = Util
module TrSur = Traces_surgical
(* Self *)
module La = Mylazy
module Cm = Common
module Apps = Apps
module Stats = Stats
module H = Heap
module D = Cm.D
module Ora = Oracle

(* Register timers *)
let tmr_trace_bpt = new Stats.timer "taint tracing the target (produces .bpt)"
let tmr_trace_cov = new Stats.timer "gathering coverage info"
let tmr_ctrace = new Stats.timer ".bpt concretization"
let tmr_symb_exec = new Stats.timer "symbolic execution"

let timers = [tmr_trace_bpt; tmr_trace_cov; tmr_ctrace; tmr_symb_exec;] 
let _ = L.map (fun t -> Stats.register_timer t) timers

module Int64Set = Set.Make(Int64)

type sample = {id: int; prio: int; crashed: bool; bbs: Int64Set.t}

module Prio = 
struct
    type t = {prio:int; e:sample} (* prio, leaf id *)
    let elem x = x.e
    let prio x = x.prio
    let make p e = {prio=p; e=e}
    let compare x y = Pervasives.compare (x.prio) (y.prio)
end

(* imperative priority queue using heaps *)
module PQ = H.Imperative(Prio)

module Box = Ora.BoxZ3Opti

(*************************************************************)
(*************************************************************)

let iNF_PRIORITY = int_of_float (10.0**6.0-.1.0)
let sAMPLE_FN_SIG = "%s"

(* Controlled from cmd line *)
let fLIP_LIMIT = ref 128
let fLIP_NUM = ref None
let sEED_FILE = ref ""
let tARGET_APP = ref ""
let cMD_LINE_FMT = ref "%s"
let sAMPLES_DIR = ref "samples"
let cRASHES_DIR = ref "crashes"

let n_COUNTER = ref 0

(* Global set of BBs saved to the log file. *)
let aLL_BBS = ref (Int64Set.empty)

let cur_fn_id () = !n_COUNTER;;
let inc_fn_id () = 
  n_COUNTER := !n_COUNTER + 1;;

let new_id () = 
  let id = cur_fn_id () in
  let _ = inc_fn_id () in
  id

let gen_fn ?dir:(dir=(!sAMPLES_DIR)) ?pref:(pref="") id suff = 
  (* let _ = assert (id < 10000) in *)
  let fn = Printf.sprintf "%s%d%s" pref id suff in
  let fn = dir^"/"^fn in
  fn 
   
(* List of visited BBs. Tracer will use this info. *)
let gen_all_bb_fn () = (!sAMPLES_DIR)^"/all.bbs";;

let gen_sol_fn id = gen_fn id ".sol"
let gen_trace_fn id = gen_fn id ".bpt"
let gen_ctrace_fn id = gen_fn id ".conc.il"
let gen_bb_fn id = gen_fn id ".bb"
let gen_exn_fn id = gen_fn id ".exn"

let gen_crash_fn id = 
  let t = Ux.time () in
  let tm = Ux.localtime t in
  let y = tm.Ux.tm_year + 1900 in
  let m = tm.Ux.tm_mon + 1 in
  let d = tm.Ux.tm_mday in
  let hh = tm.Ux.tm_hour in
  let mm = tm.Ux.tm_min in
  let ss = tm.Ux.tm_sec in
  let stamp = Pr.sprintf "%d-%02d-%02d_%02d:%02d:%02d_" y m d hh mm ss in
  gen_fn ~dir:(!cRASHES_DIR) ~pref:stamp id ".crash" 

let gen_fn_names id = 
  let sol_fn = gen_sol_fn id in
  let trace_fn = gen_trace_fn id in
  let ctrace_fn = gen_ctrace_fn id in
  (sol_fn, trace_fn, ctrace_fn)

let format_params sample_fn = 
  let swap thing repl = 
    if thing = sAMPLE_FN_SIG then repl else thing
  in
  let params = Cm.str_to_arr !cMD_LINE_FMT in
  let params = Arr.map (fun thing -> swap thing sample_fn) params in
  params

(* Trace target app with sample file specified by id. 
 * This produces .bpt trace.
 *)
let trace_sample_for_bpt id = 
  tmr_trace_bpt#start;
  let (sample_fn, trace_fn, _) = gen_fn_names id in
  let params = format_params sample_fn in
  D.dprintf "Tracing sample: %d" id;
  Apps.trace_sample sample_fn trace_fn !tARGET_APP params;
  D.dprintf "Done";
  tmr_trace_bpt#stop

(* Produce coverage info (.bb file) and .exn file (if there was an exception).*)
let trace_coverage id = 
  tmr_trace_cov#start;
  D.dprintf "Coverage tracing sample: %d" id;
  let sample_fn = gen_sol_fn id in
  let bb_fn = gen_bb_fn id in
  let exn_fn = gen_exn_fn id in
  let params = format_params sample_fn in
  let all_bb_fn = gen_all_bb_fn () in
  Apps.trace_coverage bb_fn exn_fn all_bb_fn !tARGET_APP params;
  D.dprintf "Done";
  tmr_trace_cov#stop

(* Turn .bpt trace into .conc.il *)
let concretize_trace sample_id = 
  tmr_ctrace#start;
  let (_, trace_fn, ctrace_fn) = gen_fn_names sample_id in
  D.dprintf "Concretizing trace: %s" trace_fn;
  let _ = Apps.concretize_trace trace_fn ctrace_fn in
  D.dprintf "Done";
  tmr_ctrace#stop

(* Return a list of branches ((direction:bool, label:Ty.Label) list) and a 
 * hashtable translating tainted var ids to file offsets *)
let get_branches_and_taint_info prog =
  let assert_type = function
    (* FIXME: hax. true branch *)
    | Ast.UnOp(Ty.NOT, Ast.UnOp(Ty.NOT, _)) -> true
    (* false branch was taken *)
    | Ast.UnOp(Ty.NOT, _) -> false
    (* true branch was taken *)
    | Ast.BinOp(_, _, _) -> true
    (* true branch *)
    | Ast.Var(v) ->
      assert (Var.typ v = Ty.Reg(1));
      true
    | exp -> 
      let exps = Pp.ast_exp_to_string exp in
      failwith ("Unexpected exp type: "^exps)
  in
  let save_taint_intro h = function
    | Ty.TaintIntro(tid, _, offset) -> Hsh.add h tid offset
    | _ -> ()
  in
  let make_fake_lbl lbl cnt =
    let s = 
      match lbl with
      | Ty.Name s -> Pr.sprintf "%s_%d" s cnt
      | Ty.Addr a -> Pr.sprintf "pc_%Lx_%d" a cnt
    in
    Ty.Name(s)
  in
  (* Extract all info we need, in one pass *)
  let tid2off = Hsh.create 256 in
  let lbl_asserts_and_taints prog =
    let rec aux acc last_lbl fake_lbl_cnt prog =
      match prog with
      | Ast.Label(lbl,_)::Ast.Assert(e,_)::tl ->
        (* Reset label counter to 0 *)
        aux ((lbl, e)::acc) lbl 0 tl
      (* Lone assert. Create a synthetic label based on last seen *)
      | Ast.Assert(e,_) as s::tl -> 
        let s = Pp.ast_stmt_to_string s in
        D.dprintf "%s" ("Lone assert: " ^ s);
        let lbl = make_fake_lbl last_lbl fake_lbl_cnt in
        aux ((lbl, e)::acc) last_lbl (fake_lbl_cnt+1) tl
      (* Extract TaintIntro attributes *)
      | Ast.Comment(_, atts)::tl -> 
        let _ = L.map (save_taint_intro tid2off) atts in
        aux acc last_lbl fake_lbl_cnt tl
      | _::tl -> aux acc last_lbl fake_lbl_cnt tl
      | [] -> List.rev acc
    in
    let fake_lbl = Ty.Name("pc_0") in
    let fake_lbl_counter = 0 in
    aux [] fake_lbl fake_lbl_counter prog
  in
  let lbl_dir = lbl_asserts_and_taints prog in
  let dir_lbl = List.map (fun (lbl,e)->(assert_type e, lbl)) lbl_dir in
  dir_lbl, tid2off

(* Execute a trace symbolically to get a formula and gather branches *)
let symb_exec_concrete_trace trace = 
  tmr_symb_exec#start;
  (* do not add any debug asserts to the formulas *)
  let _ = assert (not !Tr.consistency_check) in
  (* Make shure only jumps generate asserts *)
  let _ = assert (!Tr.assert_only_jumps) in
  let ctx = Box.symbolic_run [] trace in
  (* List of asserts. Different Branch modules can use different formats
   * for encoding them. *)
  let preds = Box.output_formula ctx in
  (* (type_of_branch:bool, jump's address:int64).list
   * type=true iff true branch was taken
   * type=false otherwise. *)
  let branches, tid2off = get_branches_and_taint_info trace in
  try
    let mk_branch = fun (f_exp,(dir,lbl)) -> Box.mk_branch lbl dir f_exp in
    let l = L.combine preds branches in
    let branches = L.map mk_branch l in
    tmr_symb_exec#stop;
    branches, tid2off
  with Invalid_argument err -> 
    failwith ("preds/branches length mismatch. " ^ err)

let gen_new_sample parent_id tid2off model = 
  let parent_sol = gen_sol_fn parent_id in
  let size, parent_str = Cm.read_string parent_sol in
  (* Set file bytes according to model. String is modified in place. *)
  let patch_byte str (tid, value) =
    let _ = assert (0<= value && value <= 255) in
    let _ = assert (Hsh.mem tid2off tid) in
    let offset = Hsh.find tid2off tid in
    let _ = assert (offset < size) in
    str.[offset] <- Char.chr value
  in
  let gen model = 
    let id = new_id () in
    let new_sol = gen_sol_fn id in
    (* Do it this way, because Batteries don't implement seek_out *)
    let str = String.copy parent_str in
    let _ = L.map (patch_byte str) model in
    Cm.write_string new_sol str;
    id
  in
  let id = gen model in
  id

let save_crashing_sample sample_id = 
  let sample_fn = gen_sol_fn sample_id in
  let crash_fn = gen_crash_fn sample_id in
  Pr.printf "CRASH! Sample: %s saved as %s" sample_fn crash_fn;
  Cm.copy_file sample_fn crash_fn

(* Extract a set of basic blocks visited in a trace.
 * Returns: bbs:Int64Set.t *)
let extract_bbs sample_id =
  let bb_fn = gen_bb_fn sample_id in
  let strs = Cm.read_all_lines bb_fn in
  let rec aux bbs strs = 
    match strs with
    | s::tl ->
      let bbs = 
        let bb = Int64.of_string s in
        Int64Set.add bb bbs 
      in
      aux bbs tl
    | [] -> bbs
  in
  let bbs = aux (Int64Set.empty) strs in
  bbs

let check_for_exn sample_id = 
  let exn_fn = gen_exn_fn sample_id in
  Sys.file_exists exn_fn

(* All bbs / bbs from current sample *)
let number_of_new_bbs all_bbs bbs = 
  let module S = Int64Set in
  let new_bbs = (S.diff bbs all_bbs) in
  S.cardinal new_bbs, new_bbs

(* Append new BB addresses to log file. *)
let append_new_bbs fn all_bbs new_bbs =  
  let append_strings fn strings = 
    let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 fn in
    let _ = L.map (fun s -> output_string oc (s^"\n")) strings in 
    close_out oc
  in
  let module S = Int64Set in
  let new_bbs = S.diff new_bbs all_bbs in
  let elts = S.elements new_bbs in
  let max = Int64.shift_left 1L 32 in
  let all_small = L.for_all (fun x -> x<max) elts in
  (* gentrace.cpp assumes ADDRINT is 32bit. *)
  let _ = assert all_small in
  let strings = L.map (fun bb -> Pr.sprintf "0x%Lx" bb) elts in
  let _ = append_strings fn strings in
  S.union all_bbs new_bbs

(* Coverage tracing, check for exception, calculate priority, save new BBs 
 * to a file. *)
let sample_from_id ?all_bbs:(all_bbs=Int64Set.empty) sample_id = 
  D.dprintf "Calculating priority of sample %d" sample_id;
  let _ = trace_coverage sample_id in
  let bbs = extract_bbs sample_id in
  let seen_exn = check_for_exn sample_id in
  (* let bbs = Int64Set.empty in *)
  (* How many new basic blocks does this sample explore? *)
  let prio, new_bbs = number_of_new_bbs all_bbs bbs in
  D.dprintf "Done. Priority: %d" prio;
  (* Save new BBs to a file. *)
  let all_bb_fn = gen_all_bb_fn () in
  aLL_BBS := append_new_bbs all_bb_fn (!aLL_BBS) new_bbs;
  (* Did the app crash? *)
  let _ = if seen_exn then save_crashing_sample sample_id else () in
  let sample = {prio=prio; id=sample_id; bbs=bbs; crashed=seen_exn} in
  sample

(* Generate new sample, calculate its priority, append new BBs to a file. *)
let process_one_model all_bbs parent_id tid2off model = 
  match model with
  | Some(model) -> 
    let new_sample_id = gen_new_sample parent_id tid2off model in
    let sample = sample_from_id ~all_bbs new_sample_id in
    Some(sample)
  | None -> None

let print_some_list sample_id l = 
  let fh = open_out_gen [Open_text; Open_append] 0777 "z3opti.txt" in
  let fprintf = Pr.fprintf in
  let f = function
    | Some _ -> fprintf fh "1"
    | None -> fprintf fh "0"
  in
  fprintf fh "%d: " sample_id;
  let _ = L.map f l in
  fprintf fh "\n";
  close_out fh

let process_one_sample flip_single tree all_bbs sample_id trace = 
  let prepare_vars_for_single_flip single_num prefix suffix = 
    let _ = assert (L.length prefix = 0) in
    let prefix, suffix =
      try
        BatList.split_at single_num suffix
      with _ -> 
        let n = L.length suffix in
        let s = Pr.sprintf "prepare_vars_for_single_flip: %d>list len=%d" 
          single_num n in
        failwith s
    in
    let _ = assert (L.length suffix >=1) in
    (* This is the branch to flip *)
    let suffix = [L.hd suffix] in
    prefix, suffix
  in
  let maybe_die die_early = 
    if die_early then 
      failwith "Branch flipped. Exiting."
    else ()
  in
  (* List of branch descriptions, including their asserts.
   * tid2off translates symbolic var ids to file offsets *)
  let branches, tid2off = symb_exec_concrete_trace trace in
  let tree, r = Box.split_by_and_grow_leaf tree sample_id branches in
  let process_one_model = process_one_model all_bbs sample_id tid2off in
  match r with 
  | Some(prefix, suffix, unks) ->
    let die_early, prefix, suffix = 
      (match flip_single with 
      | Some(num) -> 
        let p, s = prepare_vars_for_single_flip num prefix suffix in
        true, p, s
      | None -> false, prefix, suffix 
    ) in
    (* FIXME: limit the number of branches to flip, to avoid stack overflow *)
    let suffix = BatList.take 1024 suffix in
    let unks = BatList.take 1024 unks in
    let len1 =  L.length suffix in
    D.dprintf "%d branch(es) to flip" len1;
    (* List of models is lazy!
     * Solving is time consuming, so generating all models at once would
     * take a lot of time and block all other stages of processing (creating 
     * new sample files, tracing them, etc). *)
    let models = Box.solve_branches !fLIP_LIMIT prefix suffix in
    (* lazy list: new_samples: sample list *)
    let new_samples = La.map process_one_model models in
    (* Finally compute everything: 
     * - use a solver to generate a model
     * - generate new sample
     * - trace it
     * - calculate its priority (by collecting a trace and counting new bbs) *)
    let new_samples = La.force_all new_samples in
    (* If this is formula debug mode, then die early. *)
    let _ = maybe_die die_early in
    (* FIXME: hack *)
    let new_sample_ids = L.map (BatOption.map (fun s -> s.id)) new_samples in
    (* Now replace Unk nodes with SmallLeaf/Unsat nodes *)
    let tree = Box.replace_unks tree unks new_sample_ids in 
    let len1 = L.length new_sample_ids in
    let len2 = L.length unks in
    let _ = if len1 <> len2 then 
      let err = Pr.sprintf "len1: %d, len2: %d\n" len1 len2 in
      failwith err 
    in
    let _ = print_some_list sample_id new_samples in
    let new_samples = Cm.keep_some new_samples in
    let _ = D.dprintf "Number of new samples=%d\n" (L.length new_samples) in
    tree, new_samples
  (* Path diverged, so bail *)
  | None -> 
    tree, []

let remove_itermediate_files sample_id = 
  let (_, trace_fn, ctrace_fn) = gen_fn_names sample_id in
  try
    let _ = Sys.remove trace_fn in
    Sys.remove ctrace_fn 
  with _ ->
    ()

(* Debug version. Save IL to a file. *)
let concretize_to_file sample_id =
  let _ = concretize_trace sample_id in
  let ctrace_fn = gen_ctrace_fn sample_id in
  let prog = Cm.read_il_from_file ctrace_fn in
  prog

let concretize_to_mem sample_id =
  let trace_fn = gen_trace_fn sample_id in
  let trace = Asmir.serialized_bap_from_trace_file trace_fn in
  let trace = TrSur.concrete_substitution trace in
  let trace = Tr.trace_dce trace in
  trace

let driver prq = 
  let enqueue prio sample_id =
    let e = Prio.make prio sample_id in
    PQ.add prq e
  in
  let dequeue () = 
    let r = PQ.pop_maximum prq in
    let id = Prio.elem r in
    let p = Prio.prio r in
    p, id
  in
  (* Set holding BBs visited by processed samples *)
  let all_bbs = ref Int64Set.empty in
  (* Make a tree consisting of a single SmallLeaf. It's going to be expanded 
   * later in the main loop. *)
  let first = Prio.elem (PQ.maximum prq) in
  let tree = Box.tree_from_leaf first.id in
  let idx = ref 0 in
  while (not (PQ.is_empty prq) && !idx=0) do
    let prio, sample = dequeue () in
    let sid = sample.id in
    (* Producing .bpt traces is time consuming, so we do that only when 
     * necessary. *)
    let _ = trace_sample_for_bpt sid in
    let trace =
      if D.debug () then concretize_to_file sid 
      else concretize_to_mem sid 
    in
    (* let _ = Cm.pp_ast "o.txt" trace in *)
    (* let _ = assert false in *)
    all_bbs := Int64Set.union !all_bbs sample.bbs;
    let _ = D.dprintf "Total bbs: %d" (Int64Set.cardinal !all_bbs) in
    let _ = D.dprintf "Analyzing sample: %d, priority: %d" sid prio in
    (* If formula debugging mode is ON, we will flip a single branch and die *)
    let tree, new_samples = 
      process_one_sample !fLIP_NUM tree !all_bbs sid trace in
    let _ = D.dprintf "Number of new samples: %d" (List.length new_samples) in
    let _ = List.map (fun s -> enqueue s.prio s) new_samples in
    (* If debug mode is OFF, remove .bpt and .conc.il files. *)
    let _ = 
      if not (D.debug ()) then remove_itermediate_files sid  
      else ()
    in
    (* idx := 1; *)
    ()
  done;
  Box.export_graph tree "tree.dot"

let set_flip_num num = 
  match num with 
  | -1 -> fLIP_NUM := None
  | n -> fLIP_NUM := Some(n-1)

let report_stats () = 
  let t0 = Ux.time () in
  while true do
    let elapsed = Ux.time () -. t0 in
    let s = Stats.dump_timers elapsed in
    Pr.printf "%s\n" s;
    Pr.printf "elapsed: %f\n" elapsed;
    flush stdout;
    Thread.delay 1.0
  done

(* Kill children on SIGALRM, SIGINT. *)
let fatal_signal s = 
  Apps.kill_child ();
  exit 1

(* Make shure samples/crashes dirs exist. *)
let create_dirs dirs = 
  let f dir = 
    try 
      if Sys.is_directory dir then ()
      else failwith (Pr.sprintf "%s is not a directory" dir)
    with Sys_error _ ->
      Ux.mkdir dir 0o775
  in
  let _ = L.map f dirs in
  ()

let prep_all_bb_file all_bb_fn = 
  let _ = try
    (* Delete file with all visited BBS. *)
    Sys.remove all_bb_fn
    with _ -> ()
  in
  let oc = open_out all_bb_fn in
  close_out oc

let main () =
    let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle fatal_signal) in
    let _ = Sys.set_signal Sys.sigalrm (Sys.Signal_handle fatal_signal) in

    let usage = "Usage: "^Sys.argv.(0)^" <options>\n" in
    let flip_one_num = ref (-1) in
    let spec = [
        ("-seed", Arg.Set_string(sEED_FILE), "Set the seed file");
        ("-app", Arg.Set_string(tARGET_APP), "Set the target app");
        ("-fmt", Arg.Set_string(cMD_LINE_FMT), 
          "Set the command line for the target app");
        ("-samples-dir", Arg.Set_string(sAMPLES_DIR), 
          "Directory to save new samples and traces (default: ./samples)");
        ("-crashes-dir", Arg.Set_string(cRASHES_DIR), 
          "Directory to save crashing samples (default: ./crashes)");
        ("-flip-limit", Arg.Set_int(fLIP_LIMIT), 
          "Max. number of flips for a branch (default 128)");
        ("-flip-one", Arg.Set_int(flip_one_num), "Debug mode: flip just one
          branch and exit. Takes branch's number.");
    ] in
    let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'")) in
    let _ = Arg.parse spec anon usage in
    let _ = 
        if !sEED_FILE="" or !tARGET_APP="" then
            raise(Arg.Bad("Both -seed and -app must be set")) 
        else () 
    in
    let _ = set_flip_num !flip_one_num in
    let _ = create_dirs [!sAMPLES_DIR; !cRASHES_DIR] in
    (* Copy seed file to ./samples dir *)
    let id = new_id () in
    let sol_fn = gen_sol_fn id in
    let _ = Cm.copy_file !sEED_FILE sol_fn in
    (* Delete file with all visited BBs and create an empty one. *)
    let _ = prep_all_bb_file (gen_all_bb_fn ()) in
    (* Start stats reporting thread. *)
    let _ = Thread.create report_stats () in
    (* We need to compute coverage info and check if this sample crashes the
     * target. *)
    let first = sample_from_id id in
    (* Put first sample id into priority queue *)
    let prq = PQ.create 256 in
    let x = Prio.make first.prio first in
    PQ.add prq x;
    driver prq

let _ = main()
