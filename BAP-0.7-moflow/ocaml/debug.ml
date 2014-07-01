let logfile = ref stderr
let set_logfile log = logfile := log

let debug_string s = output_string !logfile s
let debug_endline s =
  debug_string s;
  debug_string "\n";
  flush !logfile

(* Disabling global debugging turns off all debug functionality for all modules *)
let global_debug = ref true
let set_global_debug v = global_debug := v

(* Use the appropriate logfile, as per environment variables *)
let () =
  try
    let filename = Sys.getenv "BAP_LOGFILE" in
    set_logfile (open_out_gen [Open_append;Open_creat] 0o664 filename)
  with Not_found -> ()


let get_env_options varname defvalue = 
  let default _ = defvalue in 
    try
      let modules = Sys.getenv varname in
      let len = String.length modules in
      let lookup name =
	let rec f spos =
	  let npos =
	    try String.index_from modules spos ':'
	    with Not_found -> len
	  in
	  if npos = spos then true else
	    let (sub, res) =
	      if String.get modules spos = '!'
	      then (String.sub modules (spos+1) (npos-spos-1), false)
	      else (String.sub modules spos (npos-spos), true)
	    in
	    if sub = "" || sub = name then res
	    else if npos < len then f (npos+1) else defvalue
	in
	f 0
      in
      lookup
    with Not_found ->
      default


(* [has_debug d s] returns true when debugging is enabled for s.  d is the
    default behavior.
    See documentation on [BAP_DEBUG_MODULES] at the top.
*)
let has_debug d s =
  (*get_env_options "BAP_DEBUG_MODULES"*)
  if !global_debug then get_env_options "BAP_DEBUG_MODULES" d s
  else false

(* [has_warn s] returns true when warnings are enabled for s.
    See documention on [BAP_WARN_MODULES] at the top *)
let has_warn = 
  get_env_options "BAP_WARN_MODULES" true


let indent = ref 0
let inc_indent () = indent := !indent + 1
let dec_indent () = indent := !indent - 1
let pindent () =
  let tab_is = 4 in (* number of "normal" indents *)
  let rec helper = function
    | 0 -> ()
    | n when n >= tab_is -> (debug_string "\t"; helper (n-tab_is))
    | n -> (debug_string "  "; helper (n-1))
  in
  helper (!indent)



let ptime_unix() =
  debug_string(Printf.sprintf "[%.3f]" (Unix.gettimeofday()))

let ptime_iso() =
  let secs = Unix.gettimeofday() in
  let t = Unix.localtime secs in
  Printf.ksprintf debug_string "[%4d-%2d-%2d_%2d:%2d:%2d]"
    t.Unix.tm_year t.Unix.tm_mon t.Unix.tm_mday
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let ptime_none() = ()

let ptime =
  try
    match Sys.getenv "BAP_DEBUG_TIMESTAMPS" with
    | "" | "unix" | "Unix" -> ptime_unix
    | "iso" | "ISO" -> ptime_iso
    | "none" | "None" -> ptime_none
    | "elapsed" ->
	let t0 = Unix.gettimeofday () in
	(fun () ->
	   Printf.ksprintf debug_string "[%f]" ((Unix.gettimeofday ()) -. t0) )
    | s ->
	prerr_endline("Warning: Unknown BAP_DEBUG_TIMESTAMPS value ("^s^")");
	ptime_unix
  with Not_found ->
    ptime_none

module type DEBUG =
sig
  val debug : unit -> bool

  val warn : bool

  val pdebug : string -> unit

  val dprintf : ('a, unit, string, unit) format4 -> 'a

  val dtrace : before:('a->unit) -> f:('a->'b) -> after:('b->unit) -> 'a -> 'b

  (* more should get added here *)

  val pwarn : string -> unit

  val wprintf : ('a, unit, string, unit) format4 -> 'a

end

module type DEBUG_MOD_INFO =
sig
  val name : string

  val default : [ `Debug | `NoDebug ]
end

module NoDebug : DEBUG  =
struct
  let debug _ = false
  let pdebug = (fun _ -> ())
  let dprintf fmt = Printf.ksprintf ignore fmt
  let dtrace = (fun ~before ~f ~after -> f)
  let warn = false
  let pwarn  = (fun _ -> ())
  let wprintf fmt = Printf.ksprintf ignore fmt
end

module Make(Module:DEBUG_MOD_INFO)  : DEBUG =
struct

  let debug _ = has_debug (Module.default = `Debug) Module.name 

  let pdebug s =
    pindent(); ptime();
    debug_string Module.name; debug_string ": ";
    debug_endline s
  let pdebug a = if debug() then pdebug a else NoDebug.pdebug a

  let dprintf fmt = Printf.ksprintf pdebug fmt
  let dprintf a = if debug() then dprintf a else NoDebug.dprintf a

  let dtrace ~before ~f ~after x =
    before x;
    inc_indent ();
    let r = f x in
    dec_indent ();
    after r;
    r
 
  let dtrace = if debug() then dtrace else NoDebug.dtrace

  let warn = has_warn Module.name

  let pwarn s = 
    pindent(); ptime();
    debug_string "WARNING (";
    debug_string Module.name; 
    debug_string "): ";
    debug_endline s
    
  let pwarn = if warn then pwarn else NoDebug.pwarn

  let wprintf fmt = Printf.ksprintf pwarn fmt
  let wprintf = if warn then wprintf else NoDebug.wprintf

end
