module L = List
module Hsh = Hashtbl
module Pr = Printf
(* BAP *)
open Big_int_convenience
module Ty = Type
module Tr = Traces
module TC = Tr.TraceConcrete
module TB = Traces_backtaint
module Sy = Symbeval
module Asm = Asmir
module Utl = Util
module Bi = Big_int_Z
module Dis = Disasm_i386 (* Reg vars are defined here. *)
(* self *)
module Cm = Common
open Cm.D (* dprintf *)

module TS = Tr.TraceForwardS
module MemL = TS.MemL (* SymbolicMemL *)
module Trk = TS.Trk (* Tracker *)

let (+!) = Int64.add 
let (-!) = Int64.sub

let str_to_int64 s n = 
  let res = ref 0L in
  for i = 0 to n do
    let b = Char.code s.[i] in
    res := Int64.logor !res (Int64.shift_left (Int64.of_int b) (8*i))
  done;
  !res
    
let make_tag s = 
  let rev str =
    let l = Str.split (Str.regexp "") str in
    List.fold_left (fun a b -> b ^ a) "" l 
  in
  let r = rev s in
  str_to_int64 r 3

let tAG_START = make_tag "SNAP" 
let tAG_REGS = make_tag "REGS"
let tAG_TMAP = make_tag "TMAP"
let tAG_RGNS = make_tag "RGNS"

let get_dword inch =
  let res = ref 0L in
  for i = 0 to 3 do
    let byte = input_byte inch in
    res := Int64.logor !res (Int64.shift_left (Int64.of_int byte) (8*i))
  done;
  !res

let eat_hdr ic = 
  let tag = get_dword ic in
  let ver = get_dword ic in
  let _ = assert (tag = tAG_START) in
  let _ = assert (ver = Int64.one) in
  true

let eat_regs ic = 
  let eat_triple () = 
    let rid = get_dword ic in
    let v = get_dword ic in
    let taint = get_dword ic in
    (rid, v, taint)
  in
  let eat_triples n = 
    let id_eflags = 8L in
    let rid2reg = function
      | 0L -> Dis.eax
      | 1L -> Dis.ebx
      | 2L -> Dis.ecx
      | 3L -> Dis.edx
      | 4L -> Dis.esi
      | 5L -> Dis.edi
      | 6L -> Dis.ebp
      | 7L -> Dis.esp
      | 8L -> failwith "rid2reg: EFLAGS is handled somwhere else"
      | 9L -> Dis.eip 
      | _ -> failwith "No such rid"
    in
    let handle_eflags eflags =
      (* Use ~f: parameter to 'post process' flags *)
      let one_flag ?f:(post = fun x->x) (flag_var, bit_pos) =
        let v = Int64.shift_right eflags bit_pos in
        let b = Int64.logand v 1L in
        let b = Bi.big_int_of_int64 b in
        let b = post ( Ast.Int(b, Ty.Reg 1) ) in
        (flag_var, Sy.Symbolic(b))
      in
      let post_dflag = function
        | Ast.Int(x, _) -> 
          if x = bi0 then  
            Ast.Int(bi1, Ast.reg_32)
          else if x = bi1 then 
            Ast.Int(bim1, Ast.reg_32)
          else
            failwith "post_dflag: unexpected constant"
        | _ -> failwith "post_dflag: unexpected expression"
      in
      (* df needs special handling *)
      let flags = [(Dis.cf, 0); (Dis.pf, 2); (Dis.af, 4); (Dis.zf, 6); 
                   (Dis.sf, 7); (Dis.oF, 11);] in
      let flags = L.map one_flag flags in
      let dflag_pair = one_flag ~f:post_dflag (Dis.dflag, 10) in
      dflag_pair::flags
    in
    let rec aux reg_val_pairs tainted = function
      | 0 -> reg_val_pairs, tainted
      | n -> 
        let (rid, v, taint) = eat_triple () in
        if rid = id_eflags then 
          let flag_val_pairs = handle_eflags v in
          let l = flag_val_pairs @ reg_val_pairs in
          aux l tainted (n-1)
        else
          let reg = rid2reg rid in
          let v = Bi.big_int_of_int64 v in
          let v = Sy.Symbolic(Ast.Int(v, Ty.Reg 32)) in
          let reg_val_pairs = (reg,v)::reg_val_pairs in
          let tainted = 
            if taint>0L then TS.Trk.LocSet.add (TS.Trk.Loc.V reg) tainted 
            else tainted
          in
          aux reg_val_pairs tainted (n-1)
    in
    let reg_val_pairs = [] in
    let taint = TS.Trk.empty in
    aux reg_val_pairs taint n 
  in
  let tag = get_dword ic in
  let _ = assert (tag = tAG_REGS) in
  let n = get_dword ic in
  if n>10L then failwith "eat_regs: too many regs" 
  (* return delta, taint pair *)
  else eat_triples (Int64.to_int n)

let get_pair ic = 
  let x = get_dword ic in
  let y = get_dword ic in
  x,y

let eat_taint ic = 
  let eat_pairs n = 
    let rec aux tainted = function
      | 0 -> tainted
      | n -> 
        (* All stored addresses are tainted, so just skip the snd dword. *)
        let v,_ = get_pair ic in
        let v = Bi.big_int_of_int64 v in
        let tnt = TS.Trk.LocSet.add (TS.Trk.Loc.M v) tainted in
        aux tnt (n-1)
    in
    aux TS.Trk.LocSet.empty n
  in
  let tag = get_dword ic in
  let _ = assert (tag = tAG_TMAP) in
  let n = get_dword ic in
  (* return set of tainted addresses *)
  eat_pairs (Int64.to_int n)

let eat_regions ic = 
  let eat_pairs n = 
    let rec aux rgns = function
      | 0 -> rgns
      | n -> 
        let low, size = get_pair ic in
        aux ((low,size)::rgns) (n-1)
    in
    L.rev (aux [] n)
  in
  let build_va_to_file_offset rgns pos = 
    let rec aux f rgns pos = 
      match rgns with
      | (low,size)::tl -> 
        let g va = if low<=va && va<low +! size then (va-!low)+!pos else f va in
        aux g tl (pos+!size)
      | [] -> f
    in
    let bad_va va = raise Not_found in
    aux bad_va rgns pos
  in
  let tag = get_dword ic in
  let _ = assert (tag = tAG_RGNS) in
  let n = get_dword ic in
  let rgns = eat_pairs (Int64.to_int n) in
  (* ic is now at the beginning of the first region *)
  let pos = Int64.of_int (pos_in ic) in
  let va2off = build_va_to_file_offset rgns pos in
  va2off

let build_va_reader ic va2off = 
  let read_byte va = 
    let pos1 = va2off va in
    let pos2 = Int64.to_int pos1 in
    let _ = seek_in ic pos2 in
    input_byte ic
  in
  read_byte

(* 
 * Return values:
 * delta - (reg, value) pairs
 * taint - set of tainted variables (LocSet type)
 * read_byte - memory reading function (takes int64, returns int)
 *)
let parse_snapshot fn = 
  let ic = Cm.open_file_in fn in
  let _ = eat_hdr ic in
  let delta, taint1 = eat_regs ic in
  let taint2 = eat_taint ic in
  let taint = TS.Trk.LocSet.union taint1 taint2 in
  let va2off = eat_regions ic in
  let read_byte = build_va_reader ic va2off in
  delta, taint, read_byte

let forward_eval reg_vals taint read_byte dot_fn jd id = 
  let pred (reg, v) = reg = Dis.eip in
  let unpack = function
    | Sy.Symbolic(Ast.Int(v, Ty.Reg 32)) -> Big_int_Z.int64_of_big_int v
    | _ -> failwith "Can't unpack EIP value"
  in
  (* Exception address *)
  let (_, eip_v) = 
    try L.find pred reg_vals 
    with Not_found -> failwith "Can't find EIP in snapshot" 
  in
  let exc_addr = unpack eip_v in
  let g = TS.forward_exec exc_addr reg_vals taint read_byte jd id in
  let _ = TS.to_dot g dot_fn in
  ()

let test_read read_byte = 
  let top = 0xff944 in
  let top = Int64.of_int top in
  let top = Int64.shift_left top 12 in
  let n = Int64.of_int 0x100 in
  let va = top -! n in
  let rec aux va = 
    let b = read_byte va in
    let _ = Pr.printf "va=%Lx, b=%02x\n" va b in
    if va < top then aux (Int64.succ va) else 1
  in
  aux va

(* Return filtered reg_vals pairs and read_byte that raises Not_found for
 * tainted addresses. *)
let turn_tainted_to_symbolic reg_vals taint read_byte = 
  let is_tainted reg = TS.Trk.LocSet.mem (TS.Trk.Loc.V reg) taint in
  let reg_vals = L.filter (fun (reg,v) -> not (is_tainted reg)) reg_vals in
  (* Now filter read_byte, so that all tainted reads return symb. vars *)
  let symb_read va = 
    let is_tainted_mem v = 
      let v = Bi.big_int_of_int64 v in
      TS.Trk.LocSet.mem (TS.Trk.Loc.M v) taint
    in
    if is_tainted_mem va then raise Not_found 
    else read_byte va 
  in
  reg_vals, symb_read

let main () =
  let usage = "Usage: "^Sys.argv.(0)^" <options>\n" in
  let sNAPSHOT_FN = ref "" in
  let dOT_FN = ref "" in
  let sYMB_TAINT = ref false in
  let jUMP_DEPTH = ref 5 in
  let iNSTRUCTION_DEPTH = ref 16 in
  let spec = [
    ("-sn", Arg.Set_string(sNAPSHOT_FN), "-- <snapshot file>");
    ("-dot", Arg.Set_string(dOT_FN), "-- <output dot file>");
    ("-symb_taint", Arg.Set(sYMB_TAINT), "-- make all tainted vars symbolic");
    ("-jd", Arg.Set_int(jUMP_DEPTH), "-- <jump depth> (default: 5)");
    ("-id", Arg.Set_int(iNSTRUCTION_DEPTH), "-- <instruction depth> (default: 16)");
  ] in
  let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'")) in
  let _ = Arg.parse spec anon usage in
  let _ = 
    if !sNAPSHOT_FN = "" then (
      Arg.usage spec "No snapshot file. Pass one with -sn";
      exit 1
    )
    else if !dOT_FN = "" then (
      Arg.usage spec "Not dot file name. Pass it with -dot";
      exit 1
    )
    else ()
  in
  let reg_vals, taint, read_byte = parse_snapshot !sNAPSHOT_FN in
  (* let _ = test_read read_byte in *)
  (* let _ = assert false in *)
  let reg_vals, read_byte = 
    if !sYMB_TAINT then turn_tainted_to_symbolic reg_vals taint read_byte 
    else reg_vals, read_byte
  in
  let _ = forward_eval reg_vals taint read_byte !dOT_FN !jUMP_DEPTH 
          !iNSTRUCTION_DEPTH in
  ()

let _ = main()

