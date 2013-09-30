(** High level interface to libasmir.

    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

open Asmir_consts
open Ast
open Big_int_Z
open BatListFull
open Frame_piqi
open Libasmir
open Libbfd
open Type
open Util

module BArray = Bigarray.Array1

exception Disassembly_error;;
exception Memory_error;;

type arch = Libbfd.bfd_architecture
type asmprogram = {asmp : Libasmir.asm_program_t;
		   arch : arch;
		   secs : section_ptr list;
                   (** Get executable code bytes *)
		   get_exec : int64 -> char;
                   (** Get any readable bytes. *)
                   get_readable : int64 -> char;
 }


let arch_i386 = Bfd_arch_i386
let arch_arm  = Bfd_arch_arm
(*more to come later when we support them*)

(** How many blocks to obtain when reading a FULL trace (not streaming) *)
let trace_blocksize = ref 100000L

module D = Debug.Make(struct let name = "Asmir" and default=`NoDebug end)
open D

module Status = Util.StatusPrinter

(* more verbose debugging *)
module DV = Debug.Make(struct let name = "AsmirV" and default=`NoDebug end)
(* module DCheck = Debug.Make(struct let name = "AsmirCheck" and default=`NoDebug end) *)

(* Debug output for testing*)
module DTest = Debug.Make(struct let name = "AsmirTest" and default=`NoDebug end)

(** Translate a unop *)
let tr_unop = function
  | Libasmir.NEG -> NEG
  | Libasmir.NOT -> NOT

(** Translate a type *)
let tr_regtype = function
  | Libasmir.REG_1   -> reg_1 
  | Libasmir.REG_8   -> reg_8 
  | Libasmir.REG_16  -> reg_16
  | Libasmir.REG_32  -> reg_32
  | Libasmir.REG_64  -> reg_64

(* maps a string variable to the var we are using for it *)
type varctx = (string,Var.t) Hashtbl.t

(** [gamma_create mem decls] creates a new varctx for use during translation.
    [mem] is the var that should be used for memory references, and [decls]
    should be a list of variables already in scope.
*)
let gamma_create mem decls : varctx =
  let h = Hashtbl.create 57 in
  List.iter (fun (Var.V(_,nm,_) as var) -> Hashtbl.add h nm var) decls;
  Hashtbl.add h "$mem" mem;
  Hashtbl.add h "mem" mem;
  h

let gamma_lookup (g:varctx) s =
  try Hashtbl.find g s
  with Not_found ->
    failwith("Disassembled code had undeclared variable '"^s^"'. Something is broken.")

let gamma_extend = Hashtbl.add

let gamma_unextend = Hashtbl.remove

(* Translate a string label into a name or address label as approriate *)
let tr_label s =
  Name s (* FIXME: treating them all as names for now *)

(** Translate an expression *)
let rec tr_exp g e =
  match Libasmir.exp_type e with
    | BINOP ->
	tr_binop g (Libasmir.binop_type e) (Libasmir.binop_lhs e) (Libasmir.binop_rhs e)
    | UNOP ->
        UnOp(tr_unop(Libasmir.unop_type e),
	     tr_exp g (Libasmir.unop_subexp e) )
    | CONSTANT ->
        Int(big_int_of_int64 (Libasmir.constant_val e), tr_regtype (constant_regtype e))
    | MEM ->
	let mem = gamma_lookup g "$mem" in
	let wtyp = tr_regtype (mem_regtype e) in 
	Load(Var mem, tr_exp g (mem_addr e), little_endian, wtyp)
    | TEMP ->
	let nm = temp_name e  in
	let Var.V(_,_,t) as var = gamma_lookup g nm in
	(*let t' = tr_regtype(temp_regtype e) in
	if t <> t'
	then failwith("Disassembly produced incorrect type "^type_to_string t'^" for"^var_to_string var)
	else*) Var var
    | CAST ->
        let sube = tr_exp g (cast_subexp e) in
        let newt = tr_regtype(cast_width e) in
	(match cast_casttype e with
	 | Libasmir.CAST_UNSIGNED -> Cast(CAST_UNSIGNED, newt, sube)
	 | Libasmir.CAST_SIGNED   -> Cast(CAST_SIGNED, newt, sube)
	 | Libasmir.CAST_HIGH	   -> Cast(CAST_HIGH, newt, sube)
	 | Libasmir.CAST_LOW	   -> Cast(CAST_LOW, newt, sube)
	 | Libasmir.CAST_FLOAT
	 | Libasmir.CAST_INTEGER
	 | Libasmir.CAST_RFLOAT
	 | Libasmir.CAST_RINTEGER ->
	     (pwarn "Warning: Ignoring deprecated cast type\n"; sube)
	)
    | NAME ->
	(match tr_label (name_string e) with
	 | Name n -> Lab n
	 | Addr i -> Int(big_int_of_int64 i, reg_64)
	)
    | UNKNOWN ->
        Unknown(unknown_str e, tr_regtype(unknown_regtype e))
    | LET ->
	failwith "Let expressions from C++ no longer supported"
    | EXTENSION ->
	failwith "Extension stmt types are unsupported."
    | _ ->
	failwith "Unexpected stmt type"


(** Translate a binop *)
and tr_binop g b lhs rhs =
  let (lhs,rhs) = (tr_exp g lhs, tr_exp g rhs) in
  match b with
    | Libasmir.PLUS     -> BinOp(PLUS    , lhs, rhs)
    | Libasmir.MINUS	-> BinOp(MINUS   , lhs, rhs)
    | Libasmir.TIMES	-> BinOp(TIMES   , lhs, rhs)
    | Libasmir.DIVIDE	-> BinOp(DIVIDE  , lhs, rhs)
    | Libasmir.SDIVIDE	-> BinOp(SDIVIDE  , lhs, rhs)
    | Libasmir.MOD	-> BinOp(MOD     , lhs, rhs)
    | Libasmir.SMOD	-> BinOp(SMOD     , lhs, rhs)
    | Libasmir.LSHIFT	-> BinOp(LSHIFT  , lhs, rhs)
    | Libasmir.RSHIFT	-> BinOp(RSHIFT  , lhs, rhs)
    | Libasmir.ARSHIFT	-> BinOp(ARSHIFT , lhs, rhs)
    | Libasmir.LROTATE
    | Libasmir.RROTATE	-> failwith "rotate is deprecated"
    | Libasmir.LOGICAND -> BinOp(AND  , lhs, rhs) (* operands should be bool *)
    | Libasmir.LOGICOR	-> BinOp(OR   , lhs, rhs) (* operands should be bool *)
    | Libasmir.BITAND	-> BinOp(AND  , lhs, rhs)
    | Libasmir.BITOR	-> BinOp(OR   , lhs, rhs)
    | Libasmir.XOR	-> BinOp(XOR     , lhs, rhs)
    | Libasmir.EQ	-> BinOp(EQ      , lhs, rhs)
    | Libasmir.NEQ	-> BinOp(NEQ     , lhs, rhs)
        (* FIXME: Assuming all comparisons are unsigned.
           This should be valid for IR generated via VEX. 
        *)
    | Libasmir.LT	-> BinOp(LT, lhs, rhs)
    | Libasmir.LE       -> BinOp(LE, lhs, rhs)
        (* We don't have GT or GTE, so implement using LT and LTE *)
    | Libasmir.GT	-> BinOp(LE, rhs, lhs) (* (x > y) <-> (y <= x) *)
    | Libasmir.GE	-> BinOp(LT, rhs, lhs) (* (x >= y) <-> (y < x) *)


(** Translate a vardecl, and adds the variable to the context
    
    @return vardecl and a function to restore the context
*)
let tr_vardecl (g:varctx) s =
  assert(Libasmir.stmt_type s = VARDECL);
  let nm = Libasmir.vardecl_name s in 
  let var = Var.newvar nm (tr_regtype(Libasmir.vardecl_type s)) in
  gamma_extend g nm var;
  (var, fun () -> gamma_unextend g nm)
    
(** Translate a list of vardecls, adding them to the context.
    @return vardecls and a function to restore the context *)
let tr_vardecls g ss =
  let decls,unextends = List.split(List.map (tr_vardecl g) ss) in
  (decls, fun x -> List.iter (fun f -> f x) unextends)

let cval_type_to_typ = function
 | NONE -> 
   prerr_endline "concrete expression with no type in lifted trace" ;
   reg_32        
 | BOOL -> reg_1
 | CHR -> reg_8
 | INT_16 -> reg_16
 | INT_32 -> reg_32
 | INT_64 -> reg_64
 | INT_128 -> Reg 128

let get_cval_usage = function
  | 0x00 | 0x01 -> RD
  | 0x10 -> WR
  | 0x11 -> RW
  | _ -> 
      prerr_endline "expression with no usage info" ;
      RD
      

(* TODO: needs to be refined for bytes *)
let int_to_taint n = Taint n

let big_int_of_big_val v t =
  let big_int_of_big_val_help v =
    let n = (cval_value_size v) - 1 in
    Util.foldn
      (fun acc index ->
         (* On x86, the data will be stored in litle endian form, so the
	    last index has the most significant data.  We want to access
	    this first, since we shift left as we go. *)
         let revindex = n - index in
         (* We need to convert the int64 we need to two's complement form *)
         let tempv = Arithmetic.to_big_int (big_int_of_int64 (cval_value_part v revindex), reg_64) in
         let shiftacc = shift_left_big_int acc 64 (* sizeof(int64) *) in
         (* dprintf "Hmmm.... %s %s" (string_of_big_int tempv) (string_of_big_int shiftacc); *)
         add_big_int tempv shiftacc)
      zero_big_int
      n
  in
  (* Cast off useless bits *)
  Arithmetic.to_big_int (big_int_of_big_val_help v, t)


let tr_context_tup cval =
  let t = cval_type_to_typ (Libasmir.cval_type cval) in
  Context {name=Libasmir.cval_name cval;
           mem=Libasmir.cval_mem cval;
           t=t;
           index=Libasmir.cval_ind cval;
           value=big_int_of_big_val (Libasmir.cval_value cval) t;
	   usage=get_cval_usage(Libasmir.cval_usage cval);
           taint=int_to_taint (Libasmir.cval_taint cval)}

(* deprecated *)
let tr_attributes s =
  let attr_vec = Libasmir.stmt_attributes s in
  let size = Libasmir.conc_map_size attr_vec in
  let cvals =
    if size = 0 then [] 
    else
      foldn 
	(fun i n -> 
	   (tr_context_tup (Libasmir.get_cval attr_vec n))::i
	) [] (size-1) in
  let tid = Libasmir.trace_tid attr_vec in
  let tidattr = ThreadId tid in
  if tid = -1 then cvals else
  tidattr :: cvals

(** Given a trace frame, return the list of attrs. *)
let tr_frame_attrs f =
  let attr_vec = Libasmir.asmir_frame_get_operands f in
  let size = Libasmir.asmir_frame_operands_length attr_vec in
  (* Add concrete operands *)
  let attrs =
    if size = 0 || size = -1 then [] 
    else
      let cattrs = foldn 
	(fun i n -> 
	   (tr_context_tup (Libasmir.asmir_frame_get_operand attr_vec n))::i
	) [] (size-1) 
      in
      let () = Libasmir.asmir_frame_destroy_operands attr_vec 
      in
      cattrs
  in
  (* Add ThreadId *)
  let attrs = match Libasmir.asmir_frame_tid f with
    | -1 -> attrs
    | n -> (Type.ThreadId n)::attrs
  in
  attrs

(** Translate a statement *)
let rec tr_stmt g s =
  match Libasmir.stmt_type s with
      JMP ->
	Jmp(tr_exp g (Libasmir.jmp_target s), [])
    | CJMP ->
	CJmp(tr_exp g (Libasmir.cjmp_cond s),
	    tr_exp g (Libasmir.cjmp_ttarget s),
	    tr_exp g (Libasmir.cjmp_ftarget s),
	    [] )
    | SPECIAL ->
	Special(Libasmir.special_string s, [])
    | MOVE ->
	let e = tr_exp g (move_rhs s) in
	(match tr_exp g (move_lhs s) with
	 | Var v ->
	     Move(v, e, [])
	 | Load(Var var as v, idx, endi, w) ->
	     Move(var, Store(v, idx, e, endi, w), [])
	 | _ -> 
	     failwith "Inproper lvalue in move"
	)
    | COMMENT ->
	Comment(Libasmir.comment_string s, [])
    | LABEL ->
	Label(tr_label (Libasmir.label_string s),
          tr_attributes s)
    | ASSERT ->
	Assert(tr_exp g (Libasmir.assert_cond s), [])
    | VARDECL
    | EXPSTMT
    | CALL
    | RETURN
    | FUNCTION ->
	failwith "Unsupported statement type"


(* convert certain specials into attributes *)
let rec handle_specials = 
  let reta = StrAttr "ret"
  and calla = StrAttr "call" in
  function
    | Jmp(t,a) :: Special("ret", _) :: stmts ->
      Jmp(t, reta::a) :: stmts
    | Jmp(t,a) :: Special("call", _) :: stmts ->
      Jmp(t, calla::a) :: stmts
    | x::xs -> x :: handle_specials xs
    | [] -> []
      

(** Translate a whole bap_block_t (as returned by
    Libasmir.asmir_bap_blocks_get) into a list of statements *)
let tr_bap_block_aux g b =
  let size = Libasmir.asmir_bap_block_size b - 1 in
  if Libasmir.asmir_bap_block_error b then
    failwith "Block was not lifted correctly by vine";
  assert (size+1 > 0);
  let addr = Libasmir.asmir_bap_block_address b in
  let (decs,stmts) =
    foldn (fun (ds,ss) n -> let s = asmir_bap_block_get b n in
	     match Libasmir.stmt_type s with
		 VARDECL -> (s::ds,ss)
	       | _ -> (ds,s::ss) )
      ([],[]) size
  in
  let decls, unextend = tr_vardecls g decs in
  let stmts = List.map (tr_stmt g) stmts in
  let stmts = handle_specials stmts in
  (stmts, addr, unextend)

let tr_bap_block_t g asmp b = 
  let stmts, addr, unextend = tr_bap_block_aux g b in
  let asm = Libasmir.asmir_string_of_insn asmp addr in
  let stmts = Label(Addr addr, [Asm asm])::stmts in 
  unextend();
  stmts

let tr_bap_block_t_trace_asm g b =
  let stmts, addr, unextend = tr_bap_block_aux g b in
  let asm = Libasmir.asm_string_from_block b in
  let stmts = Label(Addr addr, [Asm asm])::stmts in
  unextend();
  stmts

let tr_bap_block_t_no_asm g b =
  let stmts, addr, unextend = tr_bap_block_aux g b in
  let stmts = Label(Addr addr, [])::stmts in
  unextend();
  stmts

(** Translate a bap_blocks_t (as returned by
    Libasmir.asmir_asmprogram_to_bap) into a list of statements *)
let tr_bap_blocks_t g asmp bs = 
  let size = Libasmir.asmir_bap_blocks_size bs -1 in
    foldn (fun i n -> tr_bap_block_t g asmp (asmir_bap_blocks_get bs n)@i) [] size

let tr_bap_blocks_t_trace_asm g bs = 
  let size = Libasmir.asmir_bap_blocks_size bs -1 in
    foldn (fun i n -> tr_bap_block_t_trace_asm g (asmir_bap_blocks_get bs n)@i) [] size

let tr_bap_blocks_t_no_asm g bs = 
  let size = Libasmir.asmir_bap_blocks_size bs -1 in
    foldn (fun i n -> tr_bap_block_t_no_asm g (asmir_bap_blocks_get bs n)@i) [] size

let x86_regs = Asmir_vars.x86_regs
let x86_mem = Asmir_vars.x86_mem
let arm_regs = Asmir_vars.arm_regs
let all_regs = Asmir_vars.all_regs

let decls_for_arch = function
  | Bfd_arch_i386 -> x86_mem::x86_regs
  | Bfd_arch_arm  -> x86_mem::arm_regs
  | _ -> failwith "decls_for_arch: unsupported arch"

let gamma_for_arch = function
  | Bfd_arch_i386 -> gamma_create x86_mem x86_regs
  | Bfd_arch_arm  -> gamma_create x86_mem arm_regs
  | _ -> failwith "gamma_for_arch: unsupported arch"


let get_asmprogram_arch {arch=arch}= arch

let fold_memory_data f md acc =
  let size = Libasmir.memory_data_size md - 1 in
    foldn (fun a n ->
            let mcd = Libasmir.memory_data_get md n in
              f 
              (Libasmir.memory_cell_data_address mcd)
              (Libasmir.memory_cell_data_value mcd) 
              a)
      acc size

(* FIXME: use bfd_get_section_contents instead of this crazy memory_data thing *)
let get_rodata_assignments ?(prepend_to=[]) mem {asmp=prog} =
  let rodata = Libasmir.get_rodata prog in
  fold_memory_data
    (fun a v acc -> 
        let m_addr = Int(big_int_of_int64 a, Reg 32) in
        let m_val = Int(big_int_of_int v, Reg 8) in
        Move(mem, Store(Var mem, m_addr, m_val, little_endian, Reg 8), [InitRO]) :: acc)
    rodata prepend_to

let get_all_sections p =
  let arr,err = Libasmir.asmir_get_all_sections p in
  if err <= 0 then failwith "get_all_sections";
  arr

let get_all_asections p =
  get_all_sections p.asmp

let bfd_section_size = Libbfd.bfd_section_get_size
let bfd_section_vma = Libbfd.bfd_section_get_vma
let bfd_section_name = Libbfd.bfd_section_get_name

(** Is section s loaded? *)
let is_load s =
  let flags = bfd_section_get_flags s in
  Int64.logand Libbfd.sEC_LOAD flags <> 0L

(** Is section s code? *)
let is_code s =
  let flags = bfd_section_get_flags s in
  Int64.logand flags Libbfd.sEC_CODE <> 0L

let codeonly s = is_load s && is_code s
let loaded s = is_load s

(** Returns a list of [(addr,array)] tuples where [addr] is the
    starting address of a memory segment, and [array] is an array
    representing the memory starting at address [addr]. *)
let section_memory_helper ?(which=codeonly) prog secs =
  let bfd = Libasmir.asmir_get_bfd prog in
  let sc l s =
    let size = bfd_section_size s and vma = bfd_section_vma s
    and flags = bfd_section_get_flags s
    and name = bfd_section_name s in
    dprintf "Found section %s at %Lx with size %Ld. flags=%Lx" name vma size flags;
    if which s then
      (* if Int64.logand Libbfd.sEC_LOAD flags <> 0L then *)
      let (ok, a) = Libbfd.bfd_get_section_contents bfd s 0L size in
      if ok <> 0 then (vma, a)::l else (dprintf "failed."; l)
    else l
  in
  let bits = List.fold_left sc [] secs in
  bits

let section_contents ?(which=codeonly) prog secs =
  let bits = section_memory_helper ~which prog secs in
  let get a =
    (* let open Int64 in *)
    let (-) = Int64.sub in
    let rec f a = function [] -> raise Memory_error
      | (s,arr)::_ when a - s >= 0L && a - s < Int64.of_int(BArray.dim arr)  ->
	  arr.{Int64.to_int(a-s)}
      | _::b -> f a b
    in
    f a bits
  in
  get

let section_contents_list ?(which=codeonly) prog secs =
  let bits = section_memory_helper ~which prog secs in
  let (+) = Int64.add in
  let al l (base,arr) =
    (* [base, ..., base + len(arr)) *)
    foldn (fun l n -> (base + (Int64.of_int n), arr.{n})::l) l ((BArray.dim arr) - 1)
  in
  List.fold_left al [] bits

(** Open a binary file for translation *)
let open_program ?base filename =
  let base = match base with
    | None -> -1L
    | Some(x) -> x
  in
  let prog = Libasmir.asmir_open_file filename base in
    (* tell the GC how to free resources associated with prog *)
  Gc.finalise Libasmir.asmir_close prog;
  let secs = Array.to_list (get_all_sections prog)  in
  let get_exec = section_contents prog secs in
  let get_readable = section_contents ~which:loaded prog secs in 
 {asmp=prog; arch=Libasmir.asmir_get_asmp_arch prog; secs=secs; get_exec=get_exec; get_readable=get_readable}


let get_asm = function
  | Label(_,[Asm s])::_ -> s
  | _ -> ""

(* let check_equivalence a (ir1, next1) (ir2, next2) = *)
(*   assert(next1 = next2); *)
(*   try *)
(*     let q = Var(Var.newvar "q" reg_1) in *)
(*     let to_wp p =  *)
(*       let p = Memory2array.coerce_prog p in *)
(*       Wp.wp (Gcl.of_ast p) q *)
(*     in *)
(*     let wp1 = to_wp ir1 *)
(*     and wp2 = to_wp ir2 in *)
(*     let e = BinOp(EQ, wp1, wp2) in *)
(*     match Smtexec.CVC3.check_exp_validity e with *)
(*     | Smtexec.Valid -> () *)
(*     | Smtexec.Invalid -> wprintf "formulas for %Lx (%s aka %s) not equivalent" a (get_asm ir1) (get_asm ir2) *)
(*     | Smtexec.SmtError -> failwith "SmtError" *)
(*     | Smtexec.Timeout -> failwith "Timeout" *)
(*   with Failure s *)
(*   | Invalid_argument s -> *)
(*     (match get_asm ir1 with (\* Don't warn for known instructions *\) *)
(*     | "ret" | "hlt"-> () *)
(*     | _ -> wprintf "Could not check equivalence for %Lx: %s" a s *)
(*     ) *)
(*   | Not_found -> *)
(*     wprintf "Could not check equivalence for %Lx: Not_found" a *)


(** Translate only one address of a  Libasmir.asm_program_t to BAP *)
let asm_addr_to_bap {asmp=prog; arch=arch; get_exec=get_exec} addr =
  let fallback() =
    let g = gamma_for_arch arch in
    let (block, next) = Libasmir.asmir_addr_to_bap prog addr in
    if Libasmir.asmir_bap_block_error block then
      (* We are unable to lift this address. Decode errors are
         converted to a Special("VEX Decode Error"), so this is a
         non-Decode error.

         Unfortunately, we get no idea of the instruction length when
         this happens, so we'll optimistically increase by one.  *)
      let asm = Libasmir.asmir_string_of_insn prog addr in
      (Disasm_i386.ToIR.add_labels ~asm addr (Special("VEX General Error", [])::[]), Int64.succ addr)
    else
      (* Success: vine gave us a block to translate *)
      let ir = tr_bap_block_t g prog block in
      destroy_bap_block block;
      (ir, next)
  in
  try 
    let (ir,na) as v = 
      (try (Disasm.disasm_instr arch get_exec addr)
       with Disasm_i386.Disasm_i386_exception s -> 
	 DTest.dprintf "BAP unknown disasm_instr %Lx: %s" addr s;
         DTest.dprintf "Faulting instruction: %s" (Libasmir.asmir_string_of_insn prog addr);
	 DV.dprintf "disasm_instr %Lx: %s" addr s; raise Disasm.Unimplemented
      )
    in
    DV.dprintf "Disassembled %Lx directly" addr;
      (* if DCheck.debug then check_equivalence addr v (fallback()); *)
      (* If we don't have a string disassembly, use binutils disassembler *)
    (match ir with
    | Label(l, [])::rest ->
      (Label(l, [Asm(Libasmir.asmir_string_of_insn prog addr)])::rest,
       na)
    | _ -> v)
  with Disasm.Unimplemented ->
    DV.dprintf "Disassembling %Lx through VEX" addr;
    fallback()

let flatten ll =
	List.rev (List.fold_left (fun accu l -> List.rev_append l accu) [] ll)

(* asmprogram_to_bap_range p st en will read bytes at [st,en) from p and translate them to bap *)
let asmprogram_to_bap_range ?(init_ro = false) p st en =
  let rec f l s =
    (* This odd structure is to ensure tail-recursion *)
    let t = 
      try Some(asm_addr_to_bap p s)
      with Memory_error -> None in
    match t with
    | Some(ir, n) ->
      if n >= en then flatten (List.rev (ir::l))
      else
	f (ir::l) n
    | None ->
      (* If we fail, hopefully it is because there were some random
    	 bytes at the end of the section that we tried to
    	 disassemble *)
      wprintf "Failed to read instruction byte while disassembling at address %#Lx; end of section at %#Lx" s en;
      flatten (List.rev l)
  in
  f [] st

let asmprogram_section_to_bap p s =
  let size = bfd_section_size s and vma = bfd_section_vma s in
  asmprogram_to_bap_range p vma (Int64.add vma size)

(** Translate an entire Libasmir.asm_program_t into a BAP program *)
let asmprogram_to_bap ?(init_ro=false) p =
  let irs = List.map 
	(fun s -> 
	  if is_code s then asmprogram_section_to_bap p s else []) p.secs in
  let ir = flatten irs in
  if init_ro then
  let g = gamma_for_arch p.arch in
    let m = gamma_lookup g "$mem" in
    get_rodata_assignments ~prepend_to:ir m p
  else ir

(* Returns a single ASM instruction (as a list IL statements) from a
   sequence of bytes. *)
let byte_insn_to_bap arch addr bytes =
  let prog = Libasmir.byte_insn_to_asmp arch addr bytes in
  let get_exec a = bytes.(Int64.to_int (Int64.sub a addr)) in
  let (pr, n) = asm_addr_to_bap {asmp=prog; arch=arch; secs=[]; get_exec=get_exec; get_readable=get_exec} addr in
  Libasmir.asmir_close prog;
  pr, Int64.sub n addr

(* Transforms a byte sequence (byte array), to a list of lists of IL
   statements *)
let byte_sequence_to_bap bytes arch addr =
  let prog = Libasmir.byte_insn_to_asmp arch addr bytes in
  let len = Array.length bytes in
  let end_addr = Int64.add addr (Int64.of_int len) in
  let get_exec a = bytes.(Int64.to_int (Int64.sub a addr)) in
  let rec read_all acc cur_addr =
    if cur_addr >= end_addr then List.rev acc
    else
      let prog, next = asm_addr_to_bap {asmp=prog; arch=arch; secs=[]; get_exec=get_exec; get_readable=get_exec} cur_addr in
      read_all (prog::acc) next
  in
  let il = read_all [] addr in
  Libasmir.asmir_close prog;
  il

(** Create a function suitable for [Stream.of_func] that gets one
    block at a time from getter function [f].

    The trace is over when [getf] returns [].
*)
let rec bap_get_block_from_f f =
(* SWXXX UGLY! copy and pasted from traces....better place to put/do this? *)
  let trace_to_blocks trace = 
    let endtrace = "This is the final trace block" in
    let is_seed_label = (=) "ReadSyscall" in
    let rec to_blocks blocks current = function
      | [] -> 
        List.rev ((List.rev current)::blocks)
      | (Ast.Label (Addr _, _) as l)::rest ->
        let block = List.rev current in
        to_blocks (block::blocks) [l] rest
      | (Ast.Comment (c, _) as s)::rest when c = endtrace || (is_seed_label c) ->
        let block = List.rev current in
        to_blocks (block::blocks) [s] rest
      | x::rest ->
        to_blocks blocks (x::current) rest
    in
    let blocks = to_blocks [] [] trace in
    List.filter (fun b -> List.length b > 1) blocks
  in
  let block_q = Queue.create () in
  (fun off ->
    let refill () =
      match f () with
      | [] -> false
      | trace ->
        let blocks = trace_to_blocks trace in
        List.iter (fun x -> Queue.push x block_q) blocks;
        true
    in
    try
      Some(Queue.take block_q)
    with Queue.Empty ->
      (match refill() with
      | true -> Some(Queue.take block_q)
      | false -> None))

  let add_operands stmts ops =
    match stmts with
    | Label (l,a)::others ->
      Label (l,a@ops)::others
    | Comment (s,a)::others ->
      Comment (s,a@ops)::others
    | others when ops <> [] -> Comment("Attrs without label.", ops)::others
    | others -> others

(** The original PIN trace format (pin_trace.cpp). *)
module PinTrace = struct

(* Get stmts for a frame *)
  let trans_frame f =
    let arch = Libbfd.Bfd_arch_i386 in
    let t = Libasmir.asmir_frame_type f in
    match t with
    | Libasmir.FRM_STD2 -> 
      let bytes, addr, _ = Libasmir.asmir_frame_get_insn_bytes f in
    (* Array.iter (fun x -> dprintf "Byte: %x" (int_of_char x)) bytes; *)
      let stmts, _ = byte_insn_to_bap arch addr bytes in
      stmts
    | Libasmir.FRM_TAINT -> 
      [Comment("ReadSyscall", []); Comment("All blocks must have two statements", [])]
    | Libasmir.FRM_LOADMOD ->
      let name, lowaddr, highaddr = Libasmir.asmir_frame_get_loadmod_info f in
    (* The traceremove attr means that our Traces implementation can safely ignore this Special. *)
      [Special(Printf.sprintf "Loaded module '%s' at %#Lx to %#Lx" name lowaddr highaddr, []); Comment("All blocks must have two statements", [])]
    | Libasmir.FRM_SYSCALL ->
      let callno, addr, tid = Libasmir.asmir_frame_get_syscall_info f in
      [Special(Printf.sprintf "Syscall number %d at %#Lx by thread %d" callno addr tid, [StrAttr "TraceKeep"]);
       Comment("All blocks must have two statements", [])]
    | Libasmir.FRM_EXCEPT ->
      let exceptno, tid, from_addr, to_addr =
        Libasmir.asmir_frame_get_except_info f in
      [Special(Printf.sprintf "Exception number %d by thread %d at %#Lx to %#Lx" exceptno tid from_addr to_addr, []);
       Comment("All blocks must have two statements", [])]
    | Libasmir.FRM_KEY_GENERAL ->
      [Comment("Key frame state", []);
       Comment("All blocks must have two statements", [])]
    | Libasmir.FRM_STD | Libasmir.FRM_KEY | Libasmir.FRM_NONE -> []

  let alt_bap_from_trace_file_range_rev filename off reqframes =
    let raise_frame f =
      let stmts = trans_frame f in
      add_operands stmts (tr_frame_attrs f)
    in
    let c = ref true in
    let revstmts = ref [] in
  (* flush VEX buffers *)
    let () = Libasmir.asmir_free_vex_buffers () in
    let trace_frames =
      Libasmir.asmir_frames_from_trace_file filename !off reqframes in
    let numframes = Libasmir.asmir_frames_length trace_frames in
  (*dprintf "Got %d frames" numframes;*)
    if numframes = 0 || numframes = -1 then (
      c := false
    ) else (
      if ((Int64.of_int numframes) <> reqframes) then
        dprintf "Got %d frames which <> requested frames %s"
	  numframes (Int64.to_string reqframes);
      revstmts :=
        Util.foldn
        (fun acc n ->
	  let frameno = numframes-1-n in
	(* dprintf "frame %d" frameno; *)
	  let stmts =
	    raise_frame (Libasmir.asmir_frames_get trace_frames frameno)
	  in
	  List.rev_append stmts acc)
        [] (numframes-1);
      off := Int64.add !off (Int64.of_int numframes);
  (* let moreir = tr_bap_blocks_t_no_asm g bap_blocks in *)
  (* Build ir backwards *)
  (* ir := List.rev_append moreir !ir; *)
    );
    asmir_frames_destroy trace_frames;
    (!c, !revstmts)

  let alt_bap_from_trace_file_range filename off reqframes =
    let (c, revstmts) = alt_bap_from_trace_file_range_rev filename off reqframes in
    (c, List.rev revstmts)

  let alt_bap_from_trace_file filename =
    let off = ref 0L in
    let ir = ref [] in
    let c = ref true in
    Status.init "Lifting trace" 0 ;
    while !c do
      let (tmp_c,revstmts) =
        alt_bap_from_trace_file_range_rev filename off !trace_blocksize in
      ir := BatList.append revstmts !ir;
      c := tmp_c;
    done;
    let r = List.rev !ir in
    Status.stop () ;
    r

let alt_bap_get_block_from_trace_file ?(atts = true) ?(rate=1L) ?(pin=false) filename =
  let offset = ref 0L in
  let f () = snd (alt_bap_from_trace_file_range filename offset rate) in
  bap_get_block_from_f f

(** Return stream of trace instructions raised to the IL *)
let alt_bap_stream_from_trace_file ?(atts = true) ?(rate=1L) ?(pin = false) filename =
  Stream.from (
   alt_bap_get_block_from_trace_file ~atts ~rate ~pin filename)

end

(** The new protobuffers/piqi serialized trace format. *)
module SerializedTrace = struct

  let new_bap_from_trace_frames ?n r =
    print_mem_usage();
    let get_attrs =
      let convert_taint = function
        | `no_taint -> Taint 0
        | `taint_id(id) -> Taint (Int64.to_int id)
        | `taint_multiple -> Taint (-1)
      in
      let convert_usage = function
        | {Operand_usage.read=true; Operand_usage.written=true} -> Type.RW
        | {Operand_usage.read=true} -> Type.RD
        | {Operand_usage.written=true} -> Type.WR
        | _ -> (* Trace usage undefined; assuming read *) Type.RD
      in
      let convert_operand_info = function
        | {Operand_info.operand_info_specific=`mem_operand({Mem_operand.address=a});
           Operand_info.bit_length=b;
           Operand_info.operand_usage=use;
           Operand_info.taint_info=t;
           Operand_info.value=v} ->
          Context({name="mem";
                   mem=true;
                   t=Reg b;
                   index=a;
                   value=Util.big_int_of_binstring ~e:`Little v;
                   usage=convert_usage use;
                   taint=convert_taint t})
        | {Operand_info.operand_info_specific=`reg_operand({Reg_operand.name=n});
           Operand_info.bit_length=b;
           Operand_info.operand_usage=use;
           Operand_info.taint_info=t;
           Operand_info.value=v} ->
          Context({name=n;
                   mem=false;
                   t=Reg b;
                   index=0L;
                   value=Util.big_int_of_binstring ~e:`Little v;
                   usage=convert_usage use;
                   taint=convert_taint t})
      in
      let convert_taint_info = function
        | {Taint_intro.addr=a;
           Taint_intro.taint_id=tid;
           Taint_intro.value=value;
           Taint_intro.source_name=src_name;
           Taint_intro.offset=off} ->
          let v = match value with
            | Some x -> Util.big_int_of_binstring ~e:`Little x
            | None -> Big_int_convenience.bi0
          in
          let tid = Int64.to_int tid in
          let off = Int64.to_int off in
          let ctx = Context({name="mem"; mem=true; t=Reg 8; index=a; value=v;
                   usage=WR;
                   taint=Taint tid}) 
          in
          let intro = TaintIntro(tid, src_name, off) in
          [intro; ctx]
      in
      let convert_thread_id x = Type.ThreadId (Int64.to_int x)
      in
      function
        | `std_frame({Std_frame.operand_list=ol; Std_frame.thread_id=tid}) -> (convert_thread_id tid) :: List.map convert_operand_info ol
        | `syscall_frame _ -> []
        | `exception_frame _ -> []
        | `taint_intro_frame({Taint_intro_frame.taint_intro_list=til}) -> 
            let l = List.map convert_taint_info til in
            List.flatten l
        | `modload_frame _ -> []
        | `key_frame _ -> []
        | `metadata_frame _ -> []
    in
    let raise_frame arch f =
      let get_stmts =
        function
          | `std_frame(f) ->
      (* Convert string to byte array *)
            let a = Array.of_list (BatString.to_list f.Std_frame.rawbytes) in
            let stmts, _ = byte_insn_to_bap arch f.Std_frame.address a in
            stmts
          | `syscall_frame({Syscall_frame.number=callno;
                            Syscall_frame.address=addr;
                            Syscall_frame.thread_id=tid}) ->
            [Special(Printf.sprintf "Syscall number %Ld at %#Lx by thread %Ld" callno addr tid, [StrAttr "TraceKeep"]); Comment("All blocks must have two statements", [])]
          | `exception_frame({Exception_frame.exception_number=exceptno;
                              Exception_frame.thread_id=Some tid;
                              Exception_frame.from_addr=Some from_addr;
                              Exception_frame.to_addr=Some to_addr}) ->
            [Special(Printf.sprintf "Exception number %Ld by thread %Ld at %#Lx to %#Lx" exceptno tid from_addr to_addr, []);
             Comment("All blocks must have two statements", [])]
          | `exception_frame({Exception_frame.exception_number=exceptno}) ->
            [Special(Printf.sprintf "Exception number %Ld" exceptno, []);
             Comment("All blocks must have two statements", [])]
          | `taint_intro_frame(f) ->
            [Comment("ReadSyscall", []); Comment("All blocks must have two statements", [])]
          | `modload_frame({Modload_frame.module_name=name;
                            Modload_frame.low_address=lowaddr;
                            Modload_frame.high_address=highaddr}) ->
            [Special(Printf.sprintf "Loaded module '%s' at %#Lx to %#Lx" name lowaddr highaddr, []); Comment("All blocks must have two statements", [])]
          | `key_frame _ ->
      (* Implement key frame later *)
            []
          | `metadata_frame _ -> []
      in
      add_operands (get_stmts f) (get_attrs f)
    in
    let out = ref [] in
    let counter = ref 0L in
    let checkctr () =
      match n with
      | Some(n) -> !counter < n
      | None -> true
    in
    let blocksize = match n with | Some x -> x | None -> !trace_blocksize in
    while not r#end_of_trace && checkctr () do
      let frames = r#get_frames blocksize in
    (* XXX: Remove use of Obj.magic in an elegant way... *)
      out := List.rev_append (List.flatten (List.map (raise_frame (Obj.magic r#get_arch)) frames)) !out;
      counter := Int64.add !counter (Int64.of_int (List.length frames));
    done;

    List.rev !out

(** New trace file format: Read entire trace at once *)
  let new_bap_from_trace_file filename =
    let r = new Trace_container.reader filename in
    new_bap_from_trace_frames r

(** New trace format: Create a streamer *)
  let new_bap_stream_from_trace_file rate filename =
    let r = new Trace_container.reader filename in
    let f () = new_bap_from_trace_frames ~n:rate r in
    Stream.from (bap_get_block_from_f f)

end

(** Old pre-PIN trace format (trace_*.cpp) *)
module OldTrace = struct

(* deprecated *)  
  let old_bap_from_trace_file ?(atts = true) ?(pin = false) filename =
    let g = gamma_create x86_mem x86_regs in
    let ir = ref [] in
    let off = ref 0L in
    let c = ref true in
  (* might be better to just grab the length of the trace in advance :*( *)
    Status.init "Lifting trace" 0 ;
    while !c do
    (*dprintf "Calling the trace again.... ";*)
      let bap_blocks = 
        Libasmir.asmir_bap_from_trace_file filename !off !trace_blocksize atts pin
      in
      let numblocks = Libasmir.asmir_bap_blocks_size bap_blocks in
      if numblocks = -1 then (
        c := false
      ) else (
        let moreir = tr_bap_blocks_t_trace_asm g bap_blocks in
        let () = destroy_bap_blocks bap_blocks in
	(* Build ir backwards *)
	ir := List.rev_append moreir !ir;
	off := Int64.add !off !trace_blocksize
      )
    done;
    let r = List.rev !ir in
    Status.stop () ;
    r
end

(** Read entire trace at once.  If [pin] is true, uses PinTrace
    code.  If [pin] is false, uses old lifting code. *)
let bap_from_trace_file ?(atts = true) ?(pin = false) filename =
  if pin then
    PinTrace.alt_bap_from_trace_file filename
  else
    OldTrace.old_bap_from_trace_file ~atts ~pin filename

let serialized_bap_stream_from_trace_file = SerializedTrace.new_bap_stream_from_trace_file
let bap_stream_from_trace_file = PinTrace.alt_bap_stream_from_trace_file

let serialized_bap_from_trace_file = SerializedTrace.new_bap_from_trace_file

(* End traces functions *)

let get_symbols ?(all=false) {asmp=p} =
  let f = if all then asmir_get_all_symbols else asmir_get_symbols in
  let (arr,err) = f p in
  if err <= 0 then failwith "get_symbols";
  arr

(* XXX: Very inefficient *)
let find_symbol {asmp=p} name =
  let (arr,err) = asmir_get_all_symbols p in
  if err <= 0 then failwith "find_symbol";
  BatArray.find (fun sym -> if sym.bfd_symbol_name = name then true else false) arr

let get_flavour p = bfd_flavour (Libasmir.asmir_get_bfd p.asmp)

let get_section_startaddr p sectionname =
  Libasmir.asmir_get_sec_startaddr p.asmp sectionname

let get_section_endaddr p sectionname =
  Libasmir.asmir_get_sec_endaddr p.asmp sectionname

let get_base_address p =
  Libasmir.asmir_get_base_address p.asmp

let get_start_addr p =
  Libasmir.asmir_get_start_addr p.asmp

let get_asm_instr_string p s =
  Libasmir.asmir_string_of_insn p.asmp s

let get_asm_instr_string_range p s e =
  let s = ref s in
  let str = ref "" in
  (try
    while !s < e do

      str := !str ^ "; " ^ (get_asm_instr_string p !s);

      let len = Int64.of_int (Libasmir.asmir_get_instr_length p.asmp !s) in
      if len = -1L then raise Exit;
      s := Int64.add !s len
    done;
  with Exit -> ());
  !str

let set_print_warning = Libasmir.asmir_set_print_warning

let get_print_warning = Libasmir.asmir_get_print_warning

let set_use_simple_segments = Libasmir.asmir_set_use_simple_segments

let get_exec_mem_contents {get_exec=get_exec} =
  get_exec

let get_exec_mem_contents_list {asmp=asmp; secs=secs} = section_contents_list ~which:is_code asmp secs

let get_readable_mem_contents {get_readable=get_readable} = get_readable

let get_readable_mem_contents_list {asmp=asmp; secs=secs} = section_contents_list ~which:loaded asmp secs
