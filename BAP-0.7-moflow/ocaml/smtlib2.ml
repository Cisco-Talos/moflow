(** Output to SMTLIB2 format 

    @author ejs
*)
open Type
open Big_int_Z
open Big_int_convenience
open Ast
open Ast_convenience
open Typecheck

module D = Debug.Make(struct let name = "Smtlib2" and default=`NoDebug end)
open D

exception No_rule

module VH = Var.VarHash

type sort = BitVec | Bool

type option =
  | SetOptionProduceAssignments (** Z3 only *)

let use_booleans = ref true ;;

(** This printer has to deal with a number of differences between the
    BAP IL and SMTLIB.  One of the most striking is that SMTLIB has
    separate types for booleans and bitvectors of one bit; BAP only has
    bitvectors of one bit.  There is no one size fits all solution.
    Booleans have many nice features, such as n-ary functions.  Some
    functions only work on bitvectors (like casts).

    The primary printing functions are ast_exp_base and
    ast_exp_bool_base.  ast_exp_base prints a BAP function e, and if e
    is a 1-bit bap bitvector, it treats it as a bitvector in SMTLIB.
    ast_exp_bool_base only works on 1-bit bap bitvectors, and treats
    them as a boolean in SMTLIB.  Either function can raise the
    No_rule exception.  When the check argument is set, the function
    should raise No_rule before printing anything.

    ast_exp, ast_exp_bool, and ast_exp_bv are wrappers for the *_base
    functions.  They catch the No_rule exceptions, and send the request
    to the other base function if necessary.  They print, rather than
    returning lazy evaluations, since they can never return No_rule.
*)

class pp ?(opts=[]) ft =
  let pp = Format.pp_print_string ft
  and pc = Format.pp_print_char ft
  and pi = Format.pp_print_int ft
  and space = Format.pp_print_space ft
  and cut = Format.pp_print_cut ft
  and force_newline = Format.pp_force_newline ft
  and printf f = Format.fprintf ft f
  and opn  = Format.pp_open_box ft
  and flush = Format.pp_print_flush ft
  and cls = Format.pp_close_box ft in
  let var2s (Var.V(num,name,_)) =
    name^"_"^(string_of_int num)
  in
  let setoption = List.mem SetOptionProduceAssignments opts in

  let opflatten e =
    let rec oh bop e acc =
      match e with
      | BinOp(bop', e'1, e'2) when bop' = bop ->
	oh bop e'1 (oh bop e'2 acc)
      | _ -> e::acc
    in
    match e with
    | BinOp(bop, e1, e2) ->
	oh bop e []
    | _ -> failwith "opflatten expects a binop"
  in

object (self)
  inherit Formulap.fpp
  val used_vars : (string,Var.t) Hashtbl.t = Hashtbl.create 57
  val ctx : (string*sort) VH.t = VH.create 57

  val mutable unknown_counter = 0;

  val mutable let_counter = 0;

  method bool_to_bv ~check e =
    if check then (
      self#ast_exp_bool_base ~check e;
      self#ast_exp_base ~check exp_true;
      self#ast_exp_base ~check exp_false;
      ()
    ) else (
      pp "(ite";
      space ();
      self#ast_exp_bool_base ~check e;
      space ();
      self#ast_exp_base ~check exp_true;
      space ();
      self#ast_exp_base ~check exp_false;
      cut ();
      pc ')'
    )
  method bv_to_bool ~check e =
    if check then (
      self#ast_exp_base ~check e;
      self#ast_exp_base ~check exp_true;
      ()
    ) else (
      pp "(=";
      space ();
      self#ast_exp_base ~check e;
      space ();
      self#ast_exp_base ~check exp_true;
      cut ();
      pp ")"
    )

  method flush =
    flush();

  method extend v s st =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s v;
    VH.add ctx v (s,st)

  method unextend v =
    VH.remove ctx v

  method var v =
    match (VH.find ctx v) with
    | n,_ -> pp n

  method and_start = ()

  method and_constraint e =
    pp "(and ";
    space ();
    self#ast_exp_bool e;
    space ()

  method and_close_constraint =
    pc ')'

  method and_end = ()

  (** Seperate let_begin and let_end to allow for streaming generation of
      formulas in utils/streamtrans.ml *)
  method let_begin v e1 =
    let t1 = Typecheck.infer_ast e1 in
    let cmd,c,pf,vst =
      match t1,!use_booleans with
        | Reg 1,true -> "let","$",self#ast_exp_bool,Bool
        | _ -> "let","?",self#ast_exp,BitVec
    in
    (* The print functions called, ast_exp and ast_exp_bool never
       raise No_rule. So, we don't need to evaluate them before the lazy
       block. *)
      pp "("; pp cmd; pp " ((";
      (* v isn't allowed to shadow anything. also, smtlib requires it be
         prefixed with ? or $ *)
      let s = c ^ var2s v ^"_"^ string_of_int let_counter in
      let_counter <- succ let_counter;
      pp s;
      pc ' ';
      pf e1;
      pc ')';
      pc ')';
      space ();
      self#extend v s vst;

  method let_end v =
      self#unextend v;
      cut ();
      pc ')'

  method let_middle st =
      match st with
        | Bool -> self#ast_exp_bool
        | BitVec -> self#ast_exp

  (** Returns a lazy expression that prints let v = e1 in e2. Never raises 
      No_rule. *)
  method letme v e1 e2 st =
    lazy(
      self#let_begin v e1;
      self#let_middle st e2;
      self#let_end v 
    )

  method varname v =
    match VH.find ctx v with
    | n,_ -> n

  method varsort v =
    match VH.find ctx v with
    | _,st -> st

  method declare_given_freevars es =
    force_newline();
    pp "; free variables:"; force_newline();
    List.iter (fun v -> if not(VH.mem ctx v) then self#decl v) es;
    pp "; end free variables."; 
    force_newline()
 
  method declare_new_freevars e =
    let fvs = Formulap.freevars e in 
    self#declare_given_freevars fvs
       
  method typ = function
    | Reg n ->	printf "(_ BitVec %d)" n
    | Array((Reg idx as t1), (Reg elmt as t2)) ->
      pp "(Array";
      space ();
      self#typ t1;
      space ();
      self#typ t2;
      pc ')';
    | Array _ -> failwith "SMTLIB2 only supports Arrays with register indices and elements"
    | TMem _ ->	failwith "TMem unsupported by SMTLIB2"

 method decl_no_print (Var.V(_,_,t) as v) =
    let sort = match t with
      (* | Reg 1 -> Bool (\* Let's try making all 1-bit bvs bools for now *\) *)
      | _ -> BitVec
    in
    self#extend v (var2s v) sort

  method predeclare_free_var = self#decl_no_print

  method print_free_var (Var.V(_,_,t) as v) =
    pp "(declare-fun";
    space ();
    self#var v;
    space ();
    pp "()";
    space ();
    self#typ t;
    pc ')';
    force_newline()

  method decl v =
    self#decl_no_print v;
    self#print_free_var v

  (* Wrapper around a ~check functions to ensure the function does not
     raise No_rule before calling it. *)
  method tryit f e =
    f ~check:true e;

    (* If we get here, f did not raise No_rule.  So it had better not
       raise it if we turn off check. *)
    try
      f ~check:false e
    with No_rule -> failwith (Printf.sprintf "tryit: check is broken: %s" (Pp.ast_exp_to_string e))

  (** Prints the BAP expression e in SMTLIB format.  If e is a
      1-bit bitvector in BAP, then e is printed as a SMTLIB 1-bit
      bitvector.  Raises the No_rule exception if it is not possible
      to print e as a bitvector (e.g., it should be printed as a
      boolean). *)
  method ast_exp_base ~check e =
    let lazye = 
      (match e with
     | Int((i, Reg t) as p) ->
	 let maskedval = Arithmetic.to_big_int p in
	 lazy (
	   pp "(_ bv"; pp (string_of_big_int maskedval); space (); pi t; pc ')'
	 )
     | Int _ -> failwith "Ints may only have register types"
     | Var v ->
	 let name,st = VH.find ctx v in
	 (match st with 
	 | BitVec -> lazy (pp name)
	 | Bool -> raise No_rule)
     | Ite(cond, e1, e2) ->
	 let pe1 = lazy (self#ast_exp e1) in
	 let pe2 = lazy (self#ast_exp e2) in
	 lazy(
	   pp "(ite";
	   space ();
	   self#ast_exp_bool cond;
	   space ();
	   Lazy.force pe1;
	   space ();
	   Lazy.force pe2;
	   cut ();
	   pc ')'
	 )
     | UnOp(uop, e) ->
	 let pe = lazy (self#ast_exp_bv e) in
	 lazy(
	   (match uop with
	    | NEG -> pp "(bvneg"; space ();
	    | NOT -> pp "(bvnot"; space ();
	   );
	   Lazy.force pe;
	   pc ')'
	 )
     | BinOp(OR, _, _) when parse_concat e <> None ->
     	 let el, er = match parse_concat e with
     	   | Some(el, er) -> el, er
     	   | None -> assert false
     	 in
	 let pel,per = lazy (self#ast_exp_bv el), lazy (self#ast_exp_bv er) in
     	 lazy(
	   pp "(concat";
     	   space ();
	   Lazy.force pel;
     	   space ();
	   Lazy.force per;
     	   cut ();
     	   pc ')'
	 )
     | BinOp((AND|OR), _, _) when parse_ite e <> None ->
     	 let b, e1, e2 = match parse_ite e with
     	   | Some(b, e1, e2) -> b, e1, e2
     	   | None -> assert false
     	 in
	 let pb, pe1, pe2 = lazy (self#ast_exp_bool b), lazy (self#ast_exp e1), lazy (self#ast_exp e2) in
	 lazy(
     	   pp "(ite";
     	   space ();
	   Lazy.force pb;
     	   space ();
	   Lazy.force pe1;
     	   space ();
	   Lazy.force pe2;
     	   cut ();
     	   pc ')';
	 )
     | BinOp((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|MOD|SMOD|AND|OR|XOR|LSHIFT|RSHIFT|ARSHIFT) as bop, e1, e2) as e ->
	 let t = infer_ast e1 in
	 let t' = infer_ast e2 in
	 if t <> t' then
	   wprintf "Type mismatch: %s" (Pp.ast_exp_to_string e);
	 assert (t = t') ;
	 let fname = match bop with
	   (* | EQ -> "bvcomp" *) (* bvcomp doesn't work on memories! *)
	   | PLUS -> "bvadd"
	   | MINUS -> "bvsub"
	   | TIMES -> "bvmul"
	   | DIVIDE -> "bvudiv"
	   | SDIVIDE -> "bvsdiv"
	   | MOD -> "bvurem"
	   | SMOD -> "bvsrem"
	   | AND -> "bvand"
	   | OR -> "bvor"
	   | XOR -> "bvxor"
	   | NEQ|EQ|LE|LT|SLT|SLE -> assert false
	   | LSHIFT -> "bvshl"
	   | RSHIFT -> "bvlshr"
	   | ARSHIFT -> "bvashr"
	 in
	 let pe1, pe2 = lazy (self#ast_exp e1), lazy (self#ast_exp e2) in
	 lazy(
	   pc '('; pp fname; space (); Lazy.force pe1; space (); Lazy.force pe2; pc ')';
	 )
     | Cast(CAST_LOW, t, BinOp(RSHIFT, e', Int(i, t2))) when parse_extract e <> None ->
     	 let hbit, lbit = match parse_extract e with
     	   | Some(hbit, lbit) -> hbit, lbit
     	   | None -> assert false
     	 in
	 let pe' = lazy (self#ast_exp_bv e') in
	 lazy(
  	   pp ("(( _ extract "^string_of_big_int hbit^" "^string_of_big_int lbit^")");
     	   space ();
	   Lazy.force pe';
     	   cut ();
     	   pc ')';
	 )
     | Cast((CAST_UNSIGNED|CAST_SIGNED) as ct, t, e1) when (infer_ast e1) = Reg(1) ->
	 (* Optimization: 
	    CAST(UNSIGNED, Reg n, bool_e) =
	    ite bool_e 1[n] 0[n]
	    CAST(SIGNED, Reg n, bool_e) =
	    ite bool_e -1[n] 0[n]
	 *)
	 let t1 = infer_ast e1 in
	 let (bitsnew, bitsold) = (bits_of_width t, bits_of_width t1) in
	 let delta = bitsnew - bitsold in
	 let textend, fextend = match ct with
	   | CAST_UNSIGNED -> Int(bi1, t), Int(bi0, t)
	   | CAST_SIGNED -> Int(bim1, t), Int(bi0, t)
	   | _ -> assert false
	 in
	 assert (delta >= 0);
	 let pe1,ptext,pfext = lazy (self#ast_exp_bool e1), lazy (self#ast_exp textend), lazy (self#ast_exp fextend) in
	 (match delta with
	 | 0 -> lazy (Lazy.force pe1)
	 | _ -> 
	     lazy(
	       pp "(ite";
	       space ();
	       Lazy.force pe1;
	       space ();
	       Lazy.force ptext;
	       space ();
	       Lazy.force pfext;
	       cut ();
	       pc ')'
	     )
	 )	 	       
     | Cast((CAST_LOW|CAST_HIGH|CAST_UNSIGNED|CAST_SIGNED) as ct, t, e1) ->
	  let t1 = infer_ast e1 in
	  let (bitsnew, bitsold) = (bits_of_width t, bits_of_width t1) in
	  let delta = bitsnew - bitsold in
	  (match ct with
	    | CAST_LOW | CAST_HIGH -> assert (delta <= 0);
	    | CAST_UNSIGNED | CAST_SIGNED -> assert (delta >= 0));
	  let (pre,post) = match ct with
	    | _ when bitsnew = bitsold -> ("","")
	    | CAST_LOW      -> ("((_ extract "^string_of_int(bitsnew-1)^" 0)", ")")
	    | CAST_HIGH     -> ("((_ extract "^string_of_int(bitsold-1)^" "^string_of_int(bitsold-bitsnew)^")", ")")
	    | CAST_UNSIGNED -> ("((_ zero_extend "^string_of_int(delta)^")", ")")
	    (* | CAST_UNSIGNED -> ("(concat bv0["^string_of_int(delta)^"] ", ")") *)
	    | CAST_SIGNED -> ("((_ sign_extend "^string_of_int(delta)^")", ")")
	  in
	  let pe1 = lazy (self#ast_exp_bv e1) in
	  lazy(
	    pp pre;
	    space ();
	    Lazy.force pe1;
	    cut ();
	    pp post
	  )
     | Concat(le,re) ->
	 let pe1 = lazy (self#ast_exp le) in
	 let pe2 = lazy (self#ast_exp re) in
	 lazy (
	   pp "(concat ";
	   Lazy.force pe1;
	   space ();
	   Lazy.force pe2;
	   cut ();
	   pc ')'
	 )
     | Extract(h,l,e) ->
	 let pe = lazy (self#ast_exp e) in
	 lazy(
	   pp ("((_ extract "^string_of_big_int(h)^" "^string_of_big_int(l)^")");
	   space ();
	   Lazy.force pe;
	   cut ();
	   pc ')'
	 )
     | Unknown(s,t) ->
	  lazy (
	    pp "unknown_"; pi unknown_counter; pp" ;"; pp s; force_newline();
	    unknown_counter <- unknown_counter + 1;
	  )
      | Lab lab ->
	  failwith ("SMTLIB: don't know how to handle label names: "
		      ^ (Pp.ast_exp_to_string e))
      | Let(v, e1, e2) -> self#letme v e1 e2 BitVec
      | Load(arr,idx,endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  let parr, pidx = lazy (self#ast_exp arr), lazy (self#ast_exp idx) in
	  lazy(
	    pp "(select ";
	    Lazy.force parr;
	    space ();
	    Lazy.force pidx;
	    cut ();
	    pc ')'
	  )
      | Store(arr,idx,vl, endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  let parr, pidx, pvl = lazy (self#ast_exp arr), lazy (self#ast_exp idx), lazy (self#ast_exp vl) in
	  lazy(
	    pp "(store ";
	    Lazy.force parr;
	    space ();
	    Lazy.force pidx;
	    space ();
	    Lazy.force pvl;
	    cut ();
	    pc ')'
	  )
      | _ -> raise No_rule
      )
    in
    if not check then (
      opn 0; Lazy.force lazye; cut (); cls()
    )
  (** Evaluate an expression to a bitvector, preferring bools instead
      of 1-bit bvs. *)
  method ast_exp e =
    if not !use_booleans then self#ast_exp_bv e
    else (
      let t = Typecheck.infer_ast e in
      if t = Reg(1) then (
        try
          self#tryit self#bool_to_bv e
        with No_rule ->
          self#ast_exp_base ~check:false e
      ) else
	self#ast_exp_base ~check:false e)

  (** Evaluates an expression to a bitvector, preferring
      bitvectors over booleans. *)
  method ast_exp_bv e =
    try
      self#tryit self#ast_exp_base e
    with No_rule ->
      self#bool_to_bv ~check:false e

  (** Try to evaluate an expression to a boolean. If no good rule
      exists, then raises the No_rule exception. *)
  method ast_exp_bool_base ~check e =
    if debug () then (
      let t = Typecheck.infer_ast e in
      assert (t = Reg(1)));
    let lazye =
      (match e with
      | Int((i, Reg t) as p) when t = 1 ->
	let maskedval = Arithmetic.to_big_int p in
	(match maskedval with
	| bi when bi_is_zero bi -> lazy(pp "false")
	| bi when bi_is_one bi -> lazy(pp "true")
	| _ -> failwith "ast_exp_bool")
      | Int((i, Reg t)) -> failwith "ast_exp_bool only takes reg_1 expressions"
      | Int _ -> failwith "Ints may only have register types"
      | Ite(cond, e1, e2) ->
	lazy(
	  pp "(if_then_else";
	  space ();
	  self#ast_exp_bool cond;
	  space ();
	  self#ast_exp_bool e1;
	  space ();
	  self#ast_exp_bool e2;
	  cut ();
	  pc ')'
	)
      | UnOp((NEG|NOT), o) ->
	 (* neg and not are the same for one bit! *)
	lazy(
	  pp "(not";
	  space ();
	  self#ast_exp_bool o;
	  cut ();
	  pc ')'
	)
      | BinOp(NEQ, e1, e2) ->
       (* Rewrite NEQ in terms of EQ *)
        let newe = UnOp(NOT, BinOp(EQ, e1, e2)) in
        lazy(
	  self#ast_exp_bool newe
        )
      | BinOp((OR|AND), _, _) when parse_ite e <> None ->
     	let b, e1, e2 = match parse_ite e with
     	  | Some(b, e1, e2) -> b, e1, e2
     	  | None -> assert false
     	in
	lazy(
     	  pp "(if_then_else";
     	  space ();
     	  self#ast_exp_bool b;
     	  space ();
     	  self#ast_exp_bool e1;
     	  space ();
     	  self#ast_exp_bool e2;
     	  cut ();
     	  pc ')';
	)
     (* Short cuts for e = exp_true and e = exp_false *)
      | BinOp(EQ, e1, e2) when full_exp_eq e1 (Int(bi1, Reg(1))) ->
     	lazy(self#ast_exp_bool e2)
      | BinOp(EQ, e2, e1) when full_exp_eq e1 (Int(bi1, Reg(1))) ->
     	lazy(self#ast_exp_bool e2)
      | BinOp(EQ, e1, e2) when full_exp_eq e1 (Int(bi0, Reg(1))) ->
     	lazy(self#ast_exp_bool (UnOp(NOT, e2)))
      | BinOp(EQ, e2, e1) when full_exp_eq e1 (Int(bi0, Reg(1))) ->
     	lazy(self#ast_exp_bool (UnOp(NOT, e2)))
      | BinOp(EQ, e1, e2) ->
       (* These are predicates, which return boolean values. *)
        let t1 = Typecheck.infer_ast e1 in
        let t2 = Typecheck.infer_ast e2 in
        assert (t1 = t2);
        let sym, f =
	 (* If we can print as bool, we can use iff.  Otherwise, we
	    can use =. *)
	  if t1 = Reg(1) then (
            try
              self#ast_exp_bool_base ~check:true e1;
              self#ast_exp_bool_base ~check:true e2;
              "=", self#ast_exp_bool_base ~check:false
            with No_rule ->
              (try
                 self#ast_exp_base ~check:true e1;
                 self#ast_exp_base ~check:true e2;
                 "=", self#ast_exp_base ~check:false
               with No_rule ->
                 "=", self#ast_exp_bool)
          ) else
	    "=", self#ast_exp_bv
        in
        lazy(
	  pp "(";
	  pp sym;
	  space ();
	  f e1;
	  space ();
	  f e2;
	  pp ")";
	  cut ();
        )
      | BinOp((LT|LE|SLT|SLE) as op, e1, e2) ->
       (* These are predicates, which return boolean values. *)
        let t1 = Typecheck.infer_ast e1 in
        let t2 = Typecheck.infer_ast e2 in
        assert (t1 = t2);
        let f,pf = match op with
	  | LT -> "bvult", self#ast_exp
	  | LE -> "bvule", self#ast_exp
	  | SLT -> "bvslt", self#ast_exp
	  | SLE -> "bvsle", self#ast_exp
	  | _ -> assert false
        in
        lazy(
	  pp "(";
	  pp f;
	  space ();
	  pf e1;
	  space ();
	  pf e2;
	  pp ")";
	  cut ();
        )
     (* Z3 does not treat xor as associative; they said they would
	fix this on 3/28/2011. *)
      | BinOp(XOR as bop, e1, e2) ->
	let t = infer_ast e1 in
	let t' = infer_ast e2 in
    	if t <> t' then
    	  wprintf "Type mismatch: %s" (Pp.ast_exp_to_string e);
    	assert (t = t') ;
    	let fname = match bop with
    	  | XOR -> "xor"
	  | _ -> assert false
    	in
	lazy(
	  pc '(';
	  pp fname;
	  space ();
	  self#ast_exp_bool e1;
	  space ();
	  self#ast_exp_bool e2;
	  cut ();
	  pc ')'
	)
     | BinOp(OR, _, _) when parse_implies e <> None ->
     	 let e1, e2 = match parse_implies e with
     	   | Some(e1, e2) -> e1, e2
     	   | None -> assert false
     	 in
         let pe1, pe2 = lazy (self#ast_exp_bool e1), lazy (self#ast_exp_bool e2) in
         lazy(
     	   pp "(=>";
     	   space ();
           Lazy.force pe1;
     	   space ();
           Lazy.force pe2;
     	   cut ();
     	   pc ')';
         )
      | BinOp((AND|OR(*|XOR*)) as bop, e1, e2) ->
    	let t = infer_ast e1 in
    	let t' = infer_ast e2 in
    	if t <> t' then
    	  wprintf "Type mismatch: %s" (Pp.ast_exp_to_string e);
    	assert (t = t') ;
    	let fname = match bop with
    	  | AND -> "and"
    	  | OR -> "or"
    	  | XOR -> "xor"
	  | _ -> assert false
    	in
	let oplist = opflatten e in
	lazy(
    	  pc '('; pp fname; 
	  List.iter
	    (fun e -> space (); self#ast_exp_bool e) oplist;
	  pc ')';
	)
      | Cast((CAST_LOW|CAST_HIGH|CAST_UNSIGNED|CAST_SIGNED),t, e1) ->
     	let t1 = infer_ast e1 in
     	let (bitsnew, bitsold) = (bits_of_width t, bits_of_width t1) in
     	let delta = bitsnew - bitsold in
     	if delta = 0 
	then 
	  lazy(self#ast_exp_bool e1) 
	else 
	  raise No_rule
      | Var v ->
	let name,st = VH.find ctx v in
	(match st with
	| BitVec -> raise No_rule
	| Bool -> lazy(pp name))
      | Let(v, e1, e2) -> self#letme v e1 e2 Bool
      | _ -> raise No_rule
      ) in
    if not check then (
      opn 0; Lazy.force lazye; cut (); cls ()
    )

  (** Try to evaluate an expression to a boolean. If no good rule
      exists, uses bitvector conversion instead. *)
  method ast_exp_bool e =
    let t = Typecheck.infer_ast e in
    assert (t = Reg(1));
    if !use_booleans then (
      try
        self#tryit self#ast_exp_bool_base e
      with No_rule ->
	self#bv_to_bool ~check:false e
    ) else (
      try
        self#tryit self#bv_to_bool e
      with No_rule ->
	self#ast_exp_bool_base ~check:false e
    )

  method forall = function
    | [] -> ()
    | v::vars ->
	let var_type  (Var.V(_,_,t) as v) =
	  self#var v; pp " : "; self#typ t
	in
	opn 2;
	pp "FORALL (";space();
	(* TODO: group by type *)
	List.iter (fun v -> var_type v; pc ','; space()) vars;
	var_type v;
	pp "):";
	cls();space();

  method exists = function
    | [] -> ()
    | v::vars ->
	let var_type  (Var.V(_,_,t) as v) =
	  self#var v; pp " : "; self#typ t
	in
	opn 2;
	pp "EXISTS (";space();
	  (* TODO: group by type *)
	List.iter (fun v -> var_type v; pc ','; space()) vars;
	var_type v;
	pp "):";
	cls();space();

  method open_benchmark_with_logic logic =
    pp ("(set-logic "^logic); pc ')';
    force_newline();
    pp "(set-info :smt-lib-version 2.0)";
    force_newline();
    if setoption then (
      pp "(set-option :produce-assignments true)";
      force_newline()
    )

  method open_stream_benchmark =
    self#open_benchmark_with_logic "QF_ABV"

  method open_benchmark e =
    let has_mem e =
      let found_mem = ref false in
      let v = object(self)
	inherit Ast_visitor.nop
	method visit_exp = function
	  | Load _
	  | Store _ -> found_mem := true; SkipChildren
	  | Var v when not (is_integer_type (Var.typ v)) -> found_mem := true; SkipChildren
	  | _ when !found_mem -> SkipChildren
	  | _ -> DoChildren
      end
      in
      ignore(Ast_visitor.exp_accept v e);
      !found_mem
    in
    let get_logic e =
      match has_mem e with
      | true -> "QF_ABV"
      | false -> "QF_BV"
    in
    self#open_benchmark_with_logic (get_logic e)

  method close_benchmark =
    ()

  method assert_ast_exp_begin ?(exists=[]) ?(foralls=[]) () =
    force_newline();
    opn 1;
    pp "(assert";
    space();
    self#exists exists;
    self#forall foralls

  method assert_ast_exp_end =
    cut();
    pc ')';
    cls();
    force_newline ();
    self#formula

  method assert_ast_exp ?(exists=[]) ?(foralls=[]) e =
    pdebug "Opening benchmark... ";
    self#open_benchmark e;
    pdebug "declaring new free variables... ";
    self#declare_new_freevars e;
    self#assert_ast_exp_begin ~exists ~foralls ();
    self#ast_exp_bool e;
    self#assert_ast_exp_end;
    self#close_benchmark

  method valid_ast_exp_begin ?(exists=[]) ?(foralls=[]) () =
    self#assert_ast_exp_begin ~exists ~foralls ();
    pp "(not";
    space()

  method valid_ast_exp_end =
    pc ')';
    self#assert_ast_exp_end

  method valid_ast_exp ?exists ?foralls e =
    pdebug "Opening benchmark... ";
    self#open_benchmark e;
    pdebug "declaring new free variables... ";
    self#declare_new_freevars e;
    self#valid_ast_exp_begin ?exists ?foralls ();
    self#ast_exp_bool e;
    self#valid_ast_exp_end;
    self#close_benchmark

  (* (\** Is e a valid expression (always true)? *\) *)
  (* method valid_ast_exp ?(exists=[]) ?(foralls=[]) e = *)
  (*   self#open_benchmark (); *)
  (*   self#declare_new_freevars e; *)
  (*   force_newline(); *)
  (*   pp ":formula ("; *)
  (*   self#exists exists; *)
  (*   self#forall foralls; *)
  (*   pp "(= bv1[1] "; *)
  (*   force_newline(); *)
  (*   self#ast_exp e; *)
  (*   force_newline(); *)
  (*   pp "));"; *)
  (*   self#close_benchmark () *)


  method formula =
    pp "(check-sat)";
    force_newline();
    (* pp "(get-assignment)" *)
    pp "(exit)"

  method counterexample = ()

  method close =
    Format.pp_print_newline ft ();

end


class pp_oc ?opts fd =
  let ft = Format.formatter_of_out_channel fd in
object
  inherit pp ?opts ft as super
  inherit Formulap.fpp_oc
  inherit Formulap.stream_fpp_oc

  method close =
    super#close;
    close_out fd
end
