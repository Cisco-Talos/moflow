(** 
    The Abstract Syntax Tree.
    This IL allows nested expressions, making it closer to VEX and
    the concrete syntax than our SSA form. However, in most cases, this
    makes analysis harder, so you will generally want to convert to SSA
    for analysis.

    @author Ivan Jager
*)

open BatListFull
open Type
open Big_int_Z
open Big_int_convenience

type var = Var.t

type exp = 
  | Load of (exp * exp * exp * typ)  (** Load(arr,idx,endian, t) *)
  | Store of (exp * exp * exp * exp * typ)  (** Store(arr,idx,val, endian, t) *)
  | BinOp of (binop_type * exp * exp)
  | UnOp of (unop_type * exp)
  | Var of var
  | Lab of string
  | Int of (big_int * typ)
  | Cast of (cast_type * typ * exp) (** Cast to a new type. *)
  | Let of (var * exp * exp)
  | Unknown of (string * typ)
  (* Expression types below here are just syntactic sugar for the above *)
  | Ite of (exp * exp * exp)
  | Extract of (big_int * big_int * exp) (** Extract hbits to lbits of e (Reg type) *)
  | Concat of (exp * exp) (** Concat two reg expressions together *)

type attrs = Type.attributes

type stmt =
  | Move of (var * exp * attrs)  (** Assign the value on the right to the
				      var on the left *)
  | Jmp of (exp * attrs) (** Jump to a label/address *)
  | CJmp of (exp * exp * exp * attrs)
  (** Conditional jump. If e1 is true, jumps to e2, otherwise jumps to e3 *)
  | Label of (label * attrs) (** A label we can jump to *)
  | Halt of (exp * attrs)
  | Assert of (exp * attrs)
  | Assume of (exp * attrs)
  | Comment of (string * attrs) (** A comment to be ignored *)
  | Special of (string * attrs) (** A "special" statement. (does magic) *)

type program = stmt list

(* XXX: Should we move all of these to ast_convenience? *)

(** Make an expression corresponding to the given label, for use as the
    target of a [Jmp]. *)
let exp_of_lab = function
  | Name s -> Lab s
  | Addr a -> Int(big_int_of_int64 a, Reg 64)

(** If possible, make a label that would be refered to by the given
    expression. *)
let lab_of_exp = function
  | Lab s -> Some(Name s)
  | Int(i, t) ->
    Some(Addr(int64_of_big_int (Arithmetic.to_big_int (i,t))))
  | _ -> None
    

let reg_1 = Reg 1
and reg_8 = Reg 8
and reg_16 = Reg 16
and reg_32 = Reg 32
and reg_64 = Reg 64
and reg_128 = Reg 128

(** False constant. (If convenient, refer to this rather than building your own.) *)
let exp_false = Int(bi0, reg_1)
(** True constant. *)
let exp_true = Int(bi1, reg_1)

let little_endian = exp_false
let big_endian = exp_true

let newlab =
  let c = ref 0 in
  (fun ?(pref="newlabel_") () ->
     let i = !c in
     c := i + 1;
     Name(pref^string_of_int i))

let num_exp = function
  | Load _ -> 0
  | Store _ -> 1
  | Ite _ -> 2
  | Extract _ -> 3
  | Concat _ -> 4
  | BinOp _ -> 5
  | UnOp _ -> 6
  | Var _ -> 7
  | Lab _ -> 8
  | Int _ -> 9
  | Cast _ -> 10
  | Let _ -> 11
  | Unknown _ -> 12

  (* Returns elist, tlist, btlist, utlist, vlist, slist, ilist, clist *)
  let getargs = function
    | Load(e1,e2,e3,t1) -> [e1;e2;e3], [t1], [], [], [], [], [], []
    | Store(e1,e2,e3,e4,t1) -> [e1;e2;e3;e4], [t1], [], [], [], [], [], []
    | Ite(e1,e2,e3) -> [e1;e2;e3], [], [], [], [], [], [], []
    | Extract(h,l,e) -> [e], [], [], [], [], [], [h;l], []
    | Concat(le, re) -> [le;re], [], [], [], [], [], [], []
    | BinOp(bt,e1,e2) -> [e1;e2], [], [bt], [], [], [], [], []
    | UnOp(ut,e1) -> [e1], [], [], [ut], [], [], [], []
    | Var(v1) -> [], [], [], [], [v1], [], [], []
    | Lab(s1) -> [], [], [], [], [], [s1], [], []
    | Int(i1,t1) -> [], [t1], [], [], [], [], [i1], []
    | Cast(c1,t1,e1) -> [e1], [t1], [], [], [], [], [], [c1]
    | Let(v1,e1,e2) -> [e1;e2], [], [], [], [v1], [], [], []
    | Unknown(s1,t1) -> [], [t1], [], [], [], [s1], [], []

(*
(** quick_exp_eq e1 e2 returns true if and only if the subexpressions
    in e1 and e2 are *physically* equal. *)
let quick_exp_eq e1 e2 =
  if (num_exp e1) <> (num_exp e2) then false else
    let l1,l2,l3,l4,l5,l6,l7,l8 = getargs e1 in
    let r1,r2,r3,r4,r5,r6,r7,r8 = getargs e2 in
    let b1 = List.for_all2 (==) l1 r1 in
    let b2 = List.for_all2 (==) l2 r2 in
    let b3 = List.for_all2 (==) l3 r3 in
    let b4 = List.for_all2 (==) l4 r4 in
    let b5 = List.for_all2 (==) l5 r5 in
    let b6 = List.for_all2 (==) l6 r6 in
    let b7 = List.for_all2 (==) l7 r7 in
    let b8 = List.for_all2 (==) l8 r8 in
    if b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 then
      true else false
*)

let quick_exp_eq e1 e2 =
  match e1, e2 with
    | Int (i1, t1), Int (i2, t2) ->
      t1 == t2 && i1 == i2
    | Var v1, Var v2 ->
      v1 == v2
    | BinOp (op1, e11, e12), BinOp (op2, e21, e22) ->
      op1 == op2 && e11 == e21 && e12 == e22
    | UnOp (op1, e1), UnOp (op2, e2) ->
      op1 == op2 && e1 == e2
    | Cast (ct1, t1, e1), Cast (ct2, t2, e2) ->
      ct1 == ct2 && t1 == t2 && e1 == e2
    | Extract (h1, l1, e1), Extract (h2, l2, e2) ->
      h1 == h2 && l1 == l2 && e1 == e2
    | Concat (e11, e12), Concat (e21, e22) ->
      e11 == e21 && e12 == e22
    | Lab l1, Lab l2 ->
      l1 == l2
    | Ite (b11, e11, e12), Ite (b21, e21, e22) ->
      b11 == b21 && e11 == e21 && e12 == e22
    | Load (e11, e12, e13, t1), Load (e21, e22, e23, t2) ->
      t1 == t2 && e11 == e21 && e12 == e22 && e13 == e23
    | Store (e11, e12, e13, e14, t1), Store (e21, e22, e23, e24, t2) ->
      t1 == t2 && e11 == e21 && e12 == e22 && e13 == e23 && e14 == e24
    | Let (v1, e11, e12), Let (v2, e21, e22) ->
      v1 == v2 && e11 == e21 && e12 == e22
    | Unknown (s1, t1), Unknown (s2, t2) ->
      s1 == s2 && t1 == t2
    | _, _ ->
      false

(** full_exp_eq e1 e2 returns true if and only if e1 and e2 are
    structurally equivalent. *)
let rec full_exp_eq e1 e2 = e1 = e2
(*   if (num_exp e1) <> (num_exp e2) then false else *)
(*     let l1,l2,l3,l4,l5,l6,l7,l8 = getargs e1 in *)
(*     let r1,r2,r3,r4,r5,r6,r7,r8 = getargs e2 in *)
(*     let b1 = List.for_all2 (==) l1 r1 in (\* e must be == *\) *)
(*     let b2 = List.for_all2 (=) l2 r2 in *)
(*     let b3 = List.for_all2 (=) l3 r3 in *)
(*     let b4 = List.for_all2 (=) l4 r4 in *)
(*     let b5 = List.for_all2 (Var.equal) l5 r5 in *)
(*     let b6 = List.for_all2 (=) l6 r6 in *)
(*     let b7 = List.for_all2 (eq_big_int) l7 r7 in *)
(*     let b8 = List.for_all2 (=) l8 r8 in *)
(*     if b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 then *)
(*       true *)
(*     else if b2 & b3 & b4 & b5 & b6 & b7 & b8 then *)
(* (\* e1 and e2 are not physically equal.  But maybe the subexpressions *)
(*    are structurally, but not physically, equal. *\) *)
(*       List.for_all2 full_exp_eq l1 r1 *)
(*     else (\* If something else differs, we are definitely not equal. *\) *)
(*       false *)

let (===) = full_exp_eq

let rec compare_exp e1 e2 = compare e1 e2
  (* let c = compare (num_exp e1) (num_exp e2) in *)
  (* if c <> 0 then c else *)
  (*   let l1,l2,l3,l4,l5,l6,l7,l8 = getargs e1 in *)
  (*   let r1,r2,r3,r4,r5,r6,r7,r8 = getargs e2 in *)
  (*   (\* Put each comparison in a list as a lazy computation *\) *)
  (*   let l = *)
  (*     lazy (compare l2 r2) *)
  (*     :: lazy (compare l3 r3) *)
  (*     :: lazy (compare l4 r4) *)
  (*     :: lazy (compare l5 r5) *)
  (*     :: lazy (compare l6 r6) *)
  (*     :: lazy (compare l8 r8) *)
  (*     :: lazy (Util.list_compare compare_big_int l7 r7) *)
  (*     :: lazy (Util.list_compare compare_exp l1 r1) *)
  (*     :: [] *)
  (*   in *)
  (*   (\* Compute each comparison, and stop when we get a non-zero *\) *)
  (*   match List.fold_left *)
  (*     (fun acc lz -> match acc with *)
  (*     | Some _ as x -> x *)
  (*     | None -> *)
  (*       (match Lazy.force lz with *)
  (*       | 0 -> None *)
  (*       | x -> Some(x)) *)
  (*     ) None l *)
  (*   with *)
  (*   | Some(x) -> x *)
  (*   | None -> 0 *)

let num_stmt = function
  | Move _ -> 0
  | Jmp _ -> 1
  | CJmp _ -> 2
  | Label _ -> 3
  | Halt _ -> 4
  | Assert _ -> 5
  | Assume _ -> 6
  | Comment _ -> 7
  | Special _ -> 8

let getargs_stmt = function
  | Move(v,e,a) -> [e], [v], [], [a], []
  | CJmp(e1,e2,e3,a) -> [e1;e2;e3], [], [], [a], []
  | Label(l,a) -> [], [], [l], [a], []
  | Jmp(e,a)
  | Halt(e,a)
  | Assert(e,a)
  | Assume(e,a) -> [e], [], [], [a], []
  | Comment(s,a)
  | Special(s,a) -> [], [], [], [a], [s]

(** quick_stmt_eq returns true if and only if the subexpressions in e1
    and e2 are *physically* equal. *)
let quick_stmt_eq s1 s2 =
  if (num_stmt s1) <> (num_stmt s2) then false else
    let l1,l2,l3,l4,l5 = getargs_stmt s1 in
    let r1,r2,r3,r4,r5 = getargs_stmt s2 in
    let b1 = List.for_all2 (==) l1 r1 in
    let b2 = List.for_all2 (==) l2 r2 in
    let b3 = List.for_all2 (==) l3 r3 in
    let b4 = List.for_all2 (==) l4 r4 in
    let b5 = List.for_all2 (==) l5 r5 in
    if b1 & b2 & b3 & b4 & b5 then
      true
    else if b2 & b3 & b4 & b5 then
      (* e1 and e2 are not physically equal.  But maybe their subexpressions
	 are physically equal. *)
      List.for_all2 quick_exp_eq l1 r1
    else
      false

(** full_stmt_eq returns true if and only if e1 and e2 are
    structurally equivalent. *)
let full_stmt_eq s1 s2 = s1 = s2
(*   if (num_stmt s1) <> (num_stmt s2) then false else *)
(*     let l1,l2,l3,l4,l5 = getargs_stmt s1 in *)
(*     let r1,r2,r3,r4,r5 = getargs_stmt s2 in *)
(*     let b1 = List.for_all2 (==) l1 r1 in (\* e must use == *\) *)
(*     let b2 = List.for_all2 (=) l2 r2 in *)
(*     let b3 = List.for_all2 (=) l3 r3 in *)
(*     let b4 = List.for_all2 (=) l4 r4 in *)
(*     let b5 = List.for_all2 (=) l5 r5 in *)
(*     if b1 & b2 & b3 & b4 & b5 then *)
(*       true *)
(*     else if b2 & b3 & b4 & b5 then *)
(* (\* e1 and e2 are not physically equal.  But maybe the subexpressions *)
(*    are structurally, but not physically, equal. *\) *)
(*       List.for_all2 full_exp_eq l1 r1 *)
(*     else *)
(*       false *)

let is_indirectjump s =
  let is_indirect_exp e = match lab_of_exp e with
    | Some _ -> false
    | None -> true
  in
match s with
| Jmp(e, _) -> is_indirect_exp e (* Only jumps to labels are direct *)
| CJmp(_, e1, e2, _) -> is_indirect_exp e1 || is_indirect_exp e2
| _ -> false

let is_syscall = function
  | Special(("syscall"|"int 80"), _) -> true
  | _ -> false

let full_stmts_eq s1 s2 =
  if (List.length s1) <> (List.length s2) then false
  else List.for_all2 full_stmt_eq s1 s2

let is_true a = a === exp_true
let is_false a = a === exp_false

let get_attrs = function
  | Move(_,_,a)
  | Jmp(_,a)
  | CJmp(_,_,_,a)
  | Label(_,a)
  | Halt(_,a)
  | Assert(_,a)
  | Assume(_,a)
  | Comment(_,a)
  | Special(_,a) -> a
