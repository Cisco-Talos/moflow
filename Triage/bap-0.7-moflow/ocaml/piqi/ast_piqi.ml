module V = Var
open Ast
open Stmt_piqi
open Type

let casttype_to_piqi : Type.cast_type -> Stmt_piqi.cast_type = function
  | CAST_UNSIGNED -> `cast_unsigned
  | CAST_SIGNED -> `cast_signed
  | CAST_HIGH -> `cast_high
  | CAST_LOW -> `cast_low

let unop_to_piqi : Type.unop_type -> Stmt_piqi.unop_type = function
  | NEG -> `neg
  | NOT -> `not

let binop_to_piqi : Type.binop_type -> Stmt_piqi.binop_type = function
  | PLUS -> `plus
  | MINUS -> `minus
  | TIMES -> `times
  | DIVIDE -> `divide
  | SDIVIDE -> `sdivide
  | MOD -> `modbop
  | SMOD -> `smod
  | LSHIFT -> `lshift
  | RSHIFT -> `rshift
  | ARSHIFT -> `arshift
  | AND -> `andbop
  | OR -> `orbop
  | XOR -> `xor
  | EQ -> `eq
  | NEQ -> `neq
  | LT -> `lt
  | LE -> `le
  | SLT -> `slt
  | SLE -> `sle

let rec type_to_piqi : Type.typ -> Stmt_piqi.typ = function
  | Reg n -> `reg (n)
  | TMem t -> `tmem ({Tmem.index_type=type_to_piqi t})
  | Array(t, t') -> `array ({Array.index_type=type_to_piqi t; element_type=type_to_piqi t';})

let var_to_piqi (V.V(id, n, t)) : Stmt_piqi.var =
  {Var.name=n; Var.id=id; Var.typ = type_to_piqi t}

let rec exp_to_piqi : Ast.exp -> Stmt_piqi.exp = function
  | Load(m, i, e, t) ->
    let m = exp_to_piqi m in
    let i = exp_to_piqi i in
    let e = exp_to_piqi e in
    let t = type_to_piqi t in
    `load({Load.memory=m; Load.address=i; Load.endian=e; Load.typ=t;})
  | Store(m, i, v, e, t) ->
    let m = exp_to_piqi m in
    let i = exp_to_piqi i in
    let v = exp_to_piqi v in
    let e = exp_to_piqi e in
    let t = type_to_piqi t in
    `store({Store.memory=m; Store.address=i; Store.value=v; Store.endian=e; Store.typ=t;})
  | BinOp(bop, e1, e2) ->
    let bop = binop_to_piqi bop in
    let e1 = exp_to_piqi e1 in
    let e2 = exp_to_piqi e2 in
    `binop({Binop.binop_type=bop; Binop.lexp=e1; Binop.rexp=e2})
  | UnOp(uop, e) ->
    let uop = unop_to_piqi uop in
    let e = exp_to_piqi e in
    `unop({Unop.unop_type=uop; Unop.exp=e})
  | Var v ->
    `var(var_to_piqi v)
  | Lab s ->
    `lab s
  | Int(i,t) ->
    `inte({Inte.int=Big_int_Z.string_of_big_int i; Inte.typ=type_to_piqi t})
  | Cast(ct, t, e) ->
    let ct = casttype_to_piqi ct in
    let t = type_to_piqi t in
    let e = exp_to_piqi e in
    `cast({Cast.cast_type=ct; Cast.new_type=t; Cast.exp=e})
  | Let(v, e, e') ->
    let v = var_to_piqi v in
    let e = exp_to_piqi e in
    let e' = exp_to_piqi e' in
    `let_exp({Let_exp.var=v; Let_exp.e1=e; Let_exp.e2=e'})
  | Unknown(s, t) ->
    let t = type_to_piqi t in
    `unknown({Unknown.string=s; Unknown.typ=t})
  | Ite(e, te, fe) ->
    let e = exp_to_piqi e in
    let te = exp_to_piqi te in
    let fe = exp_to_piqi fe in
    `ite({Ite.condition=e; Ite.iftrue=te; Ite.iffalse=fe})
  | Extract(h, l, e) ->
    let h = Big_int_Z.int_of_big_int h in
    let l = Big_int_Z.int_of_big_int l in
    let e = exp_to_piqi e in
    `extract({Extract.hbit=h; Extract.lbit=l; Extract.exp=e})
  | Concat(e1, e2) ->
    let e1 = exp_to_piqi e1 in
    let e2 = exp_to_piqi e2 in
    `concat({Concat.le=e1; Concat.re=e2})

let operand_usage_to_piqi : Type.usage -> Stmt_piqi.operand_usage = function
  | RD -> {Operand_usage.read=true; Operand_usage.written=false; Operand_usage.index=false; Operand_usage.base=false}
  | WR -> {Operand_usage.read=false; Operand_usage.written=true; Operand_usage.index=false; Operand_usage.base=false}
  | RW -> {Operand_usage.read=true; Operand_usage.written=true; Operand_usage.index=false; Operand_usage.base=false}

let taint_to_piqi : Type.taint_type -> Stmt_piqi.taint_info = function
  | Taint 0 -> `no_taint
  | Taint (-1) -> `taint_multiple
  | Taint n -> `taint_id (Int64.of_int n)

let context_to_piqi {name; mem; t; index; value; usage; taint} : Stmt_piqi.context =
  let oi = if mem then
      `mem_operand({Mem_operand.address=index})
    else
      `reg_operand({Reg_operand.name=name})
  in
  {Context.operand_info_specific=oi;
   Context.bit_length=Typecheck.bits_of_width t;
   Context.operand_usage=operand_usage_to_piqi usage;
   Context.taint_info=taint_to_piqi taint;
   Context.value=Big_int_Z.string_of_big_int value}

let attribute_to_piqi : Type.attribute -> Stmt_piqi.attribute = function
  | Asm s -> `asm s
  | Address a -> `address a
  | Liveout -> `liveout ({Liveout._dummy=()})
  | Synthetic -> `synthetic ({Synthetic._dummy=()})
  | ThreadId i -> `thread_id (Int64.of_int i)
  | Context c -> `context (context_to_piqi c)
  | StrAttr s -> `strattr s
  | a -> `other (Pp.attr_to_string a)

let attributes_to_piqi = List.map attribute_to_piqi

let label_to_piqi : Type.label -> Stmt_piqi.label = function
  | Name n -> `name n
  | Addr a -> `addr a

let stmt_to_piqi : Ast.stmt -> Stmt_piqi.stmt = function
  | Move(v, e, attrs) ->
    let v = var_to_piqi v in
    let e = exp_to_piqi e in
    let attrs = attributes_to_piqi attrs in
    `move({Move.var=v; Move.exp=e; Move.attributes=attrs})
  | Jmp(e, attrs) ->
    let e = exp_to_piqi e in
    let attrs = attributes_to_piqi attrs in
    `jmp({Jmp.exp=e; Jmp.attributes=attrs})
  | CJmp(e, e1, e2, attrs) ->
    let e = exp_to_piqi e in
    let e1 = exp_to_piqi e1 in
    let e2 = exp_to_piqi e2 in
    let attrs = attributes_to_piqi attrs in
    `cjmp({Cjmp.cond=e; Cjmp.iftrue=e1; Cjmp.iffalse=e2; Cjmp.attributes=attrs})
  | Label(l, attrs) ->
    let l = label_to_piqi l in
    let attrs = attributes_to_piqi attrs in
    `label_stmt({Label_stmt.label=l; Label_stmt.attributes=attrs})
  | Halt(e, attrs) ->
    let e = exp_to_piqi e in
    let attrs = attributes_to_piqi attrs in
    `halt({Halt.exp=e; Halt.attributes=attrs})
  | Assert(e, attrs) ->
    let e = exp_to_piqi e in
    let attrs = attributes_to_piqi attrs in
    `assert_stmt({Assert_stmt.exp=e; Assert_stmt.attributes=attrs})
  | Assume(e, attrs) ->
    let e = exp_to_piqi e in
    let attrs = attributes_to_piqi attrs in
    `assume({Assume.exp=e; Assume.attributes=attrs})
  | Comment(s, attrs) ->
    let attrs = attributes_to_piqi attrs in
    `comment({Comment.string=s; Comment.attributes=attrs})
  | Special(s, attrs) ->
    let attrs = attributes_to_piqi attrs in
    `special({Special.string=s; Special.attributes=attrs})

let prog_to_piqi = List.map stmt_to_piqi

let to_pb p = Stmt_piqi_ext.gen_program (prog_to_piqi p) `pb
let to_json p = Stmt_piqi_ext.gen_program (prog_to_piqi p) `json
let to_xml p = Stmt_piqi_ext.gen_program (prog_to_piqi p) `xml
