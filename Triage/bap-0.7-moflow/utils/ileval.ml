let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

open Ast

type ast = Ast.program
type astcfg = Cfg.AST.G.t
type ssa = Cfg.SSA.G.t

type prog =
  | Ast of ast
  | AstCfg of astcfg
  | Ssa of ssa

type cmd = 
  | AnalysisAst of (ast -> unit)
  | AnalysisAstCfg of (astcfg -> unit)
  | AnalysisSsa of (ssa -> unit)
  | TransformAst of (ast -> ast)
  | TransformAstCfg of (astcfg -> astcfg)
  | TransformSsa of (ssa -> ssa)
  | ToCfg
  | ToAst
  | ToSsa
 (* add more *)

let pipeline = ref []

(* Initialization statements *)
let inits = ref []

(* Variable initializations *)
let lazyinits = ref []

let scope = ref (Grammar_private_scope.default_scope ())

let init_stmts () =
  List.fold_left (fun l (v,e) -> Move(v,e,[])::l) [] !inits

let cexecute_at s p =
  ignore(Symbeval.concretely_execute p ~i:(init_stmts ()) ~s)

let cexecute p =
  ignore(Symbeval.concretely_execute p ~i:(init_stmts ()))

let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

let mapv v e =
  let e,ns = Parser.exp_from_string ~scope:!scope e in
  let t = Typecheck.infer_ast e in
  let ts = Pp.typ_to_string t in
  let v,ns = match Parser.exp_from_string ~scope:ns (v ^ ":" ^ ts) with
    | Var(v), ns -> v, ns
    | _ -> assert false
  in
  scope := ns;
  inits := (v, e) :: !inits
  (* let s = Move(v, e, []) in *)
  (* inits := s :: !inits *)

let mapmem a e =
  let a,ns = Parser.exp_from_string ~scope:!scope a in
  let e,ns = Parser.exp_from_string ~scope:ns e in
  let t = Typecheck.infer_ast e in
  let m,ns = match Parser.exp_from_string ~scope:ns "mem:?u32" with
    | Var(v), ns -> v, ns
    | _ -> assert false
  in
  scope := ns;
  (* let s = Move(m, Store(Var(m), a, e, exp_false, t), []) in *)
  inits := (m, Store(Var(m), a, e, exp_false, t)) :: !inits
  (* inits := s :: !inits *)

let jitexecute p = Utils_common.jitexecute (List.rev !inits) p

let speclist =
  ("-eval", 
     Arg.Unit (fun () -> add(AnalysisAst cexecute)),
     "Concretely execute the IL from the beginning of the program")
  ::("-eval-at", 
     Arg.String (fun s -> add(AnalysisAst (cexecute_at (Int64.of_string s)))),
     "<pc> Concretely execute the IL from pc")
  ::("-jiteval",
     Arg.Unit (fun () -> add(AnalysisAst jitexecute)),
     "Concretely execute the IL using the LLVM JIT compiler")
  ::("-init-var",
     Arg.Tuple 
       (let vname = ref "" and vval = ref "" in
	[
	  Arg.Set_string vname; Arg.Set_string vval;
	  Arg.Unit (fun () ->
            let vname = !vname and vval = !vval in
            lazyinits := lazy (mapv vname vval) :: !lazyinits)
	]),
     "<var> <expression> Set variable to expression before evaluation.")
  ::("-init-mem",
     Arg.Tuple 
       (let maddr = ref "" and mval = ref "" in
	[
	  Arg.Set_string maddr; Arg.Set_string mval;
          let maddr = !maddr and mval = !mval in
	  Arg.Unit (fun () -> lazyinits := lazy (mapmem maddr mval) :: !lazyinits)
	]),
     "<addr expression> <value expression> Set variable to expression before evaluation.")
  :: Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline

let prog =
  try let p,s = Input.get_program() in
      (* Save scope for expression parsing *)
      scope := s;
      p
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1
;;

(* Now handle the initializations, since we have the program's scope
   available *)
List.iter Lazy.force (List.rev !lazyinits)
;;

let rec apply_cmd prog = function
  | AnalysisAst f -> (
    match prog with
    | Ast p as p' -> f p; p'
    | _ -> failwith "need explicit translation to AST"
  )
  | AnalysisAstCfg f -> (
    match prog with
    | AstCfg p as p' -> f p; p'
    | _ -> failwith "need explicit translation to AST CFG"
  )
  | AnalysisSsa f -> (
    match prog with
    | Ssa p as p' -> f p; p'
    | _ -> failwith "need explicit translation to SSA"
  )
  | TransformAst f -> (
    match prog with
    | Ast p -> Ast(f p)
    | _ -> failwith "need explicit translation to AST"
  )
  | TransformAstCfg f -> (
    match prog with
    | AstCfg p -> AstCfg(f p)
    | _ -> failwith "need explicit translation to AST CFG"
  )
  | TransformSsa f -> (
    match prog with
    | Ssa p -> Ssa(f p)
    | _ -> failwith "need explicit translation to SSA"
  )
  | ToCfg -> (
    match prog with
    | Ast p -> AstCfg(Cfg_ast.of_prog p)
    | Ssa p -> AstCfg(Cfg_ssa.to_astcfg p)
    | AstCfg _ as p -> prerr_endline "Warning: null transformation"; p
  )
  | ToAst -> (
    match prog with
    | AstCfg p -> Ast(Cfg_ast.to_prog p)
    | p -> apply_cmd (apply_cmd p ToCfg) ToAst
  )
  | ToSsa -> (
    match prog with
    | AstCfg p -> Ssa(Cfg_ssa.of_astcfg p)
    | p -> apply_cmd (apply_cmd p ToCfg) ToSsa
  )
;;

List.fold_left apply_cmd (Ast prog) pipeline


