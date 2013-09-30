(* Verification conditions

   XXX: Rename functions
*)

module D=Debug.Make(struct let name = "Vc" and default = `NoDebug end)
open D
open Utils_common
open Type

type options = {
  k : int;
  mode : formula_mode;
  full_subst : bool;
}

let default_options = {
  k = 1;
  mode = Sat;
  full_subst = true;
}

type ast_vc = options -> Ast.program -> Ast.exp -> Ast.exp * Ast.var list
type cfg_vc = options -> Cfg.AST.G.t -> Ast.exp -> Ast.exp * Ast.var list
type ssa_vc = options -> Cfg.SSA.G.t -> Ast.exp -> Ast.exp * Ast.var list

type t = | AstVc of ast_vc
         | CfgVc of cfg_vc
         | SsaVc of ssa_vc

let vc_astprog vc options prog post  = match vc with
  | AstVc vc -> vc options prog post
  | CfgVc vc -> vc options (Cfg_ast.of_prog prog) post
  | SsaVc vc ->
    let cfg = Cfg_ast.of_prog prog in
    let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
    let post = rename_astexp tossa post in
    vc options cfg post

let vc_astcfg vc options prog post  = match vc with
  | AstVc vc -> vc options (Cfg_ast.to_prog prog) post
  | CfgVc vc -> vc options prog post
  | SsaVc vc ->
    let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg prog in
    let post = rename_astexp tossa post in
    vc options cfg post

let vc_ssacfg vc options prog post  = match vc with
  | AstVc vc -> vc options (Cfg_ssa.to_ast prog) post
  | CfgVc vc -> vc options (Cfg_ssa.to_astcfg prog) post
  | SsaVc vc -> vc options prog post

let compute_dwp1 _ cfg post =
  let (gcl, foralls) = Gcl.passified_of_ssa ~mode:Foralls cfg in
  let (moreforalls, wp) = Wp.dwp_1st gcl post in
  (wp, moreforalls@foralls)
let compute_dwp1_gen = SsaVc compute_dwp1

let compute_wp _ cfg post =
  let gcl = Gcl.of_astcfg cfg in
  (Wp.wp gcl post, [])
let compute_wp_gen = CfgVc compute_wp

let compute_passified_wp {mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa ~mode cfg in
  (Wp.passified_wp gcl post, foralls)
let compute_passified_wp_gen = SsaVc compute_passified_wp

let compute_uwp _ cfg post =
  let ugcl = Gcl.Ugcl.of_ssacfg cfg in
  (Wp.dijkstra_uwp ugcl post, [])
let compute_uwp_gen = SsaVc compute_uwp

let compute_uwp_efficient {mode=mode} cfg post =
  let ugcl = Gcl.Ugcl.of_ssacfg ~mode cfg in
  (Wp.efficient_uwp ugcl post, [])
let compute_uwp_efficient_gen = SsaVc compute_uwp_efficient

let compute_dwp {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa ~mode cfg in
  (Wp.dwp ~k mode gcl post, foralls)
let compute_dwp_gen = SsaVc compute_dwp

let compute_dwp_let {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa ~mode cfg in
  (Wp.dwp_let ~k mode gcl post, foralls)
let compute_dwp_let_gen = SsaVc compute_dwp_let

let compute_fwp {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa ~mode cfg in
  (Fwp.fwp ~k mode gcl post, foralls)
let compute_fwp_gen = SsaVc compute_fwp

let compute_fwp_uwp {k=k; mode=mode} cfg post =
  let ugcl = Gcl.Ugcl.of_ssacfg ~mode cfg in
  (Fwp.fwp_uwp ~k mode ugcl post, [])
let compute_fwp_uwp_gen = SsaVc compute_fwp_uwp

let compute_fwp_lazyconc {k=k; mode=mode} cfg post =
  (* Do not allow Gcl to rewrite Assigns.  We need to see the
     Assignments to do concrete evaluation. *)
  let gcl, foralls = Gcl.passified_of_ssa ?mode:None cfg in
  (Fwp.fwp_lazyconc ~k mode gcl post, foralls)
let compute_fwp_lazyconc_gen = SsaVc compute_fwp_lazyconc

let compute_fwp_lazyconc_uwp {k=k; mode=mode} cfg post =
  (* Do not allow Gcl to rewrite Assigns.  We need to see the
     Assignments to do concrete evaluation. *)
  let ugcl = Gcl.Ugcl.of_ssacfg cfg in
  (Fwp.fwp_lazyconc_uwp ~k mode ugcl post, [])
let compute_fwp_lazyconc_uwp_gen = SsaVc compute_fwp_lazyconc_uwp

let compute_flanagansaxe {k=k; mode=mode} cfg post =
  let gcl, foralls = Gcl.passified_of_ssa ~mode cfg in
  (Wp.flanagansaxe ~k mode gcl post, foralls)
let compute_flanagansaxe_gen = SsaVc compute_flanagansaxe

let compute_fse_bfs {full_subst=full_subst} ast post =
  let bfs = if not full_subst then Symbeval_search.bfs_ast_program_fast
    else Symbeval_search.bfs_ast_program
  in
  (bfs ast post, [])
let compute_fse_bfs_gen = AstVc compute_fse_bfs

let compute_fse_bfs_maxdepth i {full_subst=full_subst} ast post =
  let bfs = if not full_subst then Symbeval_search.bfs_maxdepth_ast_program_fast
    else Symbeval_search.bfs_maxdepth_ast_program
  in
  (bfs i ast post, [])
let compute_fse_bfs_maxdepth_gen i = AstVc (compute_fse_bfs_maxdepth i)

let compute_fse_maxrepeat i {full_subst=full_subst} ast post =
  let maxrepeat = if not full_subst then Symbeval_search.maxrepeat_ast_program_fast
    else Symbeval_search.maxrepeat_ast_program
  in
  (maxrepeat i ast post, [])
let compute_fse_maxrepeat_gen i = AstVc (compute_fse_maxrepeat i)

let vclist =
  ("dwp", compute_dwp_gen)
  :: ("fwp", compute_fwp_gen)
  :: ("fwp_uwp", compute_fwp_uwp_gen)
  :: ("fwp_lazyconc", compute_fwp_lazyconc_gen)
  :: ("fwp_lazyconc_uwp", compute_fwp_lazyconc_uwp_gen)
  :: ("dwplet", compute_dwp_let_gen)
  :: ("dwp1", compute_dwp1_gen)
  :: ("pwp", compute_passified_wp_gen)
  :: ("flanagansaxe", compute_flanagansaxe_gen)
  :: ("wp", compute_wp_gen)
  :: ("uwp", compute_uwp_gen)
  :: ("uwpe", compute_uwp_efficient_gen)
  :: ("fse-bfs", compute_fse_bfs_gen)
  :: []

let pred_vclist =
  ("dwp", compute_dwp_gen)
  :: ("fwp", compute_fwp_gen)
  :: ("fwp_uwp", compute_fwp_uwp_gen)
  :: ("fwp_lazyconc", compute_fwp_lazyconc_gen)
  :: ("fwp_lazyconc_uwp", compute_fwp_lazyconc_uwp_gen)
  :: ("dwplet", compute_dwp_let_gen)
  :: ("pwp", compute_passified_wp_gen)
  :: ("flanagansaxe", compute_flanagansaxe_gen)
  :: ("wp", compute_wp_gen)
  :: ("uwp", compute_uwp_gen)
  :: ("uwpe", compute_uwp_efficient_gen)
  :: ("fse-bfs", compute_fse_bfs_gen)
  :: []
