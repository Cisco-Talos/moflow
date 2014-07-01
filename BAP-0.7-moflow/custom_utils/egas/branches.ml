module type BRANCH_IN = sig
  type lbl_t
  type dir_t = bool
  type exp_t

  type form_acc_t

  val produce_path_formula : form_acc_t -> exp_t -> (form_acc_t * Ast.exp)
  val label2str : lbl_t -> string
end

module type BRANCH_OUT = 
sig
  type lbl_t 
  type dir_t = bool
  type exp_t 
  type form_acc_t 
  type t = {lbl:lbl_t; dir:dir_t; exp:exp_t}

  val mk_branch : lbl_t -> dir_t -> exp_t -> t
  val get_label : t -> lbl_t
  val label2str : lbl_t -> string
  val get_dir : t -> dir_t
  val get_assert : t -> exp_t
  val produce_path_formula : form_acc_t -> t -> (form_acc_t * Ast.exp)
end

module MakeBranch(B:BRANCH_IN) =
struct
  type lbl_t = B.lbl_t
  type dir_t = B.dir_t
  type exp_t = B.exp_t
  type form_acc_t = B.form_acc_t

  (* Symb. exec. results in a list of "branches" corresponding to conditional 
   * jumps found during trace execution.
   * lbl - label of cond. jump
   * dir - true/false depending of which branch was taken
   * exp - functional expression encoding the jump's condition
   *)
  type t = {lbl:lbl_t; dir:dir_t; exp:exp_t}

  let mk_branch lbl dir exp = {lbl=lbl;dir=dir;exp=exp}
  let get_label {lbl=lbl} = lbl
  let get_dir {dir=dir} = dir
  let get_assert {exp=exp} = exp
  let produce_path_formula form_acc {exp=exp} = 
    B.produce_path_formula form_acc exp

  let label2str = B.label2str

end

