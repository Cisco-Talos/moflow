(* Apply a mapping to all expressions in a program.

   @author Thanassis
*)

open Ast
open Ast_visitor

class type map = object
  method exp : exp -> exp
  method stmt : stmt -> stmt
  method prog : program -> program
  method cfg : Cfg.AST.G.t -> Cfg.AST.G.t
end

let map_e f =
  let visitor = object(self)
    inherit nop
    method visit_exp = f
  end
  in
  let mapper =
    object(self)
      method exp = exp_accept visitor
      method stmt = stmt_accept visitor
      method prog = prog_accept visitor
      method cfg = cfg_accept visitor
    end
  in
  (mapper :> map)
