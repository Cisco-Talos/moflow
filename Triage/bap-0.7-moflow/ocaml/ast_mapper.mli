(** Apply a mapping to all expressions in a program.

   @author Thanassis
*)

open Ast
open Type

(** A mapping object. *)
class type map = object
  method exp : exp -> exp
  method stmt : stmt -> stmt
  method prog : program -> program
  method cfg : Cfg.AST.G.t -> Cfg.AST.G.t
end

(** [map_e f] returns a map object for [f].

    For instance, to map all expressions in a cfg, use [(map_e f)#cfg
    cfg]. *)
val map_e : (exp -> exp visit_action) -> map
