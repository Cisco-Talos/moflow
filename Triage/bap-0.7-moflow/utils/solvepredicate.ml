open Ast
open Ast_convenience
open Grammar_scope
open Type
open Vc
open Utils_common

let usage = "Usage: "^Sys.argv.(0)^" <options> <formula filename> \n\
             Solve predicates"

let fname = ref None
let solver = ref (Smtexec.STP.si);;

let set_solver s =
  solver := try Hashtbl.find Smtexec.solvers s
  with Not_found ->
    failwith "Unknown solver"

let solvers = Hashtbl.fold (fun k _ s -> k ^ " " ^ s) Smtexec.solvers ""

let speclist =
  ("-solver", Arg.String set_solver,
   ("Use the specified solver. Choices: " ^ solvers))
  :: []

let () = Tunegc.set_gc ()
let anon x =
  match !fname with
  | None -> fname := Some x
  | Some _ -> raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () =
  try Arg.parse speclist anon usage;
      match !fname with
      | None -> raise(Arg.Bad("Expected a formula filename"))
      | Some _ -> ()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1
;;

Printf.fprintf stderr "Solving\n"; flush stderr;
let r = (!solver)#solve_formula_file ~getmodel:true (BatOption.get !fname) in
Printf.fprintf stderr "Solve result: %s\n" (Smtexec.result_to_string r);
match r with | Smtexec.SmtError _ -> failwith "Solver error" | _ -> ()
;;


