(**
   $Id: codegen.ml 6167 2012-05-15 00:43:53Z edmcman $

   Code generation utility.
*)

open Ast
open Big_int_convenience
open Llvm_codegen

let usage = "Usage: "^Sys.argv.(0)^" <input options> [options]\n\
             Translate programs to the IL. "

let out = ref None
let exec = ref false
let memimpl = ref Llvm_codegen.FuncMulti

let speclist =
  ("-o", Arg.String (fun f -> out := Some(open_out f)),
   "<file> Output bitcode to <file>.")
  :: ("-memimpl", Arg.String (fun s -> (BatOption.may (fun m -> memimpl := m) (string_to_memimpl s))),
      "<mode> Use memory implementation mode.  Choose from Real, Func, or FuncMulti.")
  :: ("-exec", Arg.Set exec,
      "Execute the generated code. (DANGEROUS)")
  :: Input.speclist

let anon x =
  raise (Arg.Bad("Expected only one anonymous argument"))
let () = Arg.parse speclist anon usage

let prog,scope =
  try Input.get_program()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let cfg = Cfg_ast.of_prog prog;;
let cfg = Prune_unreachable.prune_unreachable_ast cfg;;

(* let prog = Memory2array.coerce_prog prog *)

(* let () = *)
(*   List.iter (fun s -> *)
(*     List.iter (fun s -> *)
(*       Printf.printf "%s\n" (Pp.ast_stmt_to_string s) *)
(*     ) (Flatten_mem.flatten_stores s) *)
(*   ) prog *)

let () =
  let codegen = new codegen !memimpl in
  let f = codegen#convert_cfg cfg in
  (* codegen#dump; *)
  (if !exec then
    let r = codegen#eval_fun f in
    Printf.printf "result: %s\n" (Pp.ast_exp_to_string r));

  (* Output bitcode *)
  (match !out with
  | Some o -> if not (codegen#output_bitcode o) then
      failwith "Unable to write bitcode to file"
  | None -> ())
