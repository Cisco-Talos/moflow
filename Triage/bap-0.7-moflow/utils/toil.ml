
let usage = "Usage: "^Sys.argv.(0)^" <input options> [-o output]\n\
             Translate programs to the IL. "

let output_ast_il oc p =
  let pp = new Pp.pp_oc oc in
  pp#ast_program p;
  pp#close

let output_ast_pb oc p =
  let () = set_binary_mode_out oc true in
  output_string oc (Ast_piqi.to_pb p);
  close_out oc;
  set_binary_mode_out oc false

let output_ast_json oc p =
  output_string oc (Ast_piqi.to_json p);
  close_out oc

let output_ast_xml oc p =
  output_string oc (Ast_piqi.to_xml p);
  close_out oc

let outf = ref output_ast_il
let out = ref stdout
let speclist =
  ("-o", Arg.String (fun f -> out := open_out f),
   "<file> Print output to <file> rather than stdout.")
  :: ("-toil", Arg.Unit (fun () -> outf := output_ast_il),
      "Output to text format (default).")
  :: ("-topb", Arg.Unit (fun () -> outf := output_ast_pb),
      "Output to protobuf format.")
  :: ("-tojson", Arg.Unit (fun () -> outf := output_ast_json),
      "Output to json format.")
  :: ("-toxml", Arg.Unit (fun () -> outf := output_ast_xml),
      "Output to xml format.")
  :: Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage


let prog =
  try fst (Input.get_program())
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1
;;

!outf !out prog
;;
