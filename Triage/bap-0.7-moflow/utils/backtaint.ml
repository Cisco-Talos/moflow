let usage = "Usage: "^Sys.argv.(0)^" <input options>\n\
             Backwards taint analysis on traces"

let speclist = Input.trace_speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let prog,scope =
  try Input.get_program()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let input_location = Test_common.backwards_taint prog;;

(* Print out the input locations *)
Traces_backtaint.print_locset input_location;;
