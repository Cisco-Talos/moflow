open Trace_container
open Frame_piqi_ext

let print f =
  print_frame f

let print_all f =

  let r = new reader f in

  while not r#end_of_trace do
    print r#get_frame
  done;;

let () =
  if Array.length Sys.argv = 2 then
    print_all Sys.argv.(1)
  else
    Printf.fprintf stderr "Usage: readtrace <trace>\n"
;;

