open Trace_container
open Frame_piqi_ext

let copy_all r w =

  while not r#end_of_trace do
    w#add r#get_frame
  done

let () =
  if Array.length Sys.argv = 3 then
    let r = new reader Sys.argv.(1) in
    let w = new writer ~arch:r#get_arch ~machine:r#get_machine ~frames_per_toc_entry:(r#get_frames_per_toc_entry) Sys.argv.(2) in
    copy_all r w;
    w#finish
  else
    Printf.fprintf stderr "Usage: copytrace <source filename> <destination filename>\n"
;;
