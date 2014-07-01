module L = List
module Ux = Unix
module Sys = Sys
(* BAP *)
module D = Debug.Make(struct let name = "Egas" and default = `NoDebug end)

let keep_some l = 
  let l = L.filter BatOption.is_some l in
  let l = L.map BatOption.get l in
  l

let file_size fn =
  let s = Unix.stat fn in
  s.Unix.st_size

let copy_file src dst = 
  try
    let ic = BatFile.open_in src in
    let oc = BatFile.open_out dst in
    BatIO.copy ic oc;
    BatIO.close_in ic;
    BatIO.close_out oc
  with _ ->
    failwith "copy_file: exception"

(* `create, `trunc *)
let open_out fn = 
  try
    BatFile.open_out fn
  with _ ->
    failwith "open_out: exception"

let write_string fn str = 
  try
    let oc = open_out fn in
    BatIO.nwrite oc str;
    BatIO.flush oc;
    BatIO.close_out oc
  with _ ->
    failwith "write_string: exception"

let read_string fn = 
  try
    let size = BatFile.size_of fn in
    let ic = BatFile.open_in fn in
    let str = BatIO.really_nread ic size in
    BatIO.close_in ic;
    size, str
  with _ ->
    failwith "read_string: exception"

let read_all_lines file_name =
  let in_channel = open_in file_name in
  let rec read_recursive lines =
    try
      Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines))
    with End_of_file ->
      lines 
  in
  let lines = read_recursive [] in
  let _ = close_in_noerr in_channel in
  List.rev (lines);;

let arr_to_str arr = 
  let l = Array.to_list arr in
  String.concat " " l

let str_to_arr s = 
  let l = Str.split (Str.regexp "[ \t]+")  s in
  Array.of_list l

(* BAP STUFF *)

(* read il *)
let read_il_from_file f = fst (Parser.program_from_file f )

let read_serialized_il_from_file f = 
  Asmir.serialized_bap_from_trace_file f

(* Grab 1 frame at a time. *)
let stream_serialized_il_from_file trace_fn = 
  let close, stream = Asmir.serialized_bap_stream_from_trace_file 1L trace_fn in
  close, stream

(* pretty print il *)
let pp_ast f p =
  let oc = Pervasives.open_out f in
  let pp = new Pp.pp_oc oc in
  pp#ast_program p;
  pp#close

let ast_attribs_to_string = Pp.pp2string (fun p -> p#attrs)
