
let read_file filename =
  let ch = open_in_bin filename in
  let len = in_channel_length ch in
  let buf = Buffer.create len in
  Buffer.add_channel buf ch len;
  close_in ch;
  Buffer.contents buf


let read_config filename =
  let contents = read_file filename in
  try
    Config_piqi_ext.parse_config contents `piq
  with
    Piqi_common.Error ((file, col, line), error) ->
      failwith (Printf.sprintf "error at %s:%d:%d: %s" file col line error)


(* test *)
let _ =
  let config = read_config "config.piq" in
  Config_piqi_ext.print_config config

