let temp_prefix = "T_"
let temp_prefix_len = String.length temp_prefix

let is_temp_name s =
  (* First try VEX style vars *)
  (String.length s > temp_prefix_len) && (String.sub s 0 2 = temp_prefix)

let is_temp (Var.V(_, s, _)) =
  is_temp_name s

let nt s t =
  if (is_temp_name s) then
    Var.newvar s t
  else
    let newname = temp_prefix^s in
    let () = assert (is_temp_name newname) in
    Var.newvar (newname) t
