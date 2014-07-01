open Format

let data = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ]
let table = 
  [|
    [| "aaaaaaaaaaaaaa"; "bbbbbb"; "ccccccccccc"; "ddddddddddd" |];
    [| "a"; "bbbb"; "c"; "d" |];
    [| "a"; "b"; "cccccc"; "d" |];
    [| "a"; "b"; "c"; "d" |];
  |]

let check_rectangularity a =
  let m = Array.length a in
  if m = 0 then (0, 0)
  else
    let n = Array.length a.(0) in
    try
      for i = 1 to m - 1 do
	if Array.length a.(i) <> n then 
	  raise Exit
      done;
      (m, n)
    with Exit -> invalid_arg "Easy_format.pp_print_table: not rectangular"

let pp_print_table fmt a =
  let m, n = check_rectangularity a in
  if m = 0 || n = 0 then ()
  else (
    pp_open_tbox fmt ();

    let a0 = a.(0) in
    for j = 0 to n - 1 do
      pp_set_tab fmt ();
      pp_print_string fmt a0.(j)
    done;
    pp_print_tbreak fmt 0 0;

    for i = 1 to m - 1 do
      let ai = a.(i) in
      for j = 0 to n - 1 do
	pp_print_string fmt ai.(j);
	pp_print_tbreak fmt 0 0
      done
    done;
    
    pp_close_tbox fmt ()
  )


let _ =
  let fmt = std_formatter in
  pp_print_table fmt table;
  pp_print_flush fmt ()


