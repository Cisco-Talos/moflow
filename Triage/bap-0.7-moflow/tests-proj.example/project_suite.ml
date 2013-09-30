open OUnit

(* Function in ocaml-proj *)
let sample_test = Test.testf

let tests = [
  "sample_test" >:: sample_test
];;

let bap_tests = ("Project" >::: tests);;

let summarize r =
  match r with 
  | RError(p,s) ->
	Format.printf "Error: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RFailure (p,s) ->
	Format.printf "Failure: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RSkip (p,s) -> 
	Format.printf "Skiped: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RTodo (p,s) -> 
	Format.printf "Todo: %s\n" ((string_of_path p) ^ "\n  " ^ s)
  | RSuccess p -> ();;

let rec summarize_results res =
  match res with
  | [] -> None
  | r::rs -> summarize r; summarize_results rs;;

let rec print_paths paths =
  match paths with
  | [] -> ()
  | p::ps -> Format.printf "%s\n" (string_of_path p); print_paths ps;;

let _ =
  let results = run_test_tt_main bap_tests in
  Format.printf "%s\n" "";
  summarize_results results
;;
