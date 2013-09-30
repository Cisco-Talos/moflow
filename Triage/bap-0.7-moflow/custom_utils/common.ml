module D = Debug.Make(struct let name = "Egas" and default = `Debug end)
open D

let oFF_PATH = "off_path"

let file_exc filename e = 
    Format.printf "Cannot open file \"%s\": %s\n" filename (Printexc.to_string e);
    assert false

let open_file fn fopen = 
    try
        let co = fopen fn in
        co
    with Sys_error _ as e ->
        file_exc fn e

let open_file_in fn = open_file fn open_in_bin
let open_file_out fn = open_file fn open_out_bin

let write_file filename str = 
    let co = open_file_out filename in
    output_string co str;
    close_out co

let read_file fn =
    let ic = open_file_in fn in
    let n = in_channel_length ic in
    let s = String.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)

let file_size fn =
    let s = Unix.stat fn in
    s.Unix.st_size

let run_cmd cmd =
    let _ = assert (Sys.os_type = "Unix") in
    let err = Unix.system cmd in
    match err with
    | Unix.WEXITED n -> n
    | _ -> failwith "run_cmd: unexpected status"

let copy_file src dst =
    let cmd = Printf.sprintf "cp %s %s" src dst in
    let e = run_cmd cmd in
    match e with 
    | 0 -> ()
    | _ -> failwith "copying failed"

let match_and_cut l1 l2 =
    let rec aux last_match l1 l2 =
        match l1, l2 with
        | hd1::tl1, hd2::tl2 ->
            if hd1=hd2 then
                aux (Some(hd2)) tl1 tl2
            else
                last_match, tl2
        | _, _ -> last_match, l2
    in
    let ll1, ll2 = List.length l1, List.length l2 in
    let _ = dprintf "match_and_cut: |l1|=%d, |l2|=%d\n" ll1 ll2 in
    aux None l1 l2

let split n l =
    let rec aux acc n l =
        if n=0 then (List.rev acc, l)
        else
        match l with
        | hd::tl -> aux (hd::acc) (n-1) tl
        | _ -> failwith "too long prefix requested"
    in
    aux [] n l 

let cut_prefix n l = snd (split n l)

let select_some l =
    let rec aux acc l =
        match l with
        | Some(x)::tl-> aux (x::acc) tl
        | None::tl -> aux acc tl
        | [] -> List.rev acc
    in
    aux [] l

(* s1 contains s2 ? *)
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

(* BAP STUFF *)

(* read il *)
let read_il_from_file f = fst (Parser.program_from_file f )

(* pretty print il *)
let pp_ast f p =
  let oc = open_out f in
  let pp = new Pp.pp_oc oc in
  pp#ast_program p;
  pp#close

let ast_attribs_to_string = Pp.pp2string (fun p -> p#attrs)
