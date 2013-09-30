(* open BatString -- overrides compare for some reason! *)
open BatList
open Big_int_Z

let id = fun x -> x

let curry f = fun x y -> f(x,y)

let uncurry f = fun (x,y) -> f x y

let (<@) f g = (fun x -> f(g x))

let hd_tl = function
  | [] -> failwith "empty list"
  | x::xs -> x, xs

let rec foldn ?(t=0) f i n =  match n-t with
  | 0 -> f i n
  | _ when n>t -> foldn ~t f (f i n) (n-1)
  | -1 -> i
  | _ -> raise (Invalid_argument "negative index number in foldn")

let rec foldn64 ?(t=0L) f i n =
  let (-) = Int64.sub in
  match n-t with
  | 0L -> f i n
  | _ when n>t -> foldn64 ~t f (f i n) (n-1L)
  | n when n == -1L -> i (* otags has trouble with '-1L' *)
  | _ -> raise (Invalid_argument "negative index number in foldn64")

let mapn f n =
  List.rev (foldn (fun l i -> f(n-i)::l) [] n)
  (* List.rev is needed to make side effects happen in the same order *)

let rec list_mem ?(eq=(=)) ele = function
  | hd::tl ->
    if eq hd ele then true
    else list_mem ~eq ele tl
  | [] -> false

let list_argmax ?(compare=compare) f = function
  | [] -> raise (Invalid_argument "list_argmax")
  | hd::tl ->
    (List.fold_left
       (fun ((maxx,maxy) as max) x ->
         let y = f x in
         if compare y maxy > 0 then (x,y)
         else max) (hd, f hd) tl)

let list_union a b =
  List.fold_left (fun acc x ->
		    if List.mem x b then acc else x::acc) b a

let list_intersection a b =
  List.rev(List.fold_left (fun acc x ->
		    if List.mem x b then x::acc else acc) [] a)

let list_does_intersect a b =
  List.exists (fun x -> List.mem x a) b


let list_difference a b = 
    List.rev  (List.fold_left (fun acc x ->
			      if List.mem x b then
				acc
			      else
				x :: acc) [] a)

let list_subset a b =
  List.for_all (fun x -> List.mem x b) a

let list_set_eq a b = list_subset a b && list_subset b a

let list_unique l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun x -> Hashtbl.replace h x ()) l;
  Hashtbl.fold (fun k () ul -> k::ul) h []

let list_pop l =
  match !l with
  | x::xs -> l := xs; x
  | [] -> failwith "hd"

let list_push l v =
  l := v :: !l

(* Is this really any faster than List.hd . List.rev? *)
let rec list_last = function
  | [x] -> x
  | _::x -> list_last x
  | [] -> raise (Invalid_argument("list_last expects non-empty list"))

let list_last_option = function [] -> None | x -> Some(list_last x)

let list_insert l li n =
  let rec list_split hl tl n =
    if n = 0 then 
      (List.rev hl,tl) 
    else
      let hl' = (List.hd tl) :: hl in
      let tl' = List.tl tl in
      let n' = n-1 in
      list_split hl' tl' n'
  in
  let hd,tl = list_split [] l n in
  hd @ li @ tl

let list_remove l s r =
  let aftere = s + r in
  let _,revl = List.fold_left
    (fun (i,l) e ->
       if i >= s && i < aftere then
	 (i+1,l)
       else
	 (i+1, e::l)
    ) (0,[]) l
  in
  List.rev revl

let list_delete l e = 
  let rec delete_aux acc = function
    | [] -> List.rev acc
    | x::xs when e == x -> (List.rev acc)@xs
    | x::xs -> delete_aux (x::acc) xs
  in
    delete_aux [] l

let list_compare = BatList.make_compare

let list_cart_prod2 f l1 l2 =
  List.iter
    (fun x ->
       List.iter (f x) l2
    ) l1

let list_cart_prod3 f l1 l2 l3 =
  List.iter (fun x -> list_cart_prod2 (f x) l2 l3) l1

let list_cart_prod4 f l1 l2 l3 l4 =
  List.iter (fun x -> list_cart_prod3 (f x) l2 l3 l4) l1

let list_permutation setlist f =
  let newf =
    List.fold_left
      (fun (acc : 'a list -> unit) (set : 'a list) ->
         (fun l -> List.iter (fun o -> acc (o::l)) set)
      ) (fun l -> f l) setlist in
  newf []

(** Calls f on each element of l, and returns the first Some(x)
    returned.  If no Some(x) are returned, None is returned. *)
let list_find_option f l =
  (* Maybe list_find_some should call this instead *)
  try Some(BatList.find_map f l) with Not_found -> None

(* let list_for_allsome f l = *)
(*   let rec m r = function *)
(*     | [] -> Some(List.rev r) *)
(*     | x::xs -> match f x with *)
(*       | None -> None *)
(*       | Some x -> m (x::r) xs *)
(*   in *)
(*   m [] l *)

(* let list_find_some x = BatOption.get (List.find (function | Some _ -> true | None -> false) x) *)


(* let list_map_some f = *)
(*   let rec help res = function *)
(*     | [] -> List.rev res *)
(*     | x::xs -> help (match f x with Some i -> (i::res) | None -> res) xs *)
(*   in *)
(*     help [] *)

(** [list_count f l] counts the number of items in [l] for which the
    predicate [f] is true. *)
(* let list_count f = *)
(*   List.fold_left (fun c x -> if f x then c+1 else c) 0 *)



  

(* let list_join = BatList.reduce *)
(* (\* let list_join f = *\) *)
(* (\*   function *\) *)
(* (\*     | x::(_::_ as xs) -> List.fold_left f x xs *\) *)
(* (\*     | [x] -> x *\) *)
(* (\*     | [] -> raise(Invalid_argument "list_join on empty list") *\) *)

(* list_firstindex l pred returns the index of the first list element
   that pred returns true on *)
(* let rec list_firstindex ?s:(s=0) l pred = *)
(*   match l with *)
(*   | [] -> raise Not_found *)
(*   | x::_ when pred x -> s *)
(*   | _::tl -> list_firstindex tl pred ~s:(s+1) *)

let list_partition_last lst = 
  match List.rev lst with
  | [x] -> ([],x)
  | x::ys -> (List.rev ys,x)
  | _ -> raise (Invalid_argument "list_partition_last expects non-empty list")

let list_shortest_first f a b =
  let (la, lb) = (List.length a, List.length b) in
  if lb < la then f lb la else f la lb


let change_ext filename new_ext =
  let last_dot_pos = String.rindex filename '.' in
  let base = String.sub filename 0 last_dot_pos in
  String.concat "." [base; new_ext]

let list_directory ?(sort_files=true) dir_path =
  let file_type = Unix.stat dir_path in
  if (file_type.Unix.st_kind <> Unix.S_DIR) 
    then raise (Invalid_argument "Not a directory.");
  let file_array =
    try Sys.readdir dir_path 
    with _ -> raise (Invalid_argument "Could not read dir.")
  in
  if (sort_files) then Array.sort (Pervasives.compare) file_array;
  Array.to_list file_array

let trim_newline s = 
  if String.length s > 0 && String.get s ((String.length s) -1) = '\n'
  then	String.sub s 0 ((String.length s)-2)
  else	s

let union_find map items =
  let add_one res item =
    let set = map item in
    let (joined,indep) =
      List.partition (fun (s,is) -> list_does_intersect s set) res
    in
    let joined =
      List.fold_left
	(fun (s,is) (s2,is2) -> (list_union s2 s, List.rev_append is2 is))
	(set,[item]) joined
    in
    joined::indep
  in
  let res = List.fold_left add_one [] items in
  List.map snd res

let rec split_common_prefix ?(eq=(=)) la lb = 
  let rec split_common_prefix_h acc la lb =
    match la,lb with
    | [], _ -> (List.rev acc, la, lb)
    | _, [] -> (List.rev acc, la, lb)
    | h1::t1, h2::t2 ->
      if eq h1 h2 then
	split_common_prefix_h (h1::acc) t1 t2
      else (List.rev acc, la, lb)
  in
  split_common_prefix_h [] la lb

let split_common_suffix ?(eq=(=)) la lb =
  let (s,rla,rlb) = split_common_prefix ~eq (List.rev la) (List.rev lb) in
  (List.rev s, List.rev rla, List.rev rlb)

let apply_option f k =
  match f with
  | None -> k
  | Some(f') -> f' k

let memoize ?(size = 128) f =
  let results = Hashtbl.create size in
  fun x ->
    try Hashtbl.find results x
    with Not_found ->
      let y = f x in
      Hashtbl.add results x y;
      y

(* (\** Given Some(a), returns a. Given None, raises Not_found *\) *)
(* let option_unwrap o = *)
(*   match o with *)
(*   | Some(x) -> x *)
(*   | None -> raise Not_found *)

(* (\** Maps an ['a option] to a ['b option], given a function [f : 'a -> 'b] *\) *)
(* let option_map f = function *)
(*   | None -> None *)
(*   | Some x -> Some(f x) *)


let get_hash_keys ?(sort_keys=false) htbl =
  let l = Hashtbl.fold (fun key data prev -> key::prev) htbl [] in
  if (sort_keys) then List.sort (Pervasives.compare) l
  else l

let get_hash_values ?(sort_values=false) htbl =
  let l = Hashtbl.fold (fun key data prev -> data::prev) htbl [] in
  if (sort_values) then List.sort (Pervasives.compare) l
  else l

module HashUtil (H:Hashtbl.S) =
struct

  let hashtbl_eq ?(eq=(=)) h1 h2 =
    let subtbl h1 h2 =
      H.fold
	(fun k v r ->
	   try r && eq v (H.find h2 k)
	   with Not_found -> false )
	h1 true
    in
      subtbl h1 h2 && subtbl h2 h1

  (* Work around buggy replace in older versions of ocaml *)
  let hashtbl_replace table x y =
    H.remove table x;
    H.add table x y

  let get_hash_keys ?(sort_keys=false) htbl =
    let l = H.fold (fun key data prev -> key::prev) htbl [] in
    if (sort_keys) then List.sort (Pervasives.compare) l
    else l

  let get_hash_values ?(sort_values=false) htbl =
    let l = H.fold (fun key data prev -> data::prev) htbl [] in
    if (sort_values) then List.sort (Pervasives.compare) l
    else l
end

(* GRR, Hashtbl doesn't ascribe to the Hashtbl.S signature *)
let hashtbl_eq ?(eq=(=)) h1 h2 =
  let subtbl h1 h2 =
    Hashtbl.fold
      (fun k v r ->
	 try r && eq v (Hashtbl.find h2 k)
	 with Not_found -> false )
      h1 true
  in
    subtbl h1 h2 && subtbl h2 h1


(* module StringSet = Set.Make(String) ;; *)

(* stuff that should be in Int64 *)


let int64_ucompare x y =
  if x < 0L && y >= 0L then 1
  else if x >= 0L && y < 0L then -1
  else Int64.compare x y

let int64_udiv x y =
  (* Reference: Hacker's Delight (Warren, 2002) Section 9.3 *)
  if y < 0L
  then if int64_ucompare x y < 0 then 0L else 1L
  else if x < 0L
  then let all_but_last_bit =
    Int64.shift_left (Int64.div (Int64.shift_right_logical x 1) y) 1
  in
    if int64_ucompare (Int64.sub x (Int64.mul all_but_last_bit y)) y >= 0 then
      Int64.succ all_but_last_bit
    else
      all_but_last_bit
  else Int64.div x y

let int64_urem x y =
  Int64.sub x (Int64.mul y (int64_udiv x y))

let int64_umax x y =
  if int64_ucompare x y > 0 then x else y

let int64_umin x y =
  if int64_ucompare x y < 0 then x else y

(** Convert integer to binary represented as a string *)
let int64_to_binary ?pad n = 
  let getb n = Int64.logand n 1L in (* Get lsb *)
  let getrest n = Int64.shift_right_logical n 1 in (* Get all but lsb *)
  let zeroextend s = match pad with
    | None -> s
    | Some(l) -> 
	let p = l - String.length s in
	assert (p >= 0);
	(String.make p '0') ^ s 
  in
  let rec f = function
    | 0L -> "0"
    | 1L -> "1"
    | n -> (f (getrest n)) ^ (f (getb n))
  in
  zeroextend (f n)

(* end stuff that should be in Int64 *)

let big_int_to_binary ?pad n = 
  let getb n = Big_int_Z.and_big_int n (big_int_of_int 1) in (* Get lsb *)
  let getrest n = Big_int_Z.shift_right_big_int n 1 in (* Get all but lsb *)
  let zeroextend s = match pad with
    | None -> s
    | Some(l) -> 
	let p = l - String.length s in
	assert (p >= 0);
	(String.make p '0') ^ s 
  in
  let rec f = function
    | bi when (eq_big_int bi zero_big_int) -> "0"
    | bi when (eq_big_int bi unit_big_int) -> "1"
    | n -> (f (getrest n)) ^ (f (getb n))
  in
  zeroextend (f n)

(*
    XXX: We could make this more efficient by operating one int64 at a
    time, instead of just a nibble.
*)
let big_int_to_hex ?pad n = 
  if n < Big_int_Z.zero_big_int then
    failwith "big_int_to_hex: Cannot convert infinite-width negative number to hex";
  let getn n = Big_int_Z.and_big_int n (big_int_of_int 0xf) in (* Get lsnibble *)
  let getrest n = Big_int_Z.shift_right_big_int n 4 in (* Get all but lsnibble *)
  let zeroextend s = match pad with
    | None -> s
    | Some(l) -> 
	let p = l - String.length s in
	assert (p >= 0);
	(String.make p '0') ^ s 
  in
  let (<=%) = le_big_int in
  let rec f = function
    | bi when bi <=% (big_int_of_int 0xf) -> Printf.sprintf "%x" (Big_int_Z.int_of_big_int bi)
    | n -> (f (getrest n)) ^ (f (getn n))
  in
  zeroextend (f n)

let big_int_of_string s =
  (* Awesome, apparently Zarith handles 0x and 0b for us! *)
  (* let hex_prefix = "0x" in *)
  (* let is_hex s = *)
  (*   let re = Str.regexp ("^"^hex_prefix) in *)
  (*   Str.string_match re s 0 *)
  (* in *)
  (* let hex_to_bitlen s = *)
  (*   (String.length s) * 4 *)
  (* in *)
  (* let bitlen_to_hex n = *)
  (*   (\* Round up *\) *)
  (*   (n+3) / 4 *)
  (* in *)
  (* (\* If the highest bit is 1, Int64.of_string will return a negative *)
  (*    value. So, we use 60 bits instead of 64 to avoid messing with *)
  (*    int64's sign bit. *\) *)
  (* let numbits = 60 in *)
  (* let getmost s = String.sub s 0 (bitlen_to_hex numbits) in *)
  (* let getrest s = *)
  (*   let start = bitlen_to_hex numbits in *)
  (*   let last = String.length s in *)
  (*   String.sub s start (last - start) *)
  (* in *)
  (* (\* Get rid of 0x prefix, if any *\) *)
  (* let rec f s = *)
  (*   let len = hex_to_bitlen s in *)
  (*   if len <= numbits then *)
  (*     let bi = Big_int_Z.big_int_of_int64 (Int64.of_string ("0x"^s)) in *)
  (*     let (>=%) = ge_big_int in *)
  (*     assert (bi >=% zero_big_int); *)
  (*     bi *)
  (*   else ( *)
  (*     (\* Printf.printf "getmost: %s v: %s\n" (getmost s) (Big_int_Z.string_of_big_int (f (getmost s))); *\) *)
  (*     (\* Printf.printf "getrest: %s v: %s\n" (getrest s) (Big_int_Z.string_of_big_int (f (getrest s))); *\) *)
  (*     let (|%) = or_big_int in *)
  (*     let (<<%) = shift_left_big_int in *)
  (*     let bi = (f (getmost s) <<% (hex_to_bitlen (getrest s))) |% (f (getrest s)) in *)
  (*     let (>=%) = ge_big_int in *)
  (*     assert (bi >=% zero_big_int); *)
  (*     bi *)
  (*   ) *)
  (* in *)
  (* if is_hex s then *)
  (*   f (BatString.slice ~first:(String.length hex_prefix) s) *)
  (* else *)
  (*   (\* big_int_of_string handles decimals *\) *)
  Big_int_Z.big_int_of_string s

let big_int_of_binstring ?(e = `Little) s =
  let s = BatString.explode s in
  let s = if e = `Little then List.rev s else s in
  let s = "0x" ^ (BatString.implode (List.flatten (List.map (fun c -> BatString.explode (Printf.sprintf "%02x" (Char.code c))) s))) in
  big_int_of_string s

let run_with_remapped_fd fd_from fd_to f =
  (* remap *)
  let fd_to_saved = Unix.dup fd_to in
  Unix.dup2 fd_from fd_to;

  (* execute *)
  let rv = f () in

  (* restore *)
  Unix.dup2 fd_to_saved fd_to;
  Unix.close fd_to_saved;

  rv

(* let take = BatList.take *)
(* let fast_append = append *)


(* let has_some o = o <> None *)


(** Some -> true, None -> false *)
(* let has_some x = x <> None *)



(* (\* Deal with system calls (stolen from  *)
(*    http://rosettacode.org/wiki/Execute_a_system_command#OCaml ) *\) *)
(* let check_exit_status =  *)
(*   let warn = "warning: the process was" in *)
(*   function *)
(*   | Unix.WEXITED 0 -> () *)
(*   | Unix.WEXITED r ->  *)
(*     Printf.eprintf "%s terminated with exit code (%d)\n%!" warn r *)
(*   | Unix.WSIGNALED n -> *)
(*     Printf.eprintf "%s killed by a signal (number: %d)\n%!" warn n *)
(*   | Unix.WSTOPPED n ->  *)
(*     Printf.eprintf "%s stopped by a signal (number: %d)\n%!" warn n *)
(* ;; *)

module StatusPrinter =
struct
  module D = Debug.Make(struct let name = "UtilStatus" and default=`Debug end)
  open D


  let updatetime = 5.0 (* update speed estimate every updatetime seconds *)
  let total = ref 0
  let current = ref 0
  let percentage = ref 0
  let last = ref 0
  let lasttime = ref 0.0
  let message = ref "Status"
  let starttime = ref 0.0

  let cpercent () =
    try
      (!current * 100 / !total)
    with Division_by_zero -> 0

  let rate () =
    let deltat = Unix.gettimeofday () -. !lasttime in
    let deltay = !current - !last in
      if deltat == 0.0 || deltay == 0 then
	-1.0
	  else
	(float_of_int deltay) /. deltat

  let update () =
    let p = cpercent () in
    if p = -1 then (
      if (debug()) then Printf.printf "%s...\r" !message)
    else (
      if (debug()) then Printf.printf "%s: %d%% (%f eps)\r" !message p (rate ()) ;

      percentage := p;
      last := !current;
      lasttime := Unix.gettimeofday();
      flush stdout)

  let init msg size = 
    last := 0 ;
    current := 0 ;
    percentage := -1 ;
    message := msg ;
    total := size ;
    starttime := Unix.gettimeofday () ;
    lasttime := !starttime ;
    update ()
      
  let inc () =
    if !total != 0 then (
      current := !current + 1 ;
      let percentage' = cpercent() in
	if ((percentage' != !percentage) 
	    (*|| ((Unix.gettimeofday() -. !lasttime) >= updatetime)*) ) then
	  (update ()))
	  
  let stop () =
    if (debug()) then 
	  Printf.printf "%s: Done! (%f seconds)\n" !message 
		(Unix.gettimeofday () -. !starttime) ;
    flush stdout
end

let rec print_separated_list ps sep lst = 
  let rec doit acc = function
    | [] -> acc^""
    | x::[] -> acc^(ps x)
    | x::y::zs -> let acc = (ps x)^sep in
	(doit acc (y::zs))
  in
    doit "" lst

let print_obj_info title value =
  let module D = Debug.Make(struct let name = "UtilSize" and default=`NoDebug end) in
  if D.debug() then
    let i = Objsize.objsize value in
    D.dprintf "%S : data_words=%i headers=%i depth=%i\n    \ 
      bytes_without_headers=%i bytes_with_headers=%i"
      title i.Objsize.data i.Objsize.headers i.Objsize.depth
      (Objsize.size_without_headers i)
      (Objsize.size_with_headers i);
    D.dprintf "%S : total size in MB = %i" title ((Objsize.size_with_headers i) / 1048576)

let syscall ?(env=[| |]) cmd =
  let check_exit_status =
    let warn = "warning: the process was" in
    function
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED r ->
        Printf.eprintf "%s terminated with exit code (%d)\n%!" warn r
      | Unix.WSIGNALED n ->
        Printf.eprintf "%s killed by a signal (number: %d)\n%!" warn n
      | Unix.WSTOPPED n ->
        Printf.eprintf "%s stopped by a signal (number: %d)\n%!" warn n
  in
  let ic, oc, ec = Unix.open_process_full cmd env in
  let buf1 = Buffer.create 96
  and buf2 = Buffer.create 48 in
  (try
     while true do Buffer.add_channel buf1 ic 1 done
   with End_of_file -> ());
  (try
     while true do Buffer.add_channel buf2 ec 1 done
   with End_of_file -> ());
  let exit_status = Unix.close_process_full (ic, oc, ec) in
  check_exit_status exit_status;
  (Buffer.contents buf1,
   Buffer.contents buf2)

let print_mem_usage _ =
  let module D = 
	Debug.Make(struct let name = "UtilMemUse" and default=`NoDebug end) 
  in
  if D.debug() then
    let pid = Unix.getpid() in
    let cmd = 
      "ps auxw | grep \'^[a-zA-Z]\\{1,\\}[[:space:]]\\{1,\\}"^
        (string_of_int pid)^"\'"
    in
    let (out1,out2) = syscall cmd in
    D.pdebug (out1^out2)

