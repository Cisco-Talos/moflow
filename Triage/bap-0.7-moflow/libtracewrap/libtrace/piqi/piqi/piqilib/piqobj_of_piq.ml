(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


module C = Piqi_common
open C

open Piqobj_common


(*
(* "unknown field" warnings will not be printed for the fields from this list *)
let ignored_fields = ref []


let add_ignored_field x =
  ignored_fields := x :: !ignored_fields


let is_ignored_field (ast :piq_ast) =
  match ast with
    | `name x | `named {T.Named.name = x} -> (* field or flag *)
        List.mem x !ignored_fields
    | _ -> false


let load_piq_ignore_field = function
  | `word x ->
      add_ignored_field x
  | x ->
      error x "invalid .piq-ignore entry"


let load_piq_ignore_node (l :piq_ast list) =
  try
    let ignore_node = List.find
      (function
        | `named {T.Named.name = "piq-ignore"; T.Named.value = `list l} ->
            add_ignored_field "piq-ignore"; (* add piq-ignore itself *)
            List.iter load_piq_ignore_field l;
            true
        | (`named {T.Named.name = "piq-ignore"} as x) ->
            error x "invalid .piq-ignore specification"
        | _ -> false) l
    in ignore (ignore_node)
  with
    Not_found -> ()


let load_piq_ignore (ast : piq_ast) =
  ignored_fields := []; (* reset ignored fields *)
  match ast with
    | `list l -> load_piq_ignore_node l
    | _ -> ()
*)

let unknown_fields = ref []

let add_unknown_field x =
  unknown_fields := x :: !unknown_fields


let get_unknown_fields () =
  let res = List.rev !unknown_fields in
  (* reset unkown field list state *)
  unknown_fields := [];
  res


(* ------------------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)

let depth = ref 0

(* depth, description, object *)
exception Error of int * string * Obj.t


(* TODO: trace errors *)
let trace_error obj s =
  let loc = C.location obj in
  trace "piqobj_of_piq error: %s\n" (strerr loc s)


let error obj s =
  (*
  trace_error obj s;
  *)
  raise (Error (!depth, s, Obj.repr obj))


(* TODO, XXX: handle integer overflows *)
let rec parse_int (obj: piq_ast) =
  match obj with
    | `int x -> `int (Piqloc.addrefret obj x)
    | `uint x -> `uint (Piqloc.addrefret obj x)
    | `raw_word x ->
        let t =
          try Piq_parser.parse_int x
          with Failure e -> error obj e
        in parse_int t
    | o -> error o "int constant expected"


let uint64_to_float x =
  if Int64.compare x 0L < 0 (* big unsinged? *)
  then
    let s = Printf.sprintf "%Lu" x in
    float_of_string s
  else
    Int64.to_float x


let rec parse_float (obj: piq_ast) =
  match obj with
    | `int x -> Int64.to_float x
    | `uint x -> uint64_to_float x
    | `float x -> x
    | `raw_word x ->
        let t =
          try Piq_parser.parse_number x
          with Failure e -> error obj e
        in parse_float t
    | o -> error o "float constant expected"


let parse_bool (x :piq_ast) = match x with
  | `bool x -> x
  | `raw_word "true" -> true
  | `raw_word "false" -> false
  | o -> error o "boolean constant expected"


let parse_string ?piq_format (x :piq_ast) =
  let unicode_error s =
      error s "string contains non-unicode binary data"
  in
  let check_piq_format () =
    match piq_format, x with
      | Some `word, `word _
      | Some `word, `raw_word _ -> (); (* ok *)
      | Some `word, `text _ ->
          error x "word literal expected instead of verbatim text"
      | Some `word, _ (* various string literals *) ->
          warning x "word literal expected instead of quoted string"
      | None, `word _ ->
          error x "quoted string literal expected instead of word"
      | Some `text, `word _ ->
          error x "verbatim text or string literal expected instead of word"
      | _ -> ()
  in
  match x with
    | `ascii_string s | `utf8_string s | `text s | `word s ->
        check_piq_format ();
        s
    | `raw_binary s ->
        if Piq_lexer.is_utf8_string s
        then s
        else unicode_error s
    | `binary s -> unicode_error s
    | `raw_word s -> s
    | o -> error o "string expected"


let parse_binary (x :piq_ast) = match x with
  | `ascii_string s | `binary s | `raw_binary s -> s
  | `utf8_string s ->
      error s "binary contains unicode characters or code points"
  | `raw_word s -> s
  | o -> error o "binary expected"


(* some common errors *)
let error_exp_list obj =
  error obj "list expected"


let check_duplicate name tail =
  match tail with
    | [] -> ()
    | l ->
        if !Config.flag_strict
        then
          let obj = List.hd l in
          error obj ("duplicate field " ^ U.quote name)
        else
          List.iter (fun obj ->
            warning obj ("duplicate field " ^ U.quote name)) l


(* truncate the string till the first newline or to max_len *)
let truncate_string s max_len =
  let max_len =
    try String.index s '\n'
    with Not_found -> max_len
  in
  if String.length s <= max_len
  then s
  else
    let s = String.sub s 0 max_len in
    s ^ " ..."


let string_of_piqast x =
  match x with
    | `name s -> s
    | `named {Piq_ast.Named.name = n} -> n
    | _ ->
        let s = Piq_gen.to_string x in
        truncate_string s 50


let warn_unknown_field x =
  warning x ("unknown field: " ^ string_of_piqast x)


let handle_unknown_field (x: piq_ast) =
  if !Config.flag_strict
  then
    error x ("unknown field: " ^ string_of_piqast x)
  else
    if !C.is_inside_parse_piqi
    then add_unknown_field x
    else warn_unknown_field x


let handle_unknown_variant (x: piq_ast) =
  error x ("unknown variant: " ^ string_of_piqast x)


let find_piqtype name =
  try
    Piqi_db.find_piqtype name
  with Not_found ->
    Piqi_common.error name ("unknown type: " ^ U.quote name)


(* idtable implemented as map: string -> 'a *)
let rec parse_obj0
      ?(piq_format: T.piq_format option)
      ~try_mode
      (t: T.piqtype) (x: piq_ast) :Piqobj.obj =
  (* fill the location DB *)
  let r f x = reference f x in
  let rr f t x = reference (f t) x in
  match t with
    (* built-in types *)
    | `int -> parse_int x
    | `float -> `float (r parse_float x)
    | `bool -> `bool (r parse_bool x)
    | `string -> `string (reference (parse_string ?piq_format) x)
    | `binary -> `binary (r parse_binary x)
    | `any -> `any (r parse_any x)
    (* custom types *)
    | `record t -> `record (rr parse_record t x)
    | `variant t -> `variant (rr (parse_variant ~try_mode) t x)
    | `enum t -> `enum (rr (parse_enum ~try_mode) t x)
    | `list t -> `list (rr parse_list t x)
    | `alias t -> `alias (reference (parse_alias t ?piq_format) x)

and parse_obj ?(try_mode=false) ?piq_format t x =
  reference (parse_obj0 ~try_mode ?piq_format t) x


and parse_typed_obj ?piqtype x = 
  match piqtype, x with
    | None, `typed {Piq_ast.Typed.typename = n; value = ast} ->
        let t = find_piqtype n in
        parse_obj t ast
    | Some t, `typed {Piq_ast.Typed.value = ast} ->
        (* XXX: if both piqtype and `typed are defined, supplied type overrides
         * object type *)
        (* XXX: produce warning if they are not equal? *)
        parse_obj t ast
    | Some t, _ ->
        (* it is not a typed object, but we can use a supplied type *)
        parse_obj t x
    | _ -> error x "typed object expected"


and try_parse_obj f t x =
  (* unwind alias to obtain its real type *)
  match unalias t with
    | `record _ | `list _ ->
        (* NOTE: all records and lists should be labeled, so try-parsing them
         * always fails *)
        None
    | `any when f.T.Field#name <> None ->
        (* NOTE, XXX: try-parsing of labeled `any always failes *)
        None
    (* NOTE, XXX: try-parsing of unlabeled `any always succeeds *)
    | _ ->
        let depth' = !depth in
        try Some (parse_obj t x ~try_mode:true ?piq_format:f.T.Field#piq_format)
        with
          (* ignore errors which occur at the same parse depth, i.e. when
           * parsing everything except for lists and records which increment
           * depth *)
          Error (depth'', _, _) when depth'' = depth' ->
            (depth := depth'; None) (* restore the original depth *)


and parse_any x :Piqobj.any =
  (* NOTE: the object is not fully resolved during this stage; at least
   * "obj" should be obtained by parsing "piqtype.ast" at later stages (see
   * Piqi.resolve_defaults for example *)
  match x with
    | `any ref ->
        (* in internal mode, returning the exact Piqobj.any object passed via a
         * reference *)
        C.debug "Piqobj_of_piq.parse_any: recovering any from existing ref %d\n" ref;
        let any = Piqobj.get_any ref in
        (* prevent adding a location reference; if we attempt to add a location
         * referene here, we end up with a circular reference *)
        Piqloc.pause_once ();
        any

    | `typed {Piq_ast.Typed.typename = typename; value = ast} ->
        let any = Any#{
          Piqobj.default_any with
          typename = Some typename;
          piq_ast = Some ast;
        }
        in
        Piqloc.addrefret ast any

    (* read untyped JSON form (json ...) as piqi-any *)
    | `form (`word "json", [`text s]) ->
        let json_ast = !Piqobj.json_of_string s in
        let any = Any#{
          Piqobj.default_any with
          json_ast = Some json_ast;
        }
        in
        Piqloc.addrefret s any
    | `form (`word "json", _) ->
        error x "verbatim text literal with JSON value expected after \"json\""

    (* read untyped XML form (xml ...) as piqi-any *)
    | `form (`word "xml", [`text s]) ->
        let xml_list = !Piqobj.xml_of_string s in
        let any = Any#{
          Piqobj.default_any with
          xml_ast = Some ("undefined", xml_list);
        }
        in
        Piqloc.addrefret s any
    | `form (`word "xml", _) ->
        error x "verbatim text literal with XML value expected after \"xml\""

    | ast ->
        let any = Any#{
          Piqobj.default_any with
          piq_ast = Some ast;
        }
        in
        Piqloc.addrefret ast any


and parse_record t x =
  match x with
    | `list l ->
        incr depth;
        (* NOTE: pass locating information as a separate parameter since empty
         * list is unboxed and doesn't provide correct location information *)
        let loc = x in
        let res = do_parse_record loc t l in
        decr depth;
        res
    | o -> error_exp_list o
 (*
  * 1. parse required fields first by label, type or (sub)type = anonymous
  * 2. parse the rest in the order they are listed in the original specification
  * 
  *
  *)

and do_parse_record loc t l =
  let required_spec, other_spec =
    List.partition is_required_field t.T.Record#field in
  (* parse required fields first *)
  let fields, rem =
    List.fold_left (parse_field loc) ([], l) (required_spec @ other_spec) in
  (* issue warnings on unparsed fields *)
  List.iter handle_unknown_field rem;
  let unparsed_piq_fields_ref =
    if rem <> [] && !C.is_inside_parse_piqi
    then Some (Piqi_objstore.put rem) (* FIXME: potential memory leak *)
    else None
  in
  (* put required fields back at the top *)
  R#{t = t; field = List.rev fields; unparsed_piq_fields_ref = unparsed_piq_fields_ref}


and is_required_field t = (t.T.Field#mode = `required)


and parse_field loc (accu, rem) t =
  let fields, rem =
    match t.T.Field#piqtype with
      | None -> do_parse_flag t rem
      | Some _ -> do_parse_field loc t rem
  in
  (List.rev_append fields accu, rem)


and do_parse_flag t l =
  let open T.Field in
  let name = name_of_field t in
  debug "do_parse_flag: %s\n" name;
  let res, rem = find_flags name t.alt_name l in
  match res with
    | [] -> [], rem
    | x::tail ->
        check_duplicate name tail;
        let res = F#{ t = t; obj = None } in
        Piqloc.addref x res;
        [res], rem


and do_parse_field loc t l =
  let open T.Field in
  let name = name_of_field t in
  debug "do_parse_field: %s\n" name;
  let field_type = some_of t.piqtype in
  let values, rem =
    match t.mode with
      | `required -> 
          let x, rem = parse_required_field t loc name field_type l in
          [x], rem
      | `optional ->
          let x, rem = parse_optional_field t name field_type t.default l in
          let res = (match x with Some x -> [x] | None -> []) in
          res, rem
      | `repeated ->
          parse_repeated_field t name field_type l
  in
  let fields =
    List.map (fun x ->
      let res = F#{ t = t; obj = Some x } in
      Piqloc.addrefret x res) values
  in
  fields, rem
  

and parse_required_field f loc name field_type l =
  let res, rem = find_fields name f.T.Field#alt_name l in
  match res with
    | [] ->
        (* try finding the first field which is successfully parsed by
         * 'parse_obj' for a given field type *)
        begin
          let res, rem = find_first_parsed f field_type l in
          match res with
            | Some x -> x, rem
            | None -> error loc ("missing field " ^ U.quote name)
        end
    | x::tail ->
        check_duplicate name tail;
        let obj = parse_obj field_type x ?piq_format:f.T.Field#piq_format in
        obj, rem


and equals_name name alt_name x =
  if x = name
  then true
  else
    match alt_name with
      | Some name -> x = name
      | None -> false


(* TODO: find_fields and find_flags are mostly identical -- combine them
 * together and avoid code duplication *)

(* find field by name, return found fields and remaining fields *)
and find_fields (name:string) (alt_name:string option) (l:piq_ast list) :(piq_ast list * piq_ast list) =
  let equals_name = equals_name name alt_name in
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (`named n)::t when equals_name n.Piq_ast.Named#name -> aux (n.Piq_ast.Named#value::accu) rem t
    | (`name n)::t when equals_name n ->
        error n ("value must be specified for field " ^ U.quote n)
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


(* find flags by name, return found flags and remaining fields *)
and find_flags (name:string) (alt_name:string option) (l:piq_ast list) :(string list * piq_ast list) =
  let equals_name = equals_name name alt_name in
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (`name n)::t when equals_name n -> aux (n::accu) rem t
    | (`named n)::t when equals_name n.Piq_ast.Named#name ->
        error n ("value can not be specified for flag " ^ U.quote n.Piq_ast.Named#name)
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


and find_first_parsed f field_type l =
  let rec aux rem = function
    | [] -> None, l
    | h::t ->
        match try_parse_obj f field_type h with
          | None -> aux (h::rem) t
          | x -> x, (List.rev rem) @ t
  in aux [] l


and parse_optional_field f name field_type default l =
  let res, rem = find_fields name f.T.Field#alt_name l in
  match res with
    | [] ->
        (* try finding the first field which is successfully parsed by
         * 'parse_obj for a given field_type' *)
        begin
          let res, rem = find_first_parsed f field_type l in
          match res with
            | Some _ ->
                res, rem
            | None ->
                let res = Piqobj_common.parse_default field_type default in
                res, l
        end
    | x::tail ->
        check_duplicate name tail;
        let obj = Some (parse_obj field_type x ?piq_format:f.T.Field#piq_format) in
        obj, rem


(* parse repeated variant field allowing variant names if field name is
 * unspecified *) 
and parse_repeated_field f name field_type l =
  let res, rem = find_fields name f.T.Field#alt_name l in
  match res with
    | [] -> 
        (* XXX: ignore errors occuring when unknown element is present in the
         * list allowing other fields to find their members among the list of
         * elements *)
        let accu, rem =
          (List.fold_left
            (fun (accu, rem) x ->
              match try_parse_obj f field_type x with
                | None -> accu, x::rem
                | Some x -> x::accu, rem) ([], []) l)
        in List.rev accu, List.rev rem
    | l ->
        (* use strict parsing *)
        let res = List.map (parse_obj field_type ?piq_format:f.T.Field#piq_format) res in
        res, rem


and parse_variant ~try_mode t x =
  debug "parse_variant: %s\n" (some_of t.T.Variant#name);
  let value = parse_option t.T.Variant#option x ~try_mode in
  V#{ t = t; option = value }


and parse_option ~try_mode options x =
  try
    let value =
      match x with
        | `name n ->
            parse_name_option options n
        | `word _ ->
            parse_word_option options x
        | `raw_word s ->
            parse_raw_word_option options x s ~try_mode
        | `bool _ ->
            parse_bool_option options x
        | `int _ ->
            parse_int_option options x
        | `float _ ->
            parse_float_option options x
        | `ascii_string _ | `utf8_string _ | `binary _ | `raw_binary _ ->
            parse_string_option options x
        | `text _ ->
            parse_text_option options x
        | `named {Piq_ast.Named.name = n; value = x} ->
            parse_named_option options n x
        | `list _ ->
            parse_list_option options x
        | o -> error o "invalid option"
    in
    Piqloc.addrefret x value
  with Not_found ->
    (* recurse through included co-variants *)
    (* XXX: aliased variants are contra-variants? *)
    let get_covariants o =
      let open T.Option in
      match o.name, o.piqtype with
        | None, Some ((`variant _) as x) ->  (* co-variant *)
            [(o, x)]
        | None, Some ((`enum _) as x) -> (* co-variant *)
            [(o, x)]
        | _ -> (* contra-variant *)
            []
    in
    let covariants = U.flatmap get_covariants options in
    let value = parse_covariants covariants x ~try_mode in
    Piqloc.addrefret x value


and parse_covariants ~try_mode covariants x =
  let rec aux = function
    | [] ->
        (* failed to parse among variant and its covariants *)
        handle_unknown_variant x
    | h::t ->
        try
          parse_covariant h x ~try_mode
        with Not_found -> aux t
  in aux covariants


and parse_covariant ~try_mode (o, v) x =
  let obj =
    match v with
      | `variant v ->
          let value = reference (parse_variant ~try_mode v) x in
          `variant value
      | `enum e ->
          let value = reference (parse_enum ~try_mode e) x in
          `enum value
  in
  O#{ t = o; obj = Some obj }


and parse_name_option options name =
  let f o =
    let open T.Option in
    let equals_name x = equals_name x o.alt_name name in
    match o.name, o.piqtype with
      | Some n, Some _ when equals_name n ->
          error name ("value expected for option " ^ U.quote n)
      | Some n, None -> equals_name n
      | _, _ -> false
  in
  let option = List.find f options in
  O#{ t = option; obj = None }


and parse_named_option options name x =
  let f o =
    let open T.Option in
    let equals_name x = equals_name x o.alt_name name in
    match o.name, o.piqtype with
      | Some n, None when equals_name n ->
          error x ("value can not be specified for option " ^ n)
      | Some n, Some _ -> equals_name n
      | None, Some t when piqi_typename t = name -> true
      | _, _ -> false
  in parse_typed_option options f x


and make_option_finder f o =
  let open T.Option in
  match o.piqtype with
    | None -> false
    | Some x -> f (unalias x) (* TODO: optimize *)


and parse_bool_option options x =
  let f = make_option_finder ((=) `bool) in
  parse_typed_option options f x


and parse_int_option options x =
  let f = make_option_finder (function `int | `float -> true | _ -> false) in
  parse_typed_option options f x


and parse_float_option options x =
  let f = make_option_finder ((=) `float) in
  parse_typed_option options f x


and parse_word_option options x =
  let f = make_option_finder ((=) `string) in
  parse_typed_option options f x


and parse_raw_word_option ~try_mode options x s =
  let len = String.length s in
  let test_f = function
    (* all of these type can have values represented as a raw (unparsed) word *)
    | `string | `binary -> true
    | `bool when s = "true" || s = "false" -> true
    | `int when s.[0] >= '0' && s.[0] <= '9' -> true
    | `int when len > 1 && s.[0] = '-' && s.[1] >= '0' && s.[1] <= '9' -> true
    | `float -> true
    | _ -> false
  in
  let f = make_option_finder test_f in
  try
    parse_typed_option options f x
  with Not_found when not try_mode -> (* don't catch in try mode *)
    (* try to parse it as a name *)
    let f o =
      let open T.Option in
      match o.name, o.piqtype with
        | Some n, None -> equals_name n o.alt_name s
        | _, _ -> false
    in
    let option = List.find f options in
    O#{ t = option; obj = None }


and parse_string_option options x =
  let f = make_option_finder (function `string | `binary -> true | _ -> false) in
  parse_typed_option options f x


and parse_text_option options x =
  let f = make_option_finder (function `string -> true | _ -> false) in
  parse_typed_option options f x


and parse_list_option options x =
  let f = make_option_finder (function `record _ | `list _ -> true | _ -> false) in
  parse_typed_option options f x


and parse_typed_option (options:T.Option.t list) f (x:piq_ast) :Piqobj.Option.t =
  let option = List.find f options in
  let option_type = some_of option.T.Option#piqtype in
  let obj = Some (parse_obj option_type x ?piq_format:option.T.Option#piq_format) in
  O#{ t = option; obj = obj }


and parse_enum ~try_mode t x =
  debug "parse_enum: %s\n" (some_of t.T.Enum#name);
  let value = parse_option t.T.Enum#option x ~try_mode in
  E#{ t = t; option = value }


and parse_list t = function
  | `list l ->
      incr depth;
      let res = do_parse_list t l in
      decr depth;
      res
  | o -> error_exp_list o


and do_parse_list t l =
  let obj_type = some_of t.T.Piqi_list#piqtype in
  let contents = List.map (parse_obj obj_type ?piq_format:t.T.Piqi_list#piq_format) l in
  L#{ t = t; obj = contents }


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias ?(piq_format: T.piq_format option) t x =
  (* upper-level setting overrides lower-level setting *)
  let this_piq_format = t.T.Alias#piq_format in
  let piq_format =
    if this_piq_format <> None
    then this_piq_format
    else piq_format
  in
  let piqtype = some_of t.T.Alias#piqtype in
  let obj = parse_obj piqtype x ?piq_format in
  A#{ t = t; obj = obj }


(* 
 * External interface:
 *      resolve parse_obj errors into common parse error format
 *)
let wrap f x =
  depth := 0; (* reset the parser's depth *)
  (*
  load_piq_ignore x; (* load ignored fields from the toplevel list *)
  *)
  try f x
  with Error (_depth, s, obj) ->
    (* print delayed warnings in case of error *) 
    List.iter warn_unknown_field (get_unknown_fields ());
    Piqi_common.error obj s


let parse_obj t x =
  wrap (parse_obj t) x


let parse_typed_obj ?piqtype x =
  wrap (parse_typed_obj ?piqtype) x


let _ =
  Piqobj.of_piq := parse_obj

