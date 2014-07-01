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


(*
 * Typefull parser generator for decoding piq data from wire (Protocol
 * Buffers wire) format.
 *)

open Piqi_common
open Iolist


(* reuse several functions *)
open Piqic_erlang_types
open Piqic_erlang_out


let gen_erlang_type_name t ot =
  match gen_piqtype t ot with
    | "string" ->
        (match !string_type with
          | `binary -> "binary_string"
          | `list -> "list_string"
        )
    | x -> x


let rec gen_parse_type erlang_type wire_type wire_packed x =
  let packed = ios (if wire_packed then "packed_" else "") in
  match x with
    | `any ->
        if !Piqic_common.is_self_spec
        then ios "parse_" ^^ ios !any_erlname
        else ios "piqi_piqi:parse_piqi_any"
    | `alias a when wire_type <> None ->
        (* need special handing for wire_type override *)
        gen_parse_type a.A#erlang_type wire_type wire_packed (some_of a.A#piqtype)
    | (#T.typedef as x) ->
        let modname = gen_parent x in
        iol [
          modname; packed; ios "parse_"; ios (typedef_erlname x)
        ]
    | _ -> (* gen parsers for built-in types *)
        iol [
            ios "piqirun:";
            ios (gen_erlang_type_name x erlang_type);
            ios "_of_"; packed;
            ios (W.get_wire_type_name x wire_type);
        ]

let gen_parse_piqtype ?erlang_type ?wire_type ?(wire_packed=false) (t :T.piqtype) =
  gen_parse_type erlang_type wire_type wire_packed t


let gen_erlang_binary x =
  let codes =
    List.map (fun x ->
      ios (string_of_int (Char.code x))) (U.list_of_string x)
  in
  iol [ ios "<<"; iod "," codes; ios ">>" ]


(* XXX: parse defaults once at boot time rather than each time when we need to
 * parse a field *)
let gen_default = function
  | None -> iol []
  | Some piqi_any ->
      let pb = Piqobj.pb_of_piqi_any piqi_any in
      iol [
        ios ", "; (* separate Default from the previous parameter *)
        gen_erlang_binary pb;
      ]


let esc x = ios "_" ^^ ios (String.capitalize x)


let rest i =
  ios "R" ^^ ios (string_of_int !i)


let gen_field_cons f =
  let open Field in
  let fname = erlname_of_field f in
  (* field construction code *)
  iol [ ios fname; ios " = "; esc fname; ]


let gen_field_parser i f =
  let open Field in
  let fname = erlname_of_field f in
  let mode = gen_mode f in
  let fcons =
    match f.piqtype with
      | Some piqtype ->
          (* field constructor *)
          iol [
            (* "parse_(req|opt|rep)_field" function invocation *)
            ios "piqirun:parse_" ^^ ios mode ^^ ios "_field(";
              gen_code f.code; ios ", ";
              ios "fun ";
                gen_parse_piqtype piqtype ~wire_packed:f.protobuf_packed;
                ios "/1, ";

              (* when parsing packed repeated fields, we should also accept
               * fields in unpacked representation; therefore, specifying an
               * unpacked field parser as another parameter *)
              if f.protobuf_packed
              then iol [
              ios "fun ";
                gen_parse_piqtype piqtype;
                ios "/1, ";
              ] else iol [];

              rest i;
              gen_default f.default;
            ios ")";
          ]
      | None ->
          (* flag constructor *)
          iol [ 
            ios "piqirun:parse_flag(";
              gen_code f.code; ios ", ";
              rest i;
            ios ")";
          ]
    in
  incr i;
  (* field parsing code *)
  iol [ ios "{"; esc fname; ios ", "; rest i; ios "} = "; fcons; ]


let gen_record r =
  let name = some_of r.R#erlang_name in
  (* NOTE: fields are already ordered by their codes when Piqi is loaded *)
  let fields = r.R#wire_field in
  let fconsl = (* field constructor list *)
    List.map gen_field_cons fields
  in
  let i = ref 0 in
  let fparserl = (* field parsers list *)
    List.map (gen_field_parser i) fields
  in
  let parsers_code =
    match fparserl with
      | [] -> iol []
      | _ -> iol [ iod ",\n    " fparserl; ios ","; eol; ]
  in
  iol [
    ios "parse_"; ios name; ios "(X) -> "; indent;
      ios "R0 = piqirun:parse_record(X),"; eol;
      parsers_code;
      ios "piqirun:check_unparsed_fields("; rest i; ios "),"; eol;
      ios "#"; ios (scoped_name name); ios "{"; indent;
      iod ",\n        " fconsl;
      unindent; eol;
      ios "}.";
      unindent; eol;
  ]


let gen_const c =
  let open Option in
  let code_str = gen_code c.code in
  iol [
    code_str; ios " -> "; ios (some_of c.erlang_name);
  ]


let gen_enum e =
  let open Enum in
  let consts = List.map gen_const e.option in
  let cases = consts @ [ ios "Y -> piqirun:error_enum_const(Y)" ] in
  iol [
    ios "parse_"; ios (some_of e.erlang_name); ios "(X) ->"; indent;
    ios "case piqirun:integer_of_signed_varint(X) of"; indent;
      iod ";\n        " cases;
      unindent; eol;
      ios "end.";
    unindent; eol;
  ]


let gen_packed_enum e =
  let open Enum in
  let consts = List.map gen_const e.option in
  let cases = consts @ [ ios "Y -> piqirun:error_enum_const(Y)" ] in
  iol [
    ios "packed_parse_"; ios (some_of e.erlang_name); ios "(X) ->"; indent;
    ios "{Code, Rest} = piqirun:integer_of_packed_signed_varint(X),"; eol;
    ios "{case Code of"; indent;
      iod ";\n        " cases;
      unindent; eol;
      ios "end, Rest}.";
    unindent; eol;
  ]


let gen_enum e =
  (* generate two functions: one for parsing normal value; another one -- for
   * packed value *)
  iod "\n\n" [
    gen_enum e;
    gen_packed_enum e;
  ]


let rec gen_option o =
  let open Option in
  match o.erlang_name, o.piqtype with
    | Some ename, None -> (* expecting boolean true for a flag *)
        iol [
          gen_code o.code; ios " when Obj == 1 -> "; ios ename;
        ]
    | None, Some ((`variant _) as t) | None, Some ((`enum _) as t) ->
        iol [
          gen_code o.code; ios " -> ";
            gen_parse_piqtype t; ios "(Obj)";
        ]
    | _, Some t ->
        let ename = erlname_of_option o in
        iol [
          gen_code o.code; ios " -> ";
            ios "{"; ios ename; ios ", ";
              gen_parse_piqtype t; ios "(Obj)";
            ios "}";
        ]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = List.map gen_option v.option in
  let cases = options @ [
    ios "_ -> piqirun:error_option(Obj, Code)";
  ]
  in
  iol [
    ios "parse_" ^^ ios (some_of v.erlang_name); ios "(X) ->"; indent;
      ios "{Code, Obj} = piqirun:parse_variant(X),"; eol;
      ios "case Code of"; indent;
        iod ";\n        " cases;
        unindent; eol;
      ios "end.";
    unindent; eol;
  ]


let gen_convert_of piqtype erlang_type value =
  gen_convert_value piqtype erlang_type "_of_" value


let gen_alias a ~wire_packed =
  let open Alias in
  let piqtype = some_of a.piqtype in
  let packed = ios (if wire_packed then "packed_" else "") in
  iol [
    packed; ios "parse_"; ios (some_of a.erlang_name); ios "(X) ->"; indent;
      gen_convert_of piqtype a.erlang_type (
        iol [
          gen_parse_piqtype piqtype
            ?erlang_type:a.erlang_type
            ?wire_type:a.protobuf_wire_type
            ~wire_packed;
          ios "(X)";
        ]
      );
      ios ".";
    unindent; eol;
  ]


let gen_alias a =
  let open Alias in
  if Piqi_protobuf.can_be_packed (some_of a.piqtype)
  then
    (* generate another function for packed encoding *)
    iod "\n\n" [
      gen_alias a ~wire_packed:false;
      gen_alias a ~wire_packed:true;
    ]
  else gen_alias a ~wire_packed:false


let gen_list l =
  let open L in
  let packed = ios (if l.protobuf_packed then "packed_" else "") in
  iol [
    ios "parse_" ^^ ios (some_of l.erlang_name); ios "(X) ->"; indent;
      ios "piqirun:parse_"; packed; ios "list(";
        ios "fun ";
          gen_parse_piqtype (some_of l.piqtype) ~wire_packed:l.protobuf_packed;
          ios "/1, ";

        (* when parsing packed repeated fields, we should also accept
         * fields in unpacked representation; therefore, specifying an
         * unpacked field parser as another parameter *)
        if l.protobuf_packed
        then iol [
        ios "fun ";
          gen_parse_piqtype (some_of l.piqtype);
          ios "/1, ";
        ] else iol [];

        ios " X).";
    unindent; eol;
  ]


let gen_spec x =
  iol [
    ios "-spec parse_"; ios (typedef_erlname x); ios "/1 :: (";
      ios "X :: "; ios "piqirun_buffer()"; ios ") -> ";
    ios_gen_in_piqtype (x :> T.piqtype);
    ios ".";
  ]


let gen_def x =
  let generator =
    match x with
      | `alias t -> gen_alias t
      | `record t -> gen_record t
      | `variant t -> gen_variant t
      | `enum t -> gen_enum t
      | `list t -> gen_list t
  in iol [
    gen_spec x; eol;
    generator;
  ]


let gen_defs (defs:T.typedef list) =
  let defs = List.map gen_def defs in
  iod "\n" defs


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_typedef
