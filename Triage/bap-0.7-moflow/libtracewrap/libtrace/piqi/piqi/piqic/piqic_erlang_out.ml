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
 * Typefull generator generator for encoding piq data into wire (Protocol
 * Buffers wire) format.
 *)

open Piqi_common
open Iolist


(* reuse several functions *)
open Piqic_erlang_types


module W = Piqi_protobuf


let gen_code = Piqic_common.gen_code


let gen_erlang_type_name t ot =
  gen_piqtype t ot


let gen_parent x =
  match get_parent x with
    | `import x -> (* imported name *)
        let piqi = some_of x.Import.piqi in
        let erlang_modname = some_of piqi.P#erlang_module in
        ios erlang_modname ^^ ios ":"
    | _ -> iol []


let rec gen_gen_type erlang_type wire_type wire_packed x =
  let packed = ios (if wire_packed then "packed_" else "") in
  match x with
    | `any ->
        if !Piqic_common.is_self_spec
        then ios "gen_" ^^ ios !any_erlname
        else ios "piqi_piqi:gen_piqi_any"
    | `alias a when wire_type <> None ->
        (* need special handing for wire_type override *)
        gen_gen_type a.A#erlang_type wire_type wire_packed (some_of a.A#piqtype)
    | (#T.typedef as x) ->
        let modname = gen_parent x in
        iol [
          modname; packed; ios "gen_"; ios (typedef_erlname x)
        ]
    | _ -> (* gen generators for built-in types *)
        iol [
          ios "piqirun:";
          ios (gen_erlang_type_name x erlang_type);
          ios "_to_"; packed;
          ios (W.get_wire_type_name x wire_type);
        ]


let gen_gen_piqtype ?erlang_type ?wire_type ?(wire_packed=false) (t :T.piqtype) =
  gen_gen_type erlang_type wire_type wire_packed t


let gen_mode f =
  let open F in
  match f.mode with
    | `required -> "required"
    | `optional -> "optional"
    | `repeated ->
        if f.protobuf_packed
        then "packed_repeated"
        else "repeated"


let gen_field rname f =
  let open Field in
  let fname = erlname_of_field f in
  let ffname = (* fully-qualified field name *)
    iol [ios "X#"; ios rname; ios "."; ios fname]
  in 
  let mode = gen_mode f in
  let fgen =
    match f.piqtype with
      | Some piqtype ->
          (* field generation code *)
          iol [
            ios "piqirun:gen_" ^^ ios mode ^^ ios "_field(";
              gen_code f.code; ios ", ";
              ios "fun ";
                gen_gen_piqtype piqtype ~wire_packed:f.protobuf_packed;
                if f.protobuf_packed (* arity *)
                then ios "/1, "
                else ios "/2, ";
              ffname;
            ios ")"
          ]
      | None ->
          (* flag generation code *)
          iod " " [
            ios "piqirun:gen_flag(";
              gen_code f.code; ios ", ";
              ffname;
            ios ")";
          ]
  in fgen


let gen_record r =
  let rname = scoped_name (some_of r.R#erlang_name) in
  (* NOTE: fields are already ordered by their codes when Piqi is loaded *)
  let fields = r.R#wire_field in
  let fgens = (* field generators list *)
    List.map (gen_field rname) fields
  in
  let arg_variable = (* prevent Erlang warning on unused variable *)
    if fields <> []
    then ios "X"
    else iol [ ios "#"; ios rname; ios "{}" ]
  in (* gen_<record-name> function delcaration *)
  iol
    [
      ios "gen_"; ios (some_of r.R#erlang_name);
        ios "(Code, "; arg_variable; ios ") ->"; indent;
        ios "piqirun:gen_record(Code, ["; indent;
          iod ",\n        " fgens;
          unindent; eol;
        ios "]).";
        unindent; eol;
    ]


let gen_const c =
  let open Option in
  iol [
    ios (some_of c.erlang_name); ios " -> "; gen_code c.code;
  ]


let gen_enum_consts l =
  let consts = List.map gen_const l in
  iol [
    ios "case X of"; indent;
    iod ";\n        " consts;
    unindent; eol;
    ios "end";
  ]


let gen_enum e =
  let open Enum in
  iol [
    ios "gen_"; ios (some_of e.erlang_name); ios "(Code, X) ->"; indent;
      ios "piqirun:integer_to_signed_varint(Code,"; indent;
        gen_enum_consts e.option;
        unindent; eol;
      ios ").";
      unindent; eol;
  ]


let gen_packed_enum e =
  let open Enum in
  iol [
    ios "packed_gen_"; ios (some_of e.erlang_name); ios "(X) ->"; indent;
        ios "piqirun:integer_to_packed_signed_varint("; indent;
          gen_enum_consts e.option;
        unindent; eol;
      ios ").";
      unindent; eol;
  ]


let gen_enum e =
  (* generate two functions: one for generating normal value; another one -- for
   * packed value *)
  iod "\n\n" [
    gen_enum e;
    gen_packed_enum e;
  ]


let gen_inner_option pattern outer_option =
  let open Option in
  let o = some_of outer_option in
  let t = some_of o.piqtype in
  let res =
    iol [
      pattern; ios " -> ";
        gen_gen_piqtype t; ios "("; gen_code o.code; ios ", X)";
    ]
  in [res]


let rec gen_option outer_option o =
  let open Option in
  (* recursively generate cases from "included" variants *)
  let gen_options options =
        if outer_option <> None
        then U.flatmap (gen_option outer_option) options
        else U.flatmap (gen_option (Some o)) options
  in
  match o.erlang_name, o.piqtype with
    | Some ename, None -> (* gen true *)
        if outer_option <> None
        then gen_inner_option (ios ename) outer_option
        else
          let res =
            iol [
              ios ename; ios " -> ";
                ios "piqirun:gen_bool_field("; gen_code o.code; ios ", true)";
            ]
          in [res]
    | None, Some (`variant v) ->
        (* recursively generate cases from "included" variants *)
        gen_options v.V#option
    | None, Some (`enum e) ->
        (* recursively generate cases from "included" enums *)
        gen_options e.E#option
    | _, Some t ->
        let ename = erlname_of_option o in
        if outer_option <> None
        then gen_inner_option (iol [ ios "{"; ios ename; ios ", _}"]) outer_option
        else
          let res = 
            iol [
              ios "{"; ios ename; ios ", Y} -> ";
                gen_gen_piqtype t; ios "("; gen_code o.code; ios ", Y)";
            ]
          in [res]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = U.flatmap (gen_option None) v.option in
  iol
    [
      ios "gen_" ^^ ios (some_of v.erlang_name);
      ios "(Code, X) ->"; indent;
      ios "piqirun:gen_variant(Code,"; indent;
        ios "case X of"; indent; iod ";\n            " options;
        unindent; eol;
        ios "end";
        unindent; eol;
        ios ").";
        unindent; eol;
    ]


let gen_convert_value piqtype erlang_type direction value =
  match piqtype with
    | (#T.typedef as typedef) when erlang_type <> None -> (* custom Erlang type *)
        iol [
          ios (some_of erlang_type);
          ios direction;
          ios (typedef_erlname typedef);
          ios "("; value; ios ")";
        ]
    | _ ->
        value


let gen_convert_to piqtype erlang_type value =
  gen_convert_value piqtype erlang_type "_to_" value


let gen_alias a =
  let open Alias in
  let piqtype = some_of a.piqtype in
  iol [
    ios "gen_"; ios (some_of a.erlang_name);
    ios "(Code, X) ->"; indent;
      gen_gen_piqtype piqtype ?erlang_type:a.erlang_type ?wire_type:a.protobuf_wire_type;
      ios "(Code, "; gen_convert_to piqtype a.erlang_type (ios "X"); ios ").";
    unindent; eol;
  ]


let gen_packed_alias a =
  let open Alias in
  let piqtype = some_of a.piqtype in
  iol [
    ios "packed_gen_"; ios (some_of a.erlang_name);
    ios "(X) ->"; indent;
      gen_gen_piqtype piqtype
        ?erlang_type:a.erlang_type
        ?wire_type:a.protobuf_wire_type
        ~wire_packed:true;
      ios "("; gen_convert_to piqtype a.erlang_type (ios "X"); ios ").";
    unindent; eol;
  ]


let gen_alias a =
  let open Alias in
  if Piqi_protobuf.can_be_packed (some_of a.piqtype)
  then
    (* generate another function for packed encoding *)
    iod "\n\n" [
      gen_alias a;
      gen_packed_alias a;
    ]
  else gen_alias a


let gen_list l =
  let open L in
  let packed = ios (if l.protobuf_packed then "packed_" else "") in
  iol [
    ios "gen_" ^^ ios (some_of l.erlang_name);
    ios "(Code, X) ->"; indent;
      ios "piqirun:gen_"; packed; ios "list(Code, ";
        ios "fun ";
          gen_gen_piqtype (some_of l.piqtype) ~wire_packed:l.protobuf_packed;
          if l.protobuf_packed (* arity *)
          then ios "/1, X)."
          else ios "/2, X).";
    unindent; eol;
  ]


(* generate gen_<name>/2 specs and functions *)
let gen_spec_2 x =
  iol [
    ios "-spec gen_"; ios (typedef_erlname x); ios "/2 :: (";
      ios "Code :: piqirun_code(), ";
      ios "X :: "; ios_gen_out_piqtype (x :> T.piqtype); ios ") -> ";
    ios "iolist().";
  ]


let gen_def_2 x =
  let generator =
    match x with
      | `alias t -> gen_alias t
      | `record t -> gen_record t
      | `variant t -> gen_variant t
      | `enum t -> gen_enum t
      | `list t -> gen_list t
  in iol [
    gen_spec_2 x; eol;
    generator;
  ]


(* generate gen_<name>/1 specs and functions *)
let gen_spec_1 x =
  iol [
    ios "-spec gen_"; ios (typedef_erlname x); ios "/1 :: (";
      ios "X :: "; ios_gen_out_piqtype (x :> T.piqtype); ios ") -> ";
    ios "iolist().";
  ]

let gen_def_1 x =
  let func_name = ios "gen_" ^^ ios (typedef_erlname x) in
  let generator =
    iol [
      func_name; ios "(X) ->"; indent;
        func_name; ios "('undefined', X).";
      unindent; eol;
    ]
  in iol [
    gen_spec_1 x; eol;
    generator;
  ]


let gen_defs (defs:T.typedef list) =
  let defs_2 = List.map gen_def_2 defs in
  let defs_1 = List.map gen_def_1 defs in
  iod "\n" (defs_2 @ defs_1)


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_typedef
