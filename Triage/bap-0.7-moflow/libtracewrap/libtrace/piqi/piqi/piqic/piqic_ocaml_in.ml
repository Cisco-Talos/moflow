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
 * Typefull generator generator for decoding piq data from wire (Protocol
 * Buffers wire) format.
 *)

open Piqi_common
open Iolist


(* reuse several functions *)
open Piqic_ocaml_types
open Piqic_ocaml_out


let rec gen_parse_type ocaml_type wire_type wire_packed x =
  let packed = ios (if wire_packed then "packed_" else "") in
  match x with
    | `any ->
        if !Piqic_common.is_self_spec
        then ios "parse_any"
        else ios "Piqi_piqi.parse_any"
    | `alias a when wire_type <> None ->
        (* need special handing for wire_type override *)
        gen_parse_type a.A#ocaml_type wire_type wire_packed (some_of a.A#piqtype)
    | (#T.typedef as x) ->
        let modname = gen_parent x in
        iol [
          modname;
          packed; ios "parse_";
          ios (typedef_mlname x)
        ]
    | _ -> (* gen parsers for built-in types *)
        iol [
          gen_cc "(fun x -> let count = next_count() in refer count (";
            ios "Piqirun.";
            ios (gen_ocaml_type_name x ocaml_type);
            ios "_of_"; packed;
            ios (W.get_wire_type_name x wire_type);
          gen_cc " x))";
        ]


let gen_parse_piqtype ?ocaml_type ?wire_type ?(wire_packed=false) (t:T.piqtype) =
  gen_parse_type ocaml_type wire_type wire_packed t


(* TODO: parse defaults once at boot time rather than each time when we need to
 * parse a field *)
let gen_default = function
  | None -> iol []
  | Some piqi_any ->
      let pb = Piqobj.pb_of_piqi_any piqi_any in
      iol [ios "~default:"; ioq (String.escaped pb) ]


let esc x = ios "_" ^^ ios x


(* let gen_field_cons = Gen_i_piq.gen_field_cons *)
let gen_field_cons rname f =
  let open Field in
  let fname = mlname_of_field f in
  let ffname = (* fully-qualified field name *)
    iol [ios rname; ios "."; ios fname]
  in 
  (* field construction code *)
  iod " " [ ffname; ios "="; esc fname; ios ";" ]


let gen_field_parser f =
  let open Field in
  let fname = mlname_of_field f in
  let mode = gen_mode f in
  let fcons =
  match f.piqtype with
    | Some piqtype ->
        (* field constructor *)
        iod " "
          [
            (* "parse_(required|optional|repeated)_field" function invocation *)
            ios "Piqirun.parse_" ^^ ios mode ^^ ios "_field";
              gen_code f.code;
              gen_parse_piqtype piqtype ~wire_packed:f.protobuf_packed;
              (* when parsing packed repeated fields, we should also accept
               * fields in unpacked representation; therefore, specifying an
               * unpacked field parser as another parameter *)
              if f.protobuf_packed then gen_parse_piqtype piqtype else iol [];
              ios " x";
              gen_default f.default;
          ]
    | None ->
        (* flag constructor *)
        iod " " [ 
          gen_cc "incr_count_if_true (";
            ios "Piqirun.parse_flag"; gen_code f.code; ios " x";
          gen_cc ")";
        ]
  in
  (* field parsing code *)
  iol [ ios "let "; esc fname; ios ", x = "; fcons; ios " in " ]


let gen_record r =
  (* fully-qualified capitalized record name *)
  let rname = capitalize (some_of r.R#ocaml_name) in
  (* NOTE: fields are already ordered by their codes when Piqi is loaded *)
  let fields = r.R#wire_field in
  let fconsl = (* field constructor list *)
    if fields <> []
    then List.map (gen_field_cons rname) fields
    else [ios rname; ios "."; ios "_dummy = ()"]
  in
  let fparserl = (* field parsers list *)
    List.map gen_field_parser fields
  in
  let rcons = (* record constructor *)
    iol [
      iol fparserl;
      ios "Piqirun.check_unparsed_fields x; {"; iol fconsl; ios "}";
    ]
  in (* parse_<record-name> function delcaration *)
  iod " "
    [
      ios "parse_" ^^ ios (some_of r.R#ocaml_name); ios "x =";
      ios "let x = Piqirun.parse_record x in";
      gen_cc "let count = next_count() in refer count (";
      rcons;
      gen_cc ")";
    ]


let gen_const c =
  let open Option in
  let name = gen_pvar_name (some_of c.ocaml_name) in
  iol [ios "| "; gen_code c.code; ios "l -> "; name]


let gen_enum e ~wire_packed =
  let open Enum in
  let consts = List.map gen_const e.option in
  let packed = ios (if wire_packed then "packed_" else "") in
  iol [
    packed; ios "parse_"; ios (some_of e.ocaml_name); ios " x = ";
    gen_cc "let count = next_count() in refer count (";
      ios "match Piqirun.int32_of_"; packed; ios "signed_varint x with ";
      iol consts;
      ios "| x -> Piqirun.error_enum_const x";
    gen_cc ")";
  ]


let gen_enum e =
  (* generate two functions: one for parsing normal value; another one -- for
   * packed value *)
  iod " and " [
    gen_enum e ~wire_packed:false;
    gen_enum e ~wire_packed:true;
  ]


let rec gen_option varname o =
  let open Option in
  match o.ocaml_name, o.piqtype with
    | Some mln, None -> (* boolean true *)
        iod " " [
          ios "|"; gen_code o.code; ios "when x = Piqirun.Varint 1"; ios "->";
            (* NOTE: providing special handling for boxed values, see "refer" *)
            gen_cc "let count = next_count() in refer count";
            gen_pvar_name mln;
        ]
    | None, Some ((`variant _) as t) | None, Some ((`enum _) as t) ->
        iod " " [
          ios "|"; gen_code o.code; ios "->";
            ios "("; gen_parse_piqtype t; ios "x :>"; ios varname; ios ")"
        ]
    | _, Some t ->
        let n = mlname_of_option o in
        iod " " [
          ios "|"; gen_code o.code; ios "->";
            ios "let res = ";
              gen_cc "let count = curr_count() in refer count (";
              gen_parse_piqtype t; ios "x";
              gen_cc ")";
              ios "in";
              gen_pvar_name n; ios "res";
        ]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = List.map (gen_option (some_of v.ocaml_name)) v.option in
  iod " "
    [
      ios "parse_" ^^ ios (some_of v.ocaml_name); ios "x =";
      ios "let code, x = Piqirun.parse_variant x in";
        gen_cc "let count = next_count() in refer count (";
        ios "match code with";
          iod " " options;
          ios "| _ -> Piqirun.error_variant x code";
          gen_cc ")";
    ]


let gen_convert_of piqtype ocaml_type value =
  gen_convert_value piqtype ocaml_type "_of_" value


let gen_alias a ~wire_packed =
  let open Alias in
  let packed = ios (if wire_packed then "packed_" else "") in
  let piqtype = some_of a.piqtype in
  iol [
    packed; ios "parse_"; ios (some_of a.ocaml_name); ios " x = ";
    gen_convert_of piqtype a.ocaml_type (
      iol [
        gen_parse_piqtype
          piqtype
          ?ocaml_type:a.ocaml_type
          ?wire_type:a.protobuf_wire_type
          ~wire_packed;
        ios " x";
      ]
    )
  ]


let gen_alias a =
  let open Alias in
  if Piqi_protobuf.can_be_packed (some_of a.piqtype)
  then
    (* generate another function for packed encoding *)
    iod " and " [
      gen_alias a ~wire_packed:false;
      gen_alias a ~wire_packed:true;
    ]
  else gen_alias a ~wire_packed:false


let gen_list l =
  let open L in
  let repr = gen_list_repr l in
  iol [
    ios "parse_"; ios (some_of l.ocaml_name); ios " x = ";
      gen_cc "let count = next_count() in refer count (";
        (* Piqirun.parse_(packed_)?(list|array|array32|array64) *)
        ios "Piqirun.parse_"; repr;
          ios " ("; gen_parse_piqtype (some_of l.piqtype) ~wire_packed:l.protobuf_packed; ios ")";

          (* when parsing packed repeated fields, we should also accept
           * fields in unpacked representation; therefore, specifying an
           * unpacked field parser as another parameter *)
          if l.protobuf_packed
          then iol [
          ios " ("; gen_parse_piqtype (some_of l.piqtype); ios ")";
          ]
          else iol [];

          ios " x";
      gen_cc ")";
  ]


let gen_def = function
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t
  | `alias t -> gen_alias t


let gen_defs (defs:T.typedef list) =
  let defs = List.map gen_def defs in
  if defs = []
  then iol []
  else iod " "
    [
      gen_cc "let next_count = Piqloc.next_icount";
      gen_cc "let curr_count () = !Piqloc.icount";
      (* NOTE: providing special handling for boxed objects, since they are not
       * references and can not be uniquely identified. Moreover they can mask
       * integers which are used for enumerating objects *)
      gen_cc "let refer ref obj =
        if not (Obj.is_int (Obj.repr obj))
        then Piqloc.addrefret ref obj
        else obj";
      gen_cc "let incr_count_if_true ((obj, _) as res) =
        if obj then ignore(next_count());
        res";
      ios "let rec"; iod " and " defs;
      ios "\n";
      (*
      ios "end\n";
      *)
    ]


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_typedef
