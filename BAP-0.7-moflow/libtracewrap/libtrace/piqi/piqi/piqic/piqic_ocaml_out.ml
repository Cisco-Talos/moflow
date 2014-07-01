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
open Piqic_ocaml_types


module W = Piqi_protobuf


let gen_code = Piqic_common.gen_code


let gen_ocaml_type_name t ot =
  gen_piqtype t ot


let gen_parent x =
  match get_parent x with
    | `import x -> (* imported name *)
        let ocaml_modname = some_of x.Import#ocaml_name in
        ios ocaml_modname ^^ ios "."
    | _ -> iol []


let rec gen_gen_type ocaml_type wire_type wire_packed x =
  let packed = ios (if wire_packed then "packed_" else "") in
  match x with
    | `any ->
        if !Piqic_common.is_self_spec
        then ios "(fun code x -> gen__any code x)"
        else ios "(fun code x -> Piqi_piqi.gen__any code x)"
    | `alias a when wire_type <> None ->
        (* need special handing for wire_type override *)
        gen_gen_type a.A#ocaml_type wire_type wire_packed (some_of a.A#piqtype)
    | (#T.typedef as x) ->
        let modname = gen_parent x in
        iol [
          modname;
          packed; ios "gen__";
          ios (typedef_mlname x)
        ]
    | _ -> (* gen generators for built-in types *)
        iol [
          (if wire_packed then gen_cc "(reference1 " else gen_cc "(reference ");
          ios "Piqirun.";
          ios (gen_ocaml_type_name x ocaml_type);
          ios "_to_"; packed;
          ios (W.get_wire_type_name x wire_type);
          gen_cc ")";
        ]

let gen_gen_piqtype ?ocaml_type ?wire_type ?(wire_packed=false) t =
  gen_gen_type ocaml_type wire_type wire_packed t


(* calculates and generates the width of a packed wire element in bits:
 * generated value can be 32, 64 or empty *)
let gen_wire_elem_width piqtype wire_packed =
  let rec aux ?wire_type = function
    | `alias x ->
        (* NOTE: overriding upper-level wire type even if this one is undefined
         *)
        aux (some_of x.A#piqtype) ?wire_type:x.A#protobuf_wire_type
    | t ->
        Piqi_protobuf.get_wire_type_width t wire_type
  in
  if not wire_packed
  then ""
  else
    match aux (some_of piqtype) with
      | Some x -> string_of_int x
      | None -> ""


let gen_mode f =
  let open F in
  match f.mode with
    | `required -> "required"
    | `optional when f.default <> None && (not f.ocaml_optional) ->
        "required" (* optional + default *)
    | `optional -> "optional"
    | `repeated ->
        let mode =
          if f.protobuf_packed
          then "packed_repeated"
          else "repeated"
        in
        if f.ocaml_array
        then
          let width = gen_wire_elem_width f.piqtype f.protobuf_packed in
          mode ^ "_array" ^ width
        else mode


let gen_field rname f =
  let open Field in
  let fname = mlname_of_field f in
  let ffname = (* fully-qualified field name *)
    iod "." [ios "x"; ios rname; ios fname]
  in 
  let mode = gen_mode f in
  let fgen =
    match f.piqtype with
      | Some piqtype ->
          (* field generation code *)
          iod " "
            [ 
              ios "Piqirun.gen_" ^^ ios mode ^^ ios "_field";
                gen_code f.code;
                gen_gen_piqtype piqtype ~wire_packed:f.protobuf_packed;
                ffname
            ]
      | None ->
          (* flag generation code *)
          iod " " [
            gen_cc "reference_if_true ";
            ios "Piqirun.gen_flag"; gen_code f.code; ffname;
          ]
  in (fname, fgen)


let gen_record r =
  (* fully-qualified capitalized record name *)
  let rname = capitalize (some_of r.R#ocaml_name) in
  (* NOTE: fields are already ordered by their codes when Piqi is loaded *)
  let fields = r.R#wire_field in
  let fgens = (* field generators list *)
    List.map (gen_field rname) fields
  in
  (* field names *)
  let fnames, _ = List.split fgens in

  let esc x = ios "_" ^^ ios x in

  (* field generator code *)
  let fgens_code = List.map
    (fun (name, gen) -> iol [ ios "let "; esc name; ios " = "; gen; ios " in "])
    fgens
  in (* gen_<record-name> function delcaration *)
  iod " "
    [
      ios "gen__" ^^ ios (some_of r.R#ocaml_name); ios "code x =";
        gen_cc "refer x;";
        iol fgens_code;
        ios "Piqirun.gen_record code"; 
        ios "["; iod ";" (List.map esc fnames); ios "]";
    ]


let gen_const c =
  let open Option in
  iod " " [
    ios "|"; gen_pvar_name (some_of c.ocaml_name); ios "->";
      gen_code c.code ^^ ios "l"; (* ocaml int32 literal *)
  ]


let gen_enum_consts l =
  let consts = List.map gen_const l in
  iol [ ios "(match x with "; iol consts; ios ")" ]


let gen_enum e =
  let open Enum in
  iol [
    ios "gen__"; ios (some_of e.ocaml_name); ios " code x = ";
      gen_cc "refer x;";
      ios "Piqirun.int32_to_signed_varint code "; gen_enum_consts e.option;
  ]


let gen_packed_enum e =
  let open Enum in
  iol [
    ios "packed_gen__"; ios (some_of e.ocaml_name); ios " x = ";
      gen_cc "refer x;";
      ios "Piqirun.int32_to_packed_signed_varint "; gen_enum_consts e.option;
  ]


let gen_enum e =
  (* generate two functions: one for generating normal value; another one -- for
   * packed value *)
  iod " and " [
    gen_enum e;
    gen_packed_enum e;
  ]


let rec gen_option o =
  let open Option in
  match o.ocaml_name, o.piqtype with
    | Some mln, None -> (* gen true *)
        iod " " [
          ios "|"; gen_pvar_name mln; ios "->";
            gen_cc "refer x;";
            ios "Piqirun.gen_bool_field"; gen_code o.code; ios "true";
        ]
    | None, Some ((`variant _) as t) | None, Some ((`enum _) as t) ->
        let ocaml_name = typedef_mlname t in
        let scoped_name =
          match get_parent t with
            | `import x -> (* imported name *)
                let ocaml_modname = some_of x.Import#ocaml_name in
                (ocaml_modname ^ "." ^ ocaml_name)
            | _ -> ocaml_name (* local name *)
        in
        iod " " [
          ios "| (#" ^^ ios scoped_name; ios " as x) ->";
            gen_gen_piqtype t; gen_code o.code; ios "x";
        ]
    | _, Some t ->
        let mln = mlname_of_option o in
        iod " " [
          ios "|"; gen_pvar_name mln; ios "x ->";
            gen_gen_piqtype t; gen_code o.code; ios "x";
        ]
    | None, None -> assert false


let gen_variant v =
  let open Variant in
  let options = List.map gen_option v.option in
  iod " "
    [
      ios "gen__" ^^ ios (some_of v.ocaml_name);
      ios "code (x:" ^^ ios_gen_piqtype (`variant v) ^^ ios ") =";
      gen_cc "refer x;";
      ios "Piqirun.gen_record code [(match x with"; iol options; ios ")]";
    ]


let gen_convert_value piqtype ocaml_type direction value =
  match piqtype with
    | (#T.typedef as typedef) when ocaml_type <> None -> (* custom OCaml type *)
        iol [
          ios "(";
            ios (some_of ocaml_type);
            ios direction;
            ios (typedef_mlname typedef);
            ios "("; value; ios ")";
          ios ")"
        ]
    | _ ->
        value


let gen_convert_to piqtype ocaml_type value =
  gen_convert_value piqtype ocaml_type "_to_" value


let gen_alias a =
  let open Alias in
  let piqtype = some_of a.piqtype in
  iol [
    ios "gen__"; ios (some_of a.ocaml_name);
    ios " code x = ";
      gen_gen_piqtype piqtype ?ocaml_type:a.ocaml_type ?wire_type:a.protobuf_wire_type;
      ios " code"; gen_convert_to piqtype a.ocaml_type (ios " x");
  ]


let gen_packed_alias a =
  let open Alias in
  let piqtype = some_of a.piqtype in
  iol [
    ios "packed_gen__"; ios (some_of a.ocaml_name);
    ios " x = ";
      gen_gen_piqtype piqtype
        ?ocaml_type:a.ocaml_type
        ?wire_type:a.protobuf_wire_type
        ~wire_packed:true;
      gen_convert_to piqtype a.ocaml_type (ios " x");
  ]


let gen_alias a =
  let open Alias in
  if Piqi_protobuf.can_be_packed (some_of a.piqtype)
  then
    (* generate another function for packed encoding *)
    iod " and " [
      gen_alias a;
      gen_packed_alias a;
    ]
  else gen_alias a


(* generate: (packed_)?(list|array|array32|array64) *)
let gen_list_repr l =
  let open L in
  let packed = ios (if l.protobuf_packed then "packed_" else "") in
  let repr =
    if l.ocaml_array
    then ios "array" ^^ ios (gen_wire_elem_width l.piqtype l.protobuf_packed)
    else ios "list"
  in
  packed ^^ repr


let gen_list l =
  let open L in
  let repr = gen_list_repr l in
  iol [
    ios "gen__"; ios (some_of l.ocaml_name); ios " code x = ";
      gen_cc "reference ";
        (* Piqirun.gen_(packed_)?(list|array|array32|array64) *)
        ios "(Piqirun.gen_"; repr; ios " (";
          gen_gen_piqtype (some_of l.piqtype) ~wire_packed:l.protobuf_packed;
        ios ")) code x";
  ]


let gen_def = function
  | `alias t -> gen_alias t
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t


(* generate gen_<name>/1 functions *)
let gen_def_1 x =
  let name = ios (typedef_mlname x) in
  iol [
    ios "let gen_"; name; ios " x = ";
      ios"gen__"; name; ios " (-1) x";
    ios "\n";
  ]


let gen_defs (defs:T.typedef list) =
  if defs = []
  then iol []
  else
    let defs_2 = List.map gen_def defs in
    let defs_1 = List.map gen_def_1 defs in
    iod " " [
      gen_cc "let next_count = Piqloc.next_ocount";
      (* NOTE: providing special handling for boxed objects, since they are not
       * references and can not be uniquely identified. Moreover they can mask
       * integers which are used for enumerating objects *)
      gen_cc "let refer obj =
        let count = next_count () in
        if not (Obj.is_int (Obj.repr obj))
        then Piqloc.addref obj count";
      gen_cc "let reference f code x = refer x; f code x";
      gen_cc "let reference1 f x = refer x; f x";
      gen_cc "let reference_if_true f code x =
        if x
        then reference f code x
        else f code x";
      ios "let rec"; iod " and " defs_2;
      ios "\n\n";
      iol defs_1;
      ios "\n";
    ]


let gen_piqi (piqi:T.piqi) =
  gen_defs piqi.P#resolved_typedef
