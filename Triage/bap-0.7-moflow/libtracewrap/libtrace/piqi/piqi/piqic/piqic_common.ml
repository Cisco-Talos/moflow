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
 * This module contain functionality that is shared between several
 * language-specific piqi compilers.
 *)

module C = Piqi_common
open C
open Iolist


let gen_code = function
  | None -> assert false
  | Some code -> ios (Int32.to_string code)


(* generate default value for a built-in type *)
let gen_builtin_default_value wire_type t =
  let gen_obj code x =
    match x with
      | #T.typedef | `any -> assert false
      | `int -> Piqobj_to_protobuf.gen_int code 0L ?wire_type
      | `float -> Piqobj_to_protobuf.gen_float code 0.0 ?wire_type
      | `bool -> Piqobj_to_protobuf.gen_bool code false
      | `string | `binary | `text | `word ->
          Piqobj_to_protobuf.gen_string code ""
  in
  Piqloc.pause ();
  let res = Piqirun.gen_binobj gen_obj t in
  Piqloc.resume ();
  res


(* indication whether there is a defintion that uses "piqi-any" type *)
let depends_on_piqi_any = ref false


(* indication whether the module that is being processed is a Piqi self-spec,
 * i.e. the module's name is "piqi" or it includes another module named "piqi"*)
let is_self_spec = ref false


let get_builtin_defs piqi seen_defs def =
  let is_local_def def = (C.get_parent_piqi def == piqi) in
  let get_builtin_def = function
    | #T.typedef as x when  (* previously unseen built-in local def? *)
          C.is_builtin_def x &&
          not (is_local_def x) &&
          not (List.memq x seen_defs) ->
        [x]
    | _ ->
        []
  in
  let get_builtin_def_opt = function
    | Some x -> get_builtin_def x
    | None -> []
  in
  match def with
    | `record x -> U.flatmap (fun x -> get_builtin_def_opt x.F#piqtype) x.R#field
    | `variant x -> U.flatmap (fun x -> get_builtin_def_opt x.O#piqtype) x.V#option
    | `list x -> get_builtin_def (some_of x.L#piqtype)
    | `enum _ -> []
    | `alias a -> get_builtin_def (some_of a.A#piqtype)


(* get all built-in defintions used by (i.e. reacheable from) the module's
 * definitions *)
let get_builtin_dependencies piqi =
  let rec aux accu root_defs =
    let new_builtin_defs = U.flatmap (get_builtin_defs piqi accu) root_defs in
    if new_builtin_defs = []
    then accu
    else
      let accu = U.uniqq (new_builtin_defs @ accu) in
      aux accu new_builtin_defs
  in
  aux [] piqi.P#resolved_typedef


let piqic_common piqi =
  (* if no definition uses "piqi-any" type, "piqi" module will not be included
   * in order to avoid unnecessary dependency on Piqtype module *)
  depends_on_piqi_any := Piqi_common.depends_on_piqi_any piqi;

  if not !is_self_spec
  then is_self_spec := Piqi_common.is_self_spec piqi;

  (* implicitly add built-in defintions reachable from the current module *)
  let builtin_defs = get_builtin_dependencies piqi in

  piqi.P#resolved_typedef <- builtin_defs @ piqi.P#resolved_typedef;
  ()


let rec get_piqi_deps piqi =
  let imports =
    List.map (fun x -> some_of x.T.Import#piqi) piqi.P#resolved_import
  in
  (* get all imports' dependencies recursively *)
  let import_deps =
    U.flatmap (fun piqi ->
        U.flatmap get_piqi_deps piqi.P#included_piqi
      ) imports
  in
  (* remove duplicate entries *)
  let deps = U.uniqq (import_deps @ imports) in
  deps @ [piqi]


let encode_embedded_piqi piqi =
  let iodata = Piqi.piqi_to_pb piqi in
  Piqirun.to_string iodata


(* build a list of all import dependencies including the specified module and
 * encode each Piqi module in the list using Protobuf encoding *)
let build_piqi_deps piqi =
  let deps = get_piqi_deps piqi in
  List.map encode_embedded_piqi deps


(* common command-line arguments processing *)
let flag_normalize = ref false
let flag_gen_defaults = ref false
let flag_embed_piqi = ref false


let arg__normalize =
  "--normalize", Arg.Bool (fun x -> flag_normalize := x),
    "<true|false> normalize identifiers (default: true)"

let arg__gen_defaults =
    "--gen-defaults", Arg.Set flag_gen_defaults,
      "generate default value constructors for generated types"

let arg__embed_piqi =
    "--embed-piqi", Arg.Set flag_embed_piqi,
      "embed Piqi modules encoded in binary format in the generated code"

