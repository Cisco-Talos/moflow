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
 * Piq interface compiler for OCaml
 *)

module C = Piqi_common
open C
open Iolist


(* command-line flags *)
let flag_pp = ref false


(*
 * set ocaml names if not specified by user
 *)

let _ =
  (* normalize Piqi identifiers unless overrided by the command-line option *)
  Piqic_common.flag_normalize := true


(* ocaml name of piqi name *)
let ocaml_name n =
  let n =
    if !Piqic_common.flag_normalize
    then Piqi_name.normalize_name n
    else n
  in
  U.dashes_to_underscores n


let ocaml_lcname n = (* lowercase *)
  String.uncapitalize (ocaml_name n)


let ocaml_ucname n = (* uppercase *)
  String.capitalize (ocaml_name n)


let mlname n =
  Some (ocaml_lcname n)


(* variant of mlname for optional names *)
let mlname' n =
  match n with
    | None -> n
    | Some n -> mlname n


let mlname_field x =
  let open Field in (
    if x.ocaml_array && x.mode <> `repeated
    then C.error x ".ocaml-array flag can be used only with repeated fields";

    if x.ocaml_optional && x.mode <> `optional
    then C.error x ".ocaml-optional flag can be used only with optional fields";

    if x.ocaml_name = None then x.ocaml_name <- mlname' x.name
  )


let mlname_record x =
  let open Record in
  (if x.ocaml_name = None then x.ocaml_name <- mlname (some_of x.name);
   List.iter mlname_field x.field)


let mlname_option x =
  let open Option in
  if x.ocaml_name = None then x.ocaml_name <- mlname' x.name


let mlname_variant x =
  let open Variant in
  (if x.ocaml_name = None then x.ocaml_name <- mlname (some_of x.name);
   List.iter mlname_option x.option)


let mlname_enum x =
  let open Enum in
  (if x.ocaml_name = None then x.ocaml_name <- mlname (some_of x.name);
   List.iter mlname_option x.option)


let mlname_alias x =
  let open Alias in
  if x.ocaml_name = None then x.ocaml_name <- mlname (some_of x.name)


let mlname_list x =
  let open L in
  if x.ocaml_name = None then x.ocaml_name <- mlname (some_of x.name)


let mlname_typedef = function
  | `record x -> mlname_record x
  | `variant x -> mlname_variant x
  | `enum x -> mlname_enum x
  | `alias x -> mlname_alias x
  | `list x -> mlname_list x


let mlname_defs (defs:T.typedef list) =
  List.iter mlname_typedef defs


let mlmodname n =
  let n = Piqi_name.get_local_name n in (* cut module path *)
  Some (ocaml_ucname n ^ "_piqi")


let mlname_func_param func_name param_name param =
  let make_name () =
    Some (func_name ^ "_" ^ param_name)
  in
  match param with
   | None -> ()
   | Some (`alias x) ->
       x.A#ocaml_name <- make_name ()
   | Some (`record x) ->
       x.R#ocaml_name <- make_name ()
   | Some (`variant x) ->
       x.V#ocaml_name <- make_name ()
   | Some (`enum x) ->
       x.E#ocaml_name <- make_name ()
   | Some (`list x) ->
       x.L#ocaml_name <- make_name ()


let mlname_func x =
  let open T.Func in (
    if x.ocaml_name = None then x.ocaml_name <- mlname x.name;
    let func_name = some_of x.ocaml_name in
    mlname_func_param func_name "input" x.resolved_input;
    mlname_func_param func_name "output" x.resolved_output;
    mlname_func_param func_name "error" x.resolved_error;
  )


let mlname_functions l =
  List.iter mlname_func l


let rec mlname_piqi (piqi:T.piqi) =
  let open P in
  begin
    if piqi.ocaml_module = None
    then piqi.ocaml_module <- mlmodname (some_of piqi.modname);

    (* naming function parameters first, because otherwise they will be
     * overriden in mlname_defs *)
    mlname_functions piqi.P#resolved_func;

    mlname_defs piqi.P#resolved_typedef;
    mlname_defs piqi.P#imported_typedef;
    mlname_imports piqi.P#resolved_import;
  end

and mlname_imports imports = List.iter mlname_import imports

and mlname_import import =
  let open Import in
  begin
    match import.piqi with
      | None -> () (* unresolved meaning that is called from piqic_expand.ml *)
      | Some piqi -> (* normal "piqic ocaml" mode -- naming the dependencies *)
          mlname_piqi piqi;
          if import.ocaml_name = None
          then import.ocaml_name <- Some (ocaml_ucname (some_of import.name))
  end


let gen_ocaml_code (piqi: T.piqi) =
  Piqic_common.piqic_common piqi;

  (* set ocaml names that are not specified by user *)
  mlname_piqi piqi;
  mlname_defs !C.builtin_typedefs;

  (* set current module's name *)
  Piqic_ocaml_types.top_modname := some_of piqi.P#ocaml_module;

  (* NOTE: generating them in this order explicitly in order to avoid
   * right-to-left evaluation if we put this code inside the list *)
  let c1 = Piqic_ocaml_types.gen_piqi piqi in
  let c2 = Piqic_ocaml_in.gen_piqi piqi in
  let c3 = Piqic_ocaml_out.gen_piqi piqi in
  let c4 =
    if !Piqic_common.flag_gen_defaults
    then Piqic_ocaml_defaults.gen_piqi piqi
    else iol []
  in
  let code = iol [ c1; c2; c3; c4 ] in
  code


module Main = Piqi_main
open Main


let ocaml_pretty_print ifile ofile =
  let cmd =
    if ofile = "-"
    then Printf.sprintf "camlp4o %s" ifile
    else Printf.sprintf "camlp4o -o %s %s" ofile ifile 
  in
  let res = Sys.command cmd in
  if res <> 0
  then piqi_error ("command execution failed: " ^ cmd)


let gen_embedded_piqi piqi =
  let l = Piqic_common.build_piqi_deps piqi in
  let l = List.map (fun s -> ioq (String.escaped s)) l in
  iol [
    ios "let piqi = ["; iod ";" l; ios "]"
  ]


let gen_output_file ofile code =
  if not !flag_pp
  then
    let ch = Main.open_output ofile in
    Iolist.to_channel ch code;
    Main.close_output ()
  else
    begin
      (* prettyprint generated OCaml code using Camlp4 *)
      let tmp_file = ofile ^ ".tmp.ml" in
      (try
        let tmp_ch = open_out tmp_file in
        Iolist.to_channel tmp_ch code;
        close_out tmp_ch;
      with Sys_error s ->
        piqi_error ("error writing temporary file: " ^ s));
      ocaml_pretty_print tmp_file ofile;
      Main.add_tmp_file tmp_file;
    end


let piqic piqi =
  (* call piq interface compiler for ocaml *)
  let code = gen_ocaml_code piqi in
  let code =
    if !Piqic_common.flag_embed_piqi
    then iol [ code; gen_embedded_piqi piqi ]
    else code
  in

  (* chdir to the output directory *)
  Main.chdir_output !odir;

  let ofile =
    match !ofile with
      | "" ->
          let modname = some_of piqi.P#ocaml_module in
          String.uncapitalize modname ^ ".ml"
      | x -> x
  in
  gen_output_file ofile code


let init () =
  Piqi_config.add_include_extension "ocaml"


let piqic_file ifile =
  init ();

  (* load input .piqi file *)
  let piqi = Piqi.load_piqi ifile in
  piqic piqi


let usage = "Usage: piqic ocaml [options] <.piqi file>\nOptions:"


let common_speclist =
  [
    arg_C;

    "--pp", Arg.Set flag_pp,
      "pretty-print output using CamlP4 (camlp4o)"; 
    Piqic_common.arg__gen_defaults;
    Piqic_common.arg__normalize;
    arg__leave_tmp_files;
  ]

let speclist =
  Main.common_speclist
  @
  [ arg_o; Piqic_common.arg__embed_piqi ]
  @
  common_speclist


let run () =
  Main.parse_args () ~usage ~speclist;
  piqic_file !ifile

 
let _ =
  Main.register_command run "ocaml" "generate OCaml stubs from %.piqi"

