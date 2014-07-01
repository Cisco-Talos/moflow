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


module Main = Piqi_main
open Main


(* command-line arguments *)
let input_encoding = ref ""
let output_encoding = ref ""
let typename = ref ""
let flag_add_defaults = ref false
let flag_embed_piqi = ref false
let flag_json_omit_null_fields = ref true


let usage = "Usage: piqi convert [options] [input file] [output file]\nOptions:"


let arg__t =
    "-t", Arg.Set_string output_encoding,
    "pb|json|xml|piq|pib output encoding (piq is used by default)"

let arg__type =
    "--type", Arg.Set_string typename,
    "<typename> type of converted object"

let arg__add_defaults =
    "--add-defaults", Arg.Set flag_add_defaults,
    "add default field values to converted records"

let arg__json_omit_null_fields =
    "--json-omit-null-fields", Arg.Bool (fun x -> flag_json_omit_null_fields := x),
    "true|false omit null fields in JSON output (default=true)"

let arg__gen_extended_piqi_any =
    "--gen-extended-piqi-any", Arg.Set Piqi_config.gen_extended_piqi_any,
    "use extended representation of piqi-any values in XML and JSON output"


let speclist = Main.common_speclist @
  [
    Piqi_main.arg__strict;
    arg_o;

    "-f", Arg.Set_string input_encoding,
    "pb|json|xml|piq|pib input encoding";

    arg__t;
    arg__type;
    arg__add_defaults;
    arg__json_omit_null_fields;
    arg__gen_extended_piqi_any;

    "--embed-piqi", Arg.Set flag_embed_piqi,
    "embed Piqi dependencies, i.e. Piqi specs which the input depends on";

    Piqi_main.arg__include_extension;

    arg__;
  ]


let first_load = ref true

let load_piqi fname :Piq.obj =
  if !first_load
  then
    begin
      first_load := false;
      (* NOTE, XXX: here also loading, processing and validating all the
       * module's dependencies *)
      let piqi = Piqi.load_piqi fname in
      Piq.Piqi piqi
    end
  else
    raise Piq.EOF (* mimic the behaviour of Piq. loaders *)


let resolve_typename () =
  if !typename <> ""
  then Some (Piqi_convert.find_piqtype !typename)
  else None


(* ensuring that Piq.load_pb is called exactly one time *)
let load_pb piqtype protobuf :Piq.obj =
  if !first_load
  then
    begin
      first_load := false;
      Piq.load_pb piqtype protobuf
    end
  else
    (* XXX: print a warning if there are more input objects? *)
    raise Piq.EOF


let first_write_pb = ref true

let write_pb ch (obj: Piq.obj) =
  if !first_write_pb
  then
    begin
      first_write_pb := false;
      Piq.write_pb ch obj
    end
  else
    piqi_error "converting more than one object to \"pb\" is not allowed"


(* write only data and skip Piqi specifications and data hints *)
let write_data ~is_piqi_input writer ch (obj: Piq.obj) =
  match obj with
    | Piq.Piqi _ when not is_piqi_input ->
        (* ignore embedded Piqi specs if we are not converting .piqi *)
        ()
    | Piq.Piqtype _ ->
        (* ignore default type names *)
        ()
    | _ ->writer ch obj


(* write data and Piqi specifications and data hints *)
let write_data_and_piqi writer ch (obj: Piq.obj) =
  match obj with
    | Piq.Piqtype _ ->
        (* ignore default type names *)
        ()
    | _ -> writer ch obj


let make_reader load_f input_param =
  (fun () -> load_f input_param)


let make_reader input_encoding =
  match input_encoding with
    | "pb" when !typename = "" ->
        piqi_error "--type parameter must be specified for \"pb\" input encoding"
    | "pb" ->
        let piqtype = some_of (resolve_typename ()) in
        let protobuf = Piq.open_pb !ifile in
        make_reader (load_pb piqtype) protobuf

    | "json" ->
        let json_parser = Piqi_json.open_json !ifile in
        make_reader (Piq.load_json_obj (resolve_typename ())) json_parser

    | "xml" when !typename = "" ->
        piqi_error "--type parameter must be specified for \"xml\" input encoding"
    | "xml" ->
        let piqtype = some_of (resolve_typename ()) in
        let xml_parser = Piqi_xml.open_xml !ifile in
        make_reader (Piq.load_xml_obj piqtype) xml_parser

    | "piq" ->
        let piq_parser = Piq.open_piq !ifile in
        make_reader (Piq.load_piq_obj (resolve_typename ())) piq_parser

    | "piqi" when !typename <> "" ->
        piqi_error "--type parameter is not applicable to \"piqi\" input encoding"
    | "piqi" ->
        make_reader load_piqi !ifile

    | "pib" ->
        let buf = Piq.open_pib !ifile in
        make_reader (Piq.load_pib_obj (resolve_typename ())) buf
    | "" ->
        piqi_error "can't determine input encoding; use -f option to specify it explicitly"
    | x ->
        piqi_error ("unknown input encoding: " ^ U.quote x)


let make_writer ?(is_piqi_input=false) output_encoding =
  (* XXX: We need to resolve all defaults before converting to JSON or XML since
   * they are dynamic encoding, and it would be too unreliable and inefficient
   * to let a consumer decide what a default value for a field should be in case
   * if the field is missing. *)
  C.resolve_defaults := !flag_add_defaults ||
    (match output_encoding with
      | "json" | "xml" -> true
      | _ -> false);
  match output_encoding with
    | "" (* default output encoding is "piq" *)
    | "piq" -> Piq.write_piq
    | "pib" -> Piq.write_pib
    | "json" ->
        write_data_and_piqi Piq.write_json
    | "pb" ->
        write_data write_pb ~is_piqi_input
    | "xml" ->
        write_data Piq.write_xml ~is_piqi_input
    | x ->
        piqi_error ("unknown output encoding " ^ U.quote x)


let seen = ref [] (* the list of seen elements *)

let is_seen x = List.memq x !seen

let add_seen x = seen := x :: !seen

let check_update_unseen x =
  let is_unseen = not (is_seen x) in
  (* add unseen element to the list of seen ones *)
  if is_unseen then add_seen x;
  is_unseen (* return true for yet unseen elements *)

let remove_update_seen l =
  List.filter check_update_unseen l


let rec get_piqi_deps piqi ~only_imports =
  if Piqi.is_boot_piqi piqi
  then [] (* boot Piqi (a parent of built-in types) is not a dependency *)
  else
    (* get all includes and includes from all included modules *)
    let include_deps =
      if only_imports
      then []
      else
        (* remove the module itself from the list of included deps (it is always
         * at the end of the list) *)
        List.filter (fun x -> x != piqi) piqi.P#included_piqi
    in
    (* get all dependencies from imports *)
    let import_deps =
      U.flatmap (fun x ->
          let piqi = some_of x.T.Import#piqi in
          U.flatmap (get_piqi_deps ~only_imports) piqi.P#included_piqi)
        piqi.P#resolved_import
    in
    (* NOTE: includes go first in the list of dependencies *)
    let l = include_deps @ import_deps @ [piqi] in
    (* remove duplicate entries *)
    U.uniqq l


let get_parent_piqi (t: T.piqtype) =
  let typedef =
    match t with
      | #T.typedef as x -> x
      | _ -> assert false
  in
  C.get_parent_piqi typedef


let get_dependencies (obj :Piq.obj) ~only_imports =
  let deps =
    match obj with
      | Piq.Piqi piqi ->
          (* add piqi itself to the list of seen *)
          add_seen piqi;
          get_piqi_deps piqi ~only_imports
      | _ -> (
          let piqtype =
            match obj with
              | Piq.Piqtype name ->
                  Piqi_db.find_piqtype name
              | Piq.Typed_piqobj obj | Piq.Piqobj obj ->
                  Piqobj_common.type_of obj
              | _ -> assert false
          in
          let piqi = get_parent_piqi piqtype in
          (* get dependencies for yet unseen (and not yet embedded) piqi *)
          if is_seen piqi
          then []
          else get_piqi_deps piqi ~only_imports
      )
  in
  (* filter out already seen deps along with updating the list of seen deps *)
  remove_update_seen deps


let validate_options input_encoding =
  let typename_str =
    if !typename = ""
    then ""
    else "values of type " ^ U.quote !typename ^ " "
  in
  let output_encoding_str =
    if !output_encoding = ""
    then "piq" (* default output encoding is "piq" *)
    else !output_encoding
  in
  trace "converting %sfrom .%s to .%s\n" typename_str input_encoding output_encoding_str;

  if !flag_embed_piqi
  then (
    match !output_encoding with
      | "pb" | "xml" ->
          if input_encoding = "piqi"
          then
            piqi_error "can't --embed-piqi when converting .piqi to .pb or .xml"
          else
            piqi_warning "--embed-piqi doesn't have any effect when converting to .pb or .xml"
      | _ -> ()
  )


let do_convert ?writer ?(is_piq_output=false) reader =
  (* read next object to the input channel *)
  let read_obj () =
    try
      let obj = reader () in
      Some obj
    with
      Piq.EOF -> None
  in
  (* write the object to the output channel *)
  let do_write_obj obj =
    match writer with
      | None -> ()
      | Some f -> f obj
  in
  (* write the object to the output, possibly including its Piqi dependencies *)
  let write_obj obj =
    (* write object's Piqi dependencies *)
    if !flag_embed_piqi
    then (
      (* write yet unwirtten object's dependencies *)
      let deps = get_dependencies obj ~only_imports:(not is_piq_output) in
      List.iter (fun x ->
        trace "piqi convert: embedding dependency module %s\n" (some_of x.P#modname);
        do_write_obj (Piq.Piqi x)
      ) deps
    );
    (* finally, write the object itself *)
    do_write_obj obj
  in
  let process_and_write_piqi piqi_list =
    List.iter (fun modname ->
      let piqi = Piqi_db.find_piqi modname in
      (* process piqi if it hasn't been fully processed yet by this point *)
      let piqi = Piqi.process_piqi piqi ~cache:false in
      Piqloc.preserve ();
      (* finally, write it to the output channel *)
      trace "piqi convert: writing module %s\n" modname;
      write_obj (Piq.Piqi piqi)
    )
    (List.rev piqi_list)
  in
  let rec aux piqi_list =
    (* resetting source location tracking back to "enabled" state; we don't
     * carefully call matching Piqloc.resume () for every Piqloc.pause () if we
     * get exceptions in between *)
    Piqloc.is_paused := 0;
    match read_obj () with
      | None ->
          (* flush all yet unwritten piqi modules at EOF *)
          process_and_write_piqi piqi_list
      | Some obj ->
          let piqi_list =
            match obj with
              | Piq.Piqi piqi ->
                  Piqi_db.add_piqi piqi;
                  (* Preserve location information so that exising location info for
                   * Piqi modules won't be discarded by subsequent Piqloc.reset()
                   * calls. *)
                  Piqloc.preserve ();
                  if Piqi.is_processed piqi
                  then
                    (* if it has been processed, ready to write it right away *)
                    (write_obj obj; [])
                  else
                    let modname = some_of piqi.P#modname in
                    modname :: piqi_list (* add to the list of unprocessed modules *)
              | _ ->
                  (* reset location db to allow GC collect previously read
                   * objects *)
                  Piqloc.reset ();
                  (* once we read a non-piqi object, fully process and flush all
                   * yet unwrittent piqi modules *)
                  process_and_write_piqi piqi_list;
                  (* finally write the object we've just read *)
                  write_obj obj;
                  (* return empty list as a new value of piqi_list *)
                  []
          in
          aux piqi_list
  in
  aux []


let convert_file () =
  Piqi_convert.init ();
  Piqi_convert.set_options
    (Piqi_convert.make_options
      ~json_omit_null_fields:!flag_json_omit_null_fields
      ~use_strict_parsing:!Piqi_config.flag_strict
      ()
    );
  let input_encoding =
    if !input_encoding <> ""
    then !input_encoding
    else Piqi_file.get_extension !ifile
  in
  validate_options input_encoding;

  let reader = make_reader input_encoding in
  let is_piqi_input = (input_encoding = "piqi" || !typename = "piqi") in
  let writer = make_writer !output_encoding ~is_piqi_input in
  (* open output file *)
  let ofile =
    match !ofile with
      | "" when !output_encoding = "" -> "" (* print "piq" to stdout by default *)
      | "" when !ifile <> "" && !ifile <> "-" ->
          let output_extension = !output_encoding in
          !ifile ^ "." ^ output_extension
      | x -> x
  in
  let och = Main.open_output ofile in

  let is_piq_output =
    match !output_encoding with
      | "" | "piq" -> true
      | _ -> false
  in
  (* main convert cycle *)
  trace "piqi convert: main loop\n";
  do_convert reader ~writer:(writer och) ~is_piq_output


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  convert_file ()

 
let _ =
  Main.register_command run "convert"
    "convert data files between various encodings (pb, json, xml, piq, pib)"

