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

(* compiler compiler: convert an (extended) self-spec into one of portable
 * formats *)


module C = Piqi_common  
open C


(* command-line arguments *)
let output_format = ref ""


(* NOTE: "piqi cc" command-line interface should be idealy as stable
 * (backward-compatible) as as the Piqi self-spec; this way users can rely on
 * the tool without closely tracking piqi releases *)
let usage = "Usage: piqi cc [options] [<.piqi file>]\nOptions:"


let arg__t =
  "-t", Arg.Set_string output_format,
  "pb|json|xml|piqi output format (default=piqi)"


let speclist = Piqi_main.common_speclist @
  [
    Piqi_main.arg__strict;
    Piqi_main.arg_o;
    arg__t;
    Piqi_main.arg__include_extension;
  ]


let load_piqi ifile =
  let piqi = Piqi.load_piqi ifile in
  (* perform some checks
   *
   * TODO: it would be useful to have more more checks verifying that this is
   * indeed an extended piqi spec and not some arbitrary module *)
  if C.is_self_spec piqi
  then (
    (* find the "piqi" typedef from the module we've just loaded *)
    let piqi_def =
      try Piqi_db.find_local_typedef piqi.P#resolved_typedef "piqi"
      with Not_found ->
        Printf.eprintf "error: invalid self-spec read from %s: no definition named \"piqi\"\n" ifile;
        piqi_error "piqi cc: invalid self-spec"
    in
    ignore (piqi_def: T.typedef :> T.piqtype)
  )
  else (
    Printf.eprintf "error: piqi module read from %s is not a self-spec" ifile;
    piqi_error "piqi cc: invalid self-spec"
  );
  piqi


let run_cc ifile och =
  let piqi =
    if ifile <> ""
    then
      load_piqi ifile
    else (
      trace "input file is missing; printing the default embedded self-spec\n";
      C.some_of !Piqi.piqi_spec
    )
  in
  let piqi = {piqi with P#modname = Some "piqi"} in
  let obj = Piq.Piqi piqi in
  match !output_format with
    | "json" ->
        Piq.write_json och obj
    | "pb" ->
        Piq.write_pb och obj
    | "xml" ->
        Piq.write_xml och obj
    | "piqi" | "" ->
        let piqi = Piqi.lang_to_spec piqi in
        let piqi = {piqi with P#modname = Some "piqi"} in
        Piqi_pp.prettyprint_piqi och piqi
    | x ->
        piqi_error ("unknown output format " ^ U.quote x)


let run () =
  Piqi_main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:1;

  Piqi_json.init (); (* we need it for converting to JSON *)

  (* always generate extended piqi any; the goal is to standardise on the
   * representation, make command-line interface simpler and don't give users
   * unnecessary choices *)
  Piqi_config.gen_extended_piqi_any := true;

  let och = Piqi_main.open_output !Piqi_main.ofile in
  run_cc !Piqi_main.ifile och

 
let _ =
  Piqi_main.register_command run "cc" "compiler compiler: convert an (extended) self-spec into one of portable formats"

