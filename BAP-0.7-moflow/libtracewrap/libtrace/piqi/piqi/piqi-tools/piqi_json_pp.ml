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


open Piqi_common


module Main = Piqi_main
open Main


let usage = "Usage: piqi json-pp [options] [<.json file>] [output file]\nOptions:"


let flag_indent = ref false

let arg__indent =
    "--indent", Arg.Set flag_indent,
    "use simple indentation instead of pretty-printing"


let speclist = Main.common_speclist @
  [
    arg__indent;
    arg_o;
    arg__;
  ]


let prettyprint_json ch json =
  Piqi_json_gen.pretty_to_channel ch json ~indent:!flag_indent


let prettyprint_json ch json_parser =
  let rec aux () =
    match Piqi_json.read_json_obj json_parser with
      | None -> ()
      | Some json ->
          (* reset location db to allow GC to collect previously read objects *)
          Piqloc.reset ();
          prettyprint_json ch json;
          output_string ch "\n\n";
          aux ()
  in aux ()


let prettyprint_file filename =
  let ch = Main.open_output !ofile in
  (* switch parser/generator to pretty-print mode *)
  Config.pp_mode := true;
  let json_parser = Piqi_json.open_json filename in
  prettyprint_json ch json_parser


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  prettyprint_file !ifile

 
let _ =
  Main.register_command run "json-pp" "pretty-print %.json"

