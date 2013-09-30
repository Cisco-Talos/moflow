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
 * Piq interface compiler for OCaml (extended interfaces for multi-format
 * JSON/XML/Protobuf/Piq serialization).
 *)

module C = Piqi_common
open C
open Iolist


let typedef_mlname = Piqic_ocaml_types.typedef_mlname


let gen_init_piqi ocaml_mod =
  (* init embedded piqi spec *)
  iol [
    ios "let piqi = "; ios ocaml_mod; ios ".piqi";
    eol;eol;
    ios "let _ = Piqirun_ext.init_piqi piqi";
    eol;eol;
  ]


let gen_init_piqtype def =
  let mlname = typedef_mlname def in
  let scoped_name = C.full_piqi_typename (def :> T.piqtype) in
  iod " " [
    ios "let _" ^^ ios mlname ^^ ios "_piqtype =";
      ios "Piqirun_ext.find_piqtype"; ioq scoped_name;
    eol;
  ]


let gen_convert mlname input_format output_format data =
  let piqtype = "_" ^ mlname ^ "_piqtype" in
  iod " " [
    ios "Piqirun_ext.convert";
      ios piqtype; ios input_format; ios output_format; ios data;
  ]


let gen_parse ocaml_mod def =
  let mlname = typedef_mlname def in
  iod " " [
    ios "let parse_" ^^ ios mlname;
        ios "?opts"; ios "x (format :Piqirun_ext.input_format) =";
      ios "let x_pb ="; gen_convert mlname "format" "`pb" "x"; ios "?opts";
      ios "in";
      ios "let buf = Piqirun.init_from_string x_pb";
      ios "in";
      ios ocaml_mod ^^ ios ".parse_" ^^ ios mlname; ios "buf";
      eol;
  ]


let gen_gen ocaml_mod def =
  let mlname = typedef_mlname def in
  iod " " [
    ios "let gen_" ^^ ios mlname;
        ios "?opts"; ios "x (format :Piqirun_ext.output_format) =";
      ios "let buf = "; ios ocaml_mod ^^ ios ".gen_" ^^ ios mlname; ios "x";
      ios "in";
      ios "let x_pb = Piqirun.to_string buf";
      ios "in";
      gen_convert mlname "`pb" "format" "x_pb"; ios "?opts";
      eol;
  ]


let gen_print def =
  let mlname = typedef_mlname def in
  iod " " [
    ios "let print_" ^^ ios mlname; ios "x =";
      ios "Pervasives.print_endline (gen_" ^^ ios mlname; ios "x `piq)";
    eol;
    ios "let prerr_" ^^ ios mlname; ios "x =";
      ios "Pervasives.prerr_endline (gen_" ^^ ios mlname; ios "x `piq)";
    eol;
  ]


let gen_code piqi =
  let ocaml_mod = some_of piqi.P#ocaml_module in
  let defs = piqi.P#resolved_typedef in

  let type_initializers = List.map gen_init_piqtype defs in
  let parsers = List.map (gen_parse ocaml_mod) defs in
  let generators = List.map (gen_gen ocaml_mod) defs in
  let printers = List.map gen_print defs in

  iol [
    gen_init_piqi ocaml_mod; eol; eol;
    iol type_initializers; eol; eol;
    iol parsers; eol;eol;
    iol generators; eol;eol;
    iol printers; eol;eol;
  ]


module Main = Piqi_main
open Main


let piqic_ext piqi =
  (* chdir to the output directory *)
  Main.chdir_output !odir;

  let code = gen_code piqi in

  let ofile =
    let modname = some_of piqi.P#ocaml_module in
    String.uncapitalize modname ^ "_ext.ml"
  in
  Piqic_ocaml.gen_output_file ofile code


let piqic_file ifile =
  Piqic_ocaml.init ();

  (* load input .piqi file *)
  let piqi = Piqi.load_piqi ifile in

  (* always generate embedded Piqi specification *)
  Piqic_common.flag_embed_piqi := true;

  Piqic_ocaml.piqic piqi;
  piqic_ext piqi


let usage = "Usage: piqic ocaml-ext [options] <.piqi file>\nOptions:"


let speclist = Main.common_speclist @ Piqic_ocaml.common_speclist


let run () =
  Main.parse_args () ~usage ~speclist;
  piqic_file !ifile

 
let _ =
  Main.register_command run "ocaml-ext" "generate extended OCaml stubs (JSON/XML/Piq/Pb) from %.piqi"

