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
 * piq interface compiler compiler
 *)


module C = Piqi_common
open Piqi_common
open Iolist


(* assign field/option/enum codes as a hash(name); by default serial wire codes
 * would be used *)
let add_hashcodes defs =
  (* add hash-based field and option codes instead of auto-enumerated ones *)
  Piqi_protobuf.add_hashcodes defs;
  (* check for hash conflicts and pre-order fields by hash codes *)
  Piqi_protobuf.process_defs defs;
  ()


(* piq interface compiler compile *)
let piqicc ch spec_fname lang_fname impl_fname =
  trace "piqicc(0)\n";

  trace "piqicc: loading piqi-spec from: %s\n" spec_fname;
  let piqi_spec = Piqi.load_piqi spec_fname in

  trace "piqicc: loading piqi-lang from: %s\n" lang_fname;
  let piqi_lang = Piqi.load_piqi lang_fname in

  trace "piqicc: loading piqi-impl from: %s\n" impl_fname;
  let piqi_impl = Piqi.load_piqi impl_fname in

  trace "piqicc: piqi compiling compiler\n";
  (* TODO: check & report piqi incompatibility *)

  (* find the Piqi module that corresponds to the Piqi language
   * self-specification -- a basic input sanity check *)
  let _ =
    try Piqi_db.find_piqi "piqi/piqi-lang"
    with Not_found ->
      piqi_error "missing module piqi/piqi-lang"
  in
  let _ =
    try Piqi_db.find_piqi "piqi/piqi"
    with Not_found ->
      piqi_error "missing imported module piqi/piqi"
  in
  (* convert piqi to binobj using piqi self-definition that we've just loaded *)
  let piqi_def = Piqi_db.find_local_typedef piqi_lang.P#resolved_typedef "piqi" in
  let piqi_type = (piqi_def: T.typedef :> T.piqtype) in
  let gen_piqi_binobj piqi =
    add_hashcodes piqi.P#typedef;
    (* Old method that does not account for possible Piqi lang
     * self-specification refactoring
     *
     Piqirun.gen_binobj T.gen__piqi piqi
     *)
    Piqloc.pause ();
    let piqi_ast = Piqi.piqi_to_ast piqi in

    (* useful for debugging:
    Piqi_pp.prettyprint_piqi stdout piqi;
    *)

    (* discard unknown fields -- they don't matter because we are transforming the
     * spec that has been validated already *)
    let piqobj =
      U.with_bool C.is_inside_parse_piqi true
      (fun () -> Piqobj_of_piq.parse_obj piqi_type piqi_ast)
    in

    ignore (Piqobj_of_piq.get_unknown_fields ());

    let res =
      U.with_bool Piqobj_to_protobuf.is_external_mode true
      (fun () -> Piqobj_to_protobuf.gen_binobj piqobj)
    in
    Piqloc.resume ();
    res
  in
  let expand_piqi piqi =
    Piqi.expand_piqi ~extensions:true ~functions:false piqi
  in
  (* prepare embedded Piqi language spec *)
  let piqi_lang = P#{
    (expand_piqi piqi_lang) with
      modname = piqi_lang.P#modname;
      ocaml_module = None; (* XXX *)
  }
  in
  (* prepare embedded Piqi self-specification *)
  let piqi_spec = P#{
    (expand_piqi piqi_spec) with
      modname = piqi_spec.P#modname;
      ocaml_module = None; (* XXX *)
      custom_field = []; (* XXX *)
  }
  in
  trace "gen_piqi_binobj piqi_lang\n";
  let piqi_lang_binobj = gen_piqi_binobj piqi_lang in
  trace "gen_piqi_binobj piqi_spec\n";
  let piqi_spec_binobj = gen_piqi_binobj piqi_spec in

  let code = iod " " [
    ios "let parse_piqi_binobj x = ";
      ios "Piqirun.parse_binobj parse_piqi x";
    eol;

    ios "let piqi_lang = ";
      ios "let piqi_lang_binobj = "; ioq (String.escaped piqi_lang_binobj);
      ios "in parse_piqi_binobj piqi_lang_binobj";
    eol;

    ios "let piqi_spec = ";
      ios "let piqi_spec_binobj = "; ioq (String.escaped piqi_spec_binobj);
      ios "in parse_piqi_binobj piqi_spec_binobj";
    eol;
  ]
  in
  (* call piq interface compiler for ocaml *)
  (* TODO: move it to Piqic_config module *)
  Piqic_ocaml_types.cc_mode := true;
  (* generate default values for generated OCaml types *)
  Piqic_common.flag_gen_defaults := true;
  (* Override supplied module name *)
  piqi_impl.P#ocaml_module <- Some "Piqtype";
  let code = iol [
    Piqic_ocaml.gen_ocaml_code piqi_impl;
    code;
  ]
  in
  Iolist.to_channel ch code;


module Main = Piqi_main
open Main


(* command-line options *)
let piqi_spec_file = ref ""
let piqi_lang_file = ref ""
let piqi_impl_file = ref ""


let usage = "Usage: piqicc --piqi ... --impl ...\nOptions:"


let speclist = Main.common_speclist @
  [
    arg_o;
    (* XXX: arg_C; *)
    "--spec", Arg.Set_string piqi_spec_file,
      "<.piqi file> specify the Piqi self-spec";
    "--lang", Arg.Set_string piqi_lang_file,
      "<.piqi file> specify the Piqi language spec";
    "--impl", Arg.Set_string piqi_impl_file,
      "<.piqi file> specify spec for internal representation";
  ]


let piqicc_file () =
  let error s =
    Printf.eprintf "Error: %s\n\n" s;
    Arg.usage speclist usage;
    die ""
  in
  if !piqi_spec_file = "" then error "'--spec' parameter is missing";
  if !piqi_lang_file = "" then error "'--lang' parameter is missing";
  if !piqi_impl_file = "" then error "'--impl' parameter is missing";

  let ch = Main.open_output !ofile in
  piqicc ch !piqi_spec_file !piqi_lang_file !piqi_impl_file


let run () =
  Main.parse_args () ~usage ~speclist ~min_arg_count:0 ~max_arg_count:0;
  piqicc_file ()

 
let _ =
  Main.register run "piqi compiler compiler"

