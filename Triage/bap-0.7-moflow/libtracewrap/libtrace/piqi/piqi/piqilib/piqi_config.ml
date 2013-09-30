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


(* .piqi search paths *)
let paths = ref []


let add_path x =
  paths := !paths @ [x]


(* TODO, XXX: what about Windows? Is ':' a valid separator there? *)
let piqi_path =
  try
    let s = Sys.getenv "PIQI_PATH" in
    let l = Piqi_util.string_split s ':' in
    List.filter (fun s -> s <> "") l (* remove empty segments *)
  with Not_found -> []


(* set .piqi search path to contain CWD and $PIQI_PATH *)
let init_paths () =
  paths := "." :: piqi_path


let reset_paths () =
  paths := []


(*
 * command-line options 
 *)


(* don't include built-in type definitions into piqi specifications that are
 * being processed *)
let flag_no_builtin_types = ref false


let flag_strict = ref false
let flag_no_warnings = ref false
let debug_level = ref 0
let flag_trace =
  try 
    ignore (Sys.getenv "PIQI_TRACE");
    ref true
  with Not_found ->
    ref false


(* this variable controls whether we parse and generate piq AST
 * for/during pretty-printing or for real use *)
let pp_mode = ref false


(* Piqi extensions automatically included when loading modules *)
let extensions = ref []

let add_include_extension (name :string) =
  extensions := !extensions @ [ name ]


(* for JSON and XML output: whether to generate piqi-any values using symbolic
 * JSON and/or XML representation (this is the default) or use full piqi-any
 * representation that wraps JSON or XML symbolic representation in a record
 * that includes the value itself, plus protobuf representation of the value,
 * typename and possibly something else *)
let gen_extended_piqi_any = ref false

