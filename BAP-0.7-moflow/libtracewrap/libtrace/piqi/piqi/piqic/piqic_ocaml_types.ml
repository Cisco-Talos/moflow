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
 * generation of Ocaml type declarations
 *)

module C = Piqi_common
open C
open Iolist


(* piqi compiler-compiler mode indictation *)
(* TODO: move to piqic configuration *)
let cc_mode = ref false


let gen_cc s =
  if !cc_mode
  then ios s
  else iol []


(*
let gen_cc_cond a b =
  if !cc_mode
  then a
  else b
*)


(* toplevel ocaml modname for the module which is currently being compiled *)
(* TODO: this is a dirty method for sharing the setting across all
 * piqic_ocaml_* modules *)
let top_modname = ref ""


let scoped_name name = !top_modname ^ "." ^ name


let typedef_mlname = function
  | `record t -> some_of t.R#ocaml_name
  | `variant t -> some_of t.V#ocaml_name
  | `enum t -> some_of t.E#ocaml_name
  | `alias t -> some_of t.A#ocaml_name
  | `list t -> some_of t.L#ocaml_name
  | _ ->
      (* this function will be called only for named types (i.e. typedefs) *)
      assert false


let capitalize = String.capitalize


let gen_deftype parent ocaml_name =
  let ocaml_name = some_of ocaml_name in
  match parent with
    | Some (`import x) -> (* imported name *)
        let ocaml_modname = some_of x.Import#ocaml_name in
        (ocaml_modname ^ "." ^ ocaml_name)
    | _ -> (* local name *)
        scoped_name ocaml_name


(* XXX: check type compatibility *)
let rec gen_piqtype t ocaml_type = 
  match ocaml_type with
    | Some x -> x
    | None ->
        match t with
          | `int -> "int"
          | `float -> "float"
          | `bool -> "bool"
          | `string | `binary -> "string"
          | `any ->
              if !Piqic_common.is_self_spec
              then scoped_name "any"
              else "Piqi_piqi.any"
          | `record r -> gen_deftype r.R#parent r.R#ocaml_name
          | `variant v -> gen_deftype v.V#parent v.V#ocaml_name
          | `enum e -> gen_deftype e.E#parent e.E#ocaml_name
          | `list l -> gen_deftype l.L#parent l.L#ocaml_name
          | `alias a -> gen_aliastype a


and gen_aliastype a =
  let open Alias in
  let ocaml_name = some_of a.ocaml_name in
  let typename = gen_piqtype (some_of a.piqtype) a.ocaml_type in
  if ocaml_name = typename
  then ocaml_name (* don't generate aliases for built-in types *)
  else gen_deftype a.parent a.ocaml_name


let ios_gen_piqtype ?ocaml_type (t :T.piqtype) =
  ios (gen_piqtype t ocaml_type)


let gen_field_type f =
  let open F in
  match f.piqtype with
    | None -> ios "bool"; (* flags are represented as booleans *)
    | Some t ->
      let deftype = ios_gen_piqtype t in
      match f.mode with
        | `required -> deftype
        | `optional when f.default <> None && (not f.ocaml_optional) ->
            deftype (* optional + default *)
        | `optional -> deftype ^^ ios " option"
        | `repeated ->
            deftype ^^
            if f.ocaml_array
            then ios " array"
            else ios " list"


let mlname_of name piqtype =
  match name, piqtype with
    | Some n, _ -> n
    | None, Some t -> typedef_mlname t
    | _ -> assert false


(* XXX: move this functionality to mlname_*. mlname assignment should be done
 * once rahter than calling it from every place where it is needed *)
let mlname_of_field f =
  let open F in mlname_of f.ocaml_name f.piqtype


let mlname_of_option o =
  let open O in mlname_of o.ocaml_name o.piqtype


let gen_field f = 
  let open F in
  let fdef = iod " " (* field definition *)
    [
      ios "mutable"; (* defining all fields as mutable at the moment *)
      ios (mlname_of_field f);
      ios ":";
      gen_field_type f;
      ios ";";
    ]
  in fdef


(* generate record type in record module; see also gen_record' *)
let gen_record_mod r =
  let modname = capitalize (some_of r.R#ocaml_name) in
  let fields = r.Record#field in
  let fdefs = (* field definition list *)
    if fields <> []
    then iol (List.map gen_field fields)
    else ios "_dummy: unit"
  in
  let rcons = (* record def constructor *)
    iol [ios "type t = "; ios "{"; fdefs; ios "}"]
  in
  let rdef = iod " "
    [
      ios modname; (* module declaration *)
      ios ":";
        ios "sig"; (* signature *) 
        rcons;
        ios "end";
      ios "=";
        ios modname;
        (* full version:
        ios "struct"; (* structure *)
        rcons;
        ios "end";
        *)
    ]
  in rdef


let gen_pvar_name name = 
  ios "`" ^^ ios name


let is_local_def def =
  match get_parent def with
    | `piqi _ -> true
    | `import _ -> false


let gen_option o =
  let open Option in
  match o.ocaml_name, o.piqtype with
    | None, Some ((`variant v) as def) ->
        (* NOTE: for some reason, ocaml complains about fully qualified
         * polymorphic variants in recursive modules, so we need to use
         * non-qualified names in this case *)
        if is_local_def def
        then ios (some_of v.V#ocaml_name)
        else ios_gen_piqtype def
    | None, Some ((`enum e) as def) ->
        if is_local_def def
        then ios (some_of e.E#ocaml_name)
        else ios_gen_piqtype def
    | _, Some t ->
        let n = gen_pvar_name (mlname_of_option o) in
        n ^^ ios " of " ^^ ios_gen_piqtype t
    | Some mln, None -> gen_pvar_name mln
    | None, None -> assert false


let gen_alias a =
  let open Alias in
  iol [
    ios (some_of a.ocaml_name); ios " = ";
      ios_gen_piqtype (some_of a.piqtype) ?ocaml_type:a.ocaml_type ]


let gen_list l =
  let open L in
  iol [
    ios (some_of l.ocaml_name); ios " = ";
      ios_gen_piqtype (some_of l.piqtype);
      if l.ocaml_array
      then ios " array"
      else ios " list";
  ]


let gen_options options =
  let var_defs =
    iod "|" (List.map gen_option options)
  in
  iol [ios "["; var_defs; ios "]"]


let gen_variant v =
  let open Variant in
  iol [
    ios (some_of v.ocaml_name);
    ios " = ";
    gen_options v.option;
  ]


let gen_enum e =
  let open Enum in
  iol [
    ios (some_of e.ocaml_name);
    ios " = ";
    gen_options e.option;
  ]


let gen_record r =
  let name = some_of r.Record#ocaml_name in
  let modname = capitalize name in
  iol [ ios name; ios " = "; ios (modname ^ ".t") ]


let gen_def = function
  | `record t -> gen_record t
  | `variant t -> gen_variant t
  | `enum t -> gen_enum t
  | `list t -> gen_list t
  | _ -> assert false


let gen_alias a =
  let open Alias in
  let name = some_of a.ocaml_name in
  let typename = gen_piqtype (some_of a.piqtype) a.ocaml_type in
  if name = typename
  then [] (* don't generate cyclic type abbreviation *)
  else [ gen_alias a ]


let gen_def = function (* gen everything except records *)
  | `alias t -> gen_alias t
  | t -> [gen_def t]


let gen_mod_def = function
  | `record r -> [gen_record_mod r]
  (* XXX: generate modules for variants? *)
  | _ -> []


let gen_defs (defs:T.typedef list) =
  let mod_defs = U.flatmap gen_mod_def defs in
  let odefs = U.flatmap gen_def defs in
  let odef =
    let odef =
      if odefs = []
      then iol []
      else iol [
        ios "type ";
        iod " type " odefs;
      ]
    in
    iod " "
    [
      ios !top_modname; (* module declaration *)
      ios ":";
        ios "sig";  (* signature *) 
        odef;
        ios "end";
      ios "=";
        ios !top_modname;
        (* full version:
        ios "struct"; (* structure *)
        odef;
        ios "end";
        *)
    ]
  in
  let defs = [odef] @ mod_defs in
  let code = iol
    [
      ios "module rec ";
      iod " and " defs;
    ]
  in
  iod " " [
    code;
    ios "include"; ios !top_modname;
    eol;
  ]


let gen_import x =
  let open Import in
  let piqi = some_of x.piqi in
  iod " " [
    ios "module"; ios (some_of x.ocaml_name); ios "=";
        ios (some_of piqi.P#ocaml_module);
    eol;
  ]


let gen_imports l =
  let l = List.map gen_import l in
  iol l


(* NOTE: for some reason, ocaml complains about fully qualified polymorphic
 * variants in recursive modules, so instead of relying on OCaml, we need to
 * preorder variants ourselves without relying on OCaml to figure out the order
 * automatically *)
let order_variant_defs variants =
  (* topologically sort local variant defintions *)
  let cycle_visit def =
    Piqi_common.error def
      ("cyclic OCaml variant definition: " ^ typedef_name def)
  in
  let get_adjacent_vertixes = function
    | `variant v ->
        (* get the list of included variants *)
        U.flatmap (fun o ->
          let open O in
          match o.ocaml_name, o.piqtype with
            | None, Some ((`variant _) as def)
            | None, Some ((`enum _) as def) ->
                if is_local_def def (* omit any imported definitions *)
                then [def]
                else []
            | _ -> []
        ) v.V#option
    | _ -> []
  in
  Piqi_graph.tsort variants get_adjacent_vertixes ~cycle_visit


(* make sure we define aliases for built-in ocaml types first; some aliases
 * (e.g. float) can override the default OCaml type names which results in
 * cyclic type definitions without such ordering *)
let order_alias_defs alias_defs =
  let rank def =
    match def with
      | `alias x ->
          if C.is_builtin_def def
          then
            (* aliases of built-in OCaml types go first *)
            if x.A#ocaml_type <> None then 1 else 2
          else 100
      | _ ->
          assert false
  in
  let compare_alias_defs a b =
    rank a - rank b
  in
  List.stable_sort compare_alias_defs alias_defs


let order_defs defs =
  (* we apply this specific ordering only to variants, to be more specific --
   * only to those variants that include other variants by not specifying tags
   * for the options *)
  let variants, rest =
    List.partition (function
      | `variant _ | `enum _ -> true
      | _ -> false)
    defs
  in
  let aliases, rest =
    List.partition (function
      | `alias _ -> true
      | _ -> false)
    rest
  in
  (* return the updated list of definitions with sorted variants and aliases *)
  (order_alias_defs aliases) @ (order_variant_defs variants) @ rest


let gen_piqi (piqi:T.piqi) =
  iol [
    gen_imports piqi.P#resolved_import;
    gen_defs (order_defs piqi.P#resolved_typedef);
  ]

