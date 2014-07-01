module rec Piqtype :
             sig
               type uint = int
               
               type uint32 = int32
               
               type uint64 = int64
               
               type float64 = float
               
               type float32 = float
               
               type protobuf_int32 = int32
               
               type protobuf_int64 = int64
               
               type binary = string
               
               type piqi_any = Piqtype.any
               
               type int32_fixed = int32
               
               type uint32_fixed = Piqtype.uint32
               
               type int64_fixed = int64
               
               type uint64_fixed = Piqtype.uint64
               
               type float = Piqtype.float64
               
               type word = string
               
               type name = Piqtype.word
               
               type typename = Piqtype.name
               
               type piq_ast = Piq_ast.ast
               
               type piq_format = [ | `word | `text ]
               
               type protobuf_wire_type =
                 [
                   | `varint
                   | `zigzag_varint
                   | `fixed32
                   | `fixed64
                   | `signed_varint
                   | `signed_fixed32
                   | `signed_fixed64
                   | `block
                 ]
               
               type typedef =
                 [
                   | `record of Piqtype.record
                   | `variant of Piqtype.variant
                   | `enum of Piqtype.enum
                   | `alias of Piqtype.alias
                   | `list of Piqtype.piqi_list
                 ]
               
               type piqi_type =
                 [ | `int | `float | `bool | `string | `binary | `any
                 ]
               
               type field_mode = [ | `required | `optional | `repeated ]
               
               type function_param =
                 [
                   | `name of Piqtype.name
                   | `record of Piqtype.record
                   | `variant of Piqtype.variant
                   | `enum of Piqtype.enum
                   | `list of Piqtype.piqi_list
                   | `alias of Piqtype.alias
                 ]
               
               type extend_target =
                 [
                   | `typedef of Piqtype.name
                   | `name of Piqtype.name
                   | `field of Piqtype.name
                   | `option of Piqtype.name
                   | `import of Piqtype.name
                   | `func of Piqtype.name
                 ]
               
               type namespace =
                 [ | `piqi of Piqtype.piqi | `import of Piqtype.import
                 ]
               
               type piqtype = [ | typedef | piqi_type ]
               
               type record = Record.t
               
               type field = Field.t
               
               type variant = Variant.t
               
               type option = Option.t
               
               type enum = Enum.t
               
               type alias = Alias.t
               
               type piqi_list = Piqi_list.t
               
               type piqi = Piqi.t
               
               type import = Import.t
               
               type any = Any.t
               
               type func = Func.t
               
               type piqi_bundle = Piqi_bundle.t
               
               type includ = Includ.t
               
               type extend = Extend.t
               
               type pib_typehint = Pib_typehint.t
               
             end = Piqtype
and
  Record :
    sig
      type t =
        { mutable name : Piqtype.name option;
          mutable field : Piqtype.field list;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option;
          mutable parent : Piqtype.namespace option;
          mutable wire_field : Piqtype.field list;
          mutable is_func_param : bool;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option
        }
      
    end = Record
and
  Field :
    sig
      type t =
        { mutable name : Piqtype.name option;
          mutable typename : Piqtype.typename option;
          mutable mode : Piqtype.field_mode;
          mutable default : Piqtype.piqi_any option;
          mutable deprecated : bool;
          mutable piq_format : Piqtype.piq_format option;
          mutable protobuf_name : string option; mutable code : int32 option;
          mutable protobuf_packed : bool; mutable json_name : string option;
          mutable getopt_letter : Piqtype.word option;
          mutable getopt_doc : string option;
          mutable proto_name : string option; mutable wire_packed : bool;
          mutable piqtype : Piqtype.piqtype option;
          mutable alt_name : Piqtype.word option;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option; mutable ocaml_array : bool;
          mutable ocaml_optional : bool
        }
      
    end = Field
and
  Variant :
    sig
      type t =
        { mutable name : Piqtype.name option;
          mutable option : Piqtype.option list;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option;
          mutable parent : Piqtype.namespace option;
          mutable is_func_param : bool;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option
        }
      
    end = Variant
and
  Option :
    sig
      type t =
        { mutable name : Piqtype.name option;
          mutable typename : Piqtype.typename option;
          mutable deprecated : bool;
          mutable piq_format : Piqtype.piq_format option;
          mutable protobuf_name : string option; mutable code : int32 option;
          mutable json_name : string option;
          mutable getopt_letter : Piqtype.word option;
          mutable getopt_doc : string option;
          mutable proto_name : string option;
          mutable piqtype : Piqtype.piqtype option;
          mutable alt_name : Piqtype.word option;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option
        }
      
    end = Option
and
  Enum :
    sig
      type t =
        { mutable name : Piqtype.name option;
          mutable option : Piqtype.option list;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable protobuf_prefix : string option;
          mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option;
          mutable parent : Piqtype.namespace option;
          mutable is_func_param : bool;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option
        }
      
    end = Enum
and
  Alias :
    sig
      type t =
        { mutable name : Piqtype.name option;
          mutable typename : Piqtype.typename option;
          mutable piqi_type : Piqtype.piqi_type option;
          mutable piq_format : Piqtype.piq_format option;
          mutable protobuf_name : string option;
          mutable protobuf_type : string option;
          mutable protobuf_wire_type : Piqtype.protobuf_wire_type option;
          mutable json_name : string option;
          mutable proto_name : string option;
          mutable parent : Piqtype.namespace option;
          mutable is_func_param : bool;
          mutable piqtype : Piqtype.piqtype option;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option;
          mutable ocaml_type : string option
        }
      
    end = Alias
and
  Piqi_list :
    sig
      type t =
        { mutable name : Piqtype.name option;
          mutable typename : Piqtype.typename;
          mutable piq_format : Piqtype.piq_format option;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable protobuf_packed : bool; mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option; mutable wire_packed : bool;
          mutable parent : Piqtype.namespace option;
          mutable is_func_param : bool;
          mutable piqtype : Piqtype.piqtype option;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option; mutable ocaml_array : bool
        }
      
    end = Piqi_list
and
  Piqi :
    sig
      type t =
        { mutable modname : Piqtype.word option;
          mutable typedef : Piqtype.typedef list;
          mutable import : Piqtype.import list;
          mutable func : Piqtype.func list;
          mutable protobuf_custom : string list;
          mutable protobuf_package : string option;
          mutable custom_field : Piqtype.word list;
          mutable includ : Piqtype.includ list;
          mutable extend : Piqtype.extend list;
          mutable proto_custom : string list;
          mutable proto_package : string option;
          mutable extended_typedef : Piqtype.typedef list;
          mutable func_typedef : Piqtype.typedef list;
          mutable extended_func_typedef : Piqtype.typedef list;
          mutable resolved_typedef : Piqtype.typedef list;
          mutable imported_typedef : Piqtype.typedef list;
          mutable resolved_import : Piqtype.import list;
          mutable extended_import : Piqtype.import list;
          mutable resolved_func : Piqtype.func list;
          mutable extended_func : Piqtype.func list;
          mutable included_piqi : Piqtype.piqi list;
          mutable original_piqi : Piqtype.piqi option;
          mutable ast : Piqtype.piq_ast option;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_module : string option
        }
      
    end = Piqi
and
  Import :
    sig
      type t =
        { mutable modname : Piqtype.word; mutable name : Piqtype.name option;
          mutable piqi : Piqtype.piqi option;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option
        }
      
    end = Import
and
  Any :
    sig
      type t =
        { mutable typename : string option;
          mutable protobuf : Piqtype.binary option;
          mutable json : string option; mutable xml : string option;
          mutable ref : int option
        }
      
    end = Any
and
  Func :
    sig
      type t =
        { mutable name : Piqtype.name;
          mutable input : Piqtype.function_param option;
          mutable output : Piqtype.function_param option;
          mutable error : Piqtype.function_param option;
          mutable resolved_input : Piqtype.typedef option;
          mutable resolved_output : Piqtype.typedef option;
          mutable resolved_error : Piqtype.typedef option;
          mutable unparsed_piq_ast : Piqtype.uint option;
          mutable ocaml_name : string option
        }
      
    end = Func
and
  Piqi_bundle : sig type t = { mutable piqi : Piqtype.piqi list }
                     end =
    Piqi_bundle
and
  Includ :
    sig
      type t =
        { mutable modname : Piqtype.word;
          mutable unparsed_piq_ast : Piqtype.uint option
        }
      
    end = Includ
and
  Extend :
    sig
      type t =
        { mutable what : Piqtype.extend_target list; mutable override : bool;
          mutable piqi_with : Piqtype.piqi_any list;
          mutable quote : Piqtype.piqi_any list;
          mutable unparsed_piq_ast : Piqtype.uint option
        }
      
    end = Extend
and
  Pib_typehint :
    sig
      type t =
        { mutable piqi_type : string; mutable typename : Piqtype.typename;
          mutable code : Piqtype.uint
        }
      
    end = Pib_typehint
  
include Piqtype
  
let next_count = Piqloc.next_icount
  
let curr_count () = !Piqloc.icount
  
let refer ref obj =
  if not (Obj.is_int (Obj.repr obj)) then Piqloc.addrefret ref obj else obj
  
let incr_count_if_true (((obj, _) as res)) =
  (if obj then ignore (next_count ()) else (); res)
  
let rec parse_piq_format x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 251462090 when x = (Piqirun.Varint 1) ->
           let count = next_count () in refer count `word
       | 217697453 when x = (Piqirun.Varint 1) ->
           let count = next_count () in refer count `text
       | _ -> Piqirun.error_variant x code)
and parse_protobuf_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_signed_varint x))
    x
and packed_parse_protobuf_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_signed_varint x))
    x
and parse_protobuf_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_signed_varint x))
    x
and packed_parse_protobuf_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_signed_varint x))
    x
and parse_protobuf_wire_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_signed_varint x with
       | 329594984l -> `varint
       | 99211597l -> `zigzag_varint
       | 136997651l -> `fixed32
       | 136998322l -> `fixed64
       | 441915897l -> `signed_varint
       | 488499298l -> `signed_fixed32
       | 488499969l -> `signed_fixed64
       | 352089421l -> `block
       | x -> Piqirun.error_enum_const x)
and packed_parse_protobuf_wire_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_packed_signed_varint x with
       | 329594984l -> `varint
       | 99211597l -> `zigzag_varint
       | 136997651l -> `fixed32
       | 136998322l -> `fixed64
       | 441915897l -> `signed_varint
       | 488499298l -> `signed_fixed32
       | 488499969l -> `signed_fixed64
       | 352089421l -> `block
       | x -> Piqirun.error_enum_const x)
and parse_bool x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.bool_of_varint x))
    x
and packed_parse_bool x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.bool_of_packed_varint x))
    x
and parse_string x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.string_of_block x))
    x
and parse_binary x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.string_of_block x))
    x
and parse_piqi_any x = parse_any x
and parse_int x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int_of_zigzag_varint x))
    x
and packed_parse_int x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int_of_packed_zigzag_varint x))
    x
and parse_uint x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int_of_varint x))
    x
and packed_parse_uint x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int_of_packed_varint x))
    x
and parse_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_zigzag_varint x))
    x
and packed_parse_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_zigzag_varint x))
    x
and parse_uint32 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int32_of_varint x))
    x
and packed_parse_uint32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_varint x))
    x
and parse_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_zigzag_varint x))
    x
and packed_parse_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_zigzag_varint x))
    x
and parse_uint64 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int64_of_varint x))
    x
and packed_parse_uint64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_varint x))
    x
and parse_float64 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.float_of_fixed64 x))
    x
and packed_parse_float64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.float_of_packed_fixed64 x))
    x
and parse_float32 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.float_of_fixed32 x))
    x
and packed_parse_float32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.float_of_packed_fixed32 x))
    x
and parse_int32_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_signed_fixed32 x))
    x
and packed_parse_int32_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_signed_fixed32 x))
    x
and parse_uint32_fixed x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int32_of_fixed32 x))
    x
and packed_parse_uint32_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_fixed32 x))
    x
and parse_int64_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_signed_fixed64 x))
    x
and packed_parse_int64_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_signed_fixed64 x))
    x
and parse_uint64_fixed x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int64_of_fixed64 x))
    x
and packed_parse_uint64_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_fixed64 x))
    x
and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x
and parse_word x = parse_string x
and parse_name x = parse_word x
and parse_typedef x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 502036113 ->
           let res =
             let count = curr_count () in refer count (parse_record x)
           in `record res
       | 484589701 ->
           let res =
             let count = curr_count () in refer count (parse_variant x)
           in `variant res
       | 51800833 ->
           let res = let count = curr_count () in refer count (parse_enum x)
           in `enum res
       | 26300816 ->
           let res = let count = curr_count () in refer count (parse_alias x)
           in `alias res
       | 129178718 ->
           let res =
             let count = curr_count () in refer count (parse_piqi_list x)
           in `list res
       | _ -> Piqirun.error_variant x code)
and parse_piqi_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_signed_varint x with
       | 5246191l -> `int
       | 43435420l -> `float
       | 18580522l -> `bool
       | 288368849l -> `string
       | 218872833l -> `binary
       | 4848364l -> `any
       | x -> Piqirun.error_enum_const x)
and packed_parse_piqi_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_packed_signed_varint x with
       | 5246191l -> `int
       | 43435420l -> `float
       | 18580522l -> `bool
       | 288368849l -> `string
       | 218872833l -> `binary
       | 4848364l -> `any
       | x -> Piqirun.error_enum_const x)
and parse_typename x = parse_name x
and parse_record x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_field, x) =
         Piqirun.parse_repeated_field 9671866 parse_field x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_wire_field, x) =
         Piqirun.parse_repeated_field 112412530 parse_field x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Record.unparsed_piq_ast = _unparsed_piq_ast;
            Record.field = _field;
            Record.protobuf_name = _protobuf_name;
            Record.protobuf_custom = _protobuf_custom;
            Record.wire_field = _wire_field;
            Record.proto_name = _proto_name;
            Record.name = _name;
            Record.parent = _parent;
            Record.ocaml_name = _ocaml_name;
            Record.is_func_param = _is_func_param;
            Record.proto_custom = _proto_custom;
            Record.json_name = _json_name;
          }))
and parse_field x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_code, x) =
         Piqirun.parse_optional_field 29667629 parse_int32 x in
       let (_deprecated, x) =
         incr_count_if_true (Piqirun.parse_flag 69402483 x) in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_mode, x) =
         Piqirun.parse_required_field 140563299 parse_field_mode x
           ~default: "\b\223\162\138\147\001" in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
       let (_alt_name, x) =
         Piqirun.parse_optional_field 177782575 parse_word x in
       let (_protobuf_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 179842426 x) in
       let (_getopt_letter, x) =
         Piqirun.parse_optional_field 215188758 parse_word x in
       let (_typename, x) =
         Piqirun.parse_optional_field 218690234 parse_typename x in
       let (_piq_format, x) =
         Piqirun.parse_optional_field 296833484 parse_piq_format x in
       let (_ocaml_array, x) =
         incr_count_if_true (Piqirun.parse_flag 333250744 x) in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_wire_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 422905280 x) in
       let (_getopt_doc, x) =
         Piqirun.parse_optional_field 442330184 parse_string x in
       let (_default, x) =
         Piqirun.parse_optional_field 465819841 parse_piqi_any x in
       let (_ocaml_optional, x) =
         incr_count_if_true (Piqirun.parse_flag 488413665 x) in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Field.unparsed_piq_ast = _unparsed_piq_ast;
            Field.code = _code;
            Field.deprecated = _deprecated;
            Field.protobuf_name = _protobuf_name;
            Field.proto_name = _proto_name;
            Field.mode = _mode;
            Field.name = _name;
            Field.piqtype = _piqtype;
            Field.alt_name = _alt_name;
            Field.protobuf_packed = _protobuf_packed;
            Field.getopt_letter = _getopt_letter;
            Field.typename = _typename;
            Field.piq_format = _piq_format;
            Field.ocaml_array = _ocaml_array;
            Field.ocaml_name = _ocaml_name;
            Field.wire_packed = _wire_packed;
            Field.getopt_doc = _getopt_doc;
            Field.default = _default;
            Field.ocaml_optional = _ocaml_optional;
            Field.json_name = _json_name;
          }))
and parse_field_mode x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_signed_varint x with
       | 308449631l -> `required
       | 510570400l -> `optional
       | 274054266l -> `repeated
       | x -> Piqirun.error_enum_const x)
and packed_parse_field_mode x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_packed_signed_varint x with
       | 308449631l -> `required
       | 510570400l -> `optional
       | 274054266l -> `repeated
       | x -> Piqirun.error_enum_const x)
and parse_variant x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_option, x) =
         Piqirun.parse_repeated_field 192598901 parse_option x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Variant.unparsed_piq_ast = _unparsed_piq_ast;
            Variant.protobuf_name = _protobuf_name;
            Variant.protobuf_custom = _protobuf_custom;
            Variant.proto_name = _proto_name;
            Variant.name = _name;
            Variant.option = _option;
            Variant.parent = _parent;
            Variant.ocaml_name = _ocaml_name;
            Variant.is_func_param = _is_func_param;
            Variant.proto_custom = _proto_custom;
            Variant.json_name = _json_name;
          }))
and parse_option x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_code, x) =
         Piqirun.parse_optional_field 29667629 parse_int32 x in
       let (_deprecated, x) =
         incr_count_if_true (Piqirun.parse_flag 69402483 x) in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
       let (_alt_name, x) =
         Piqirun.parse_optional_field 177782575 parse_word x in
       let (_getopt_letter, x) =
         Piqirun.parse_optional_field 215188758 parse_word x in
       let (_typename, x) =
         Piqirun.parse_optional_field 218690234 parse_typename x in
       let (_piq_format, x) =
         Piqirun.parse_optional_field 296833484 parse_piq_format x in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_getopt_doc, x) =
         Piqirun.parse_optional_field 442330184 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Option.unparsed_piq_ast = _unparsed_piq_ast;
            Option.code = _code;
            Option.deprecated = _deprecated;
            Option.protobuf_name = _protobuf_name;
            Option.proto_name = _proto_name;
            Option.name = _name;
            Option.piqtype = _piqtype;
            Option.alt_name = _alt_name;
            Option.getopt_letter = _getopt_letter;
            Option.typename = _typename;
            Option.piq_format = _piq_format;
            Option.ocaml_name = _ocaml_name;
            Option.getopt_doc = _getopt_doc;
            Option.json_name = _json_name;
          }))
and parse_enum x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_option, x) =
         Piqirun.parse_repeated_field 192598901 parse_option x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_protobuf_prefix, x) =
         Piqirun.parse_optional_field 366391188 parse_string x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Enum.unparsed_piq_ast = _unparsed_piq_ast;
            Enum.protobuf_name = _protobuf_name;
            Enum.protobuf_custom = _protobuf_custom;
            Enum.proto_name = _proto_name;
            Enum.name = _name;
            Enum.option = _option;
            Enum.parent = _parent;
            Enum.ocaml_name = _ocaml_name;
            Enum.protobuf_prefix = _protobuf_prefix;
            Enum.is_func_param = _is_func_param;
            Enum.proto_custom = _proto_custom;
            Enum.json_name = _json_name;
          }))
and parse_alias x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_protobuf_type, x) =
         Piqirun.parse_optional_field 157803580 parse_string x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
       let (_protobuf_wire_type, x) =
         Piqirun.parse_optional_field 198202944 parse_protobuf_wire_type x in
       let (_piqi_type, x) =
         Piqirun.parse_optional_field 198318774 parse_piqi_type x in
       let (_typename, x) =
         Piqirun.parse_optional_field 218690234 parse_typename x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_piq_format, x) =
         Piqirun.parse_optional_field 296833484 parse_piq_format x in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_ocaml_type, x) =
         Piqirun.parse_optional_field 419588219 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Alias.unparsed_piq_ast = _unparsed_piq_ast;
            Alias.protobuf_name = _protobuf_name;
            Alias.proto_name = _proto_name;
            Alias.name = _name;
            Alias.protobuf_type = _protobuf_type;
            Alias.piqtype = _piqtype;
            Alias.protobuf_wire_type = _protobuf_wire_type;
            Alias.piqi_type = _piqi_type;
            Alias.typename = _typename;
            Alias.parent = _parent;
            Alias.piq_format = _piq_format;
            Alias.ocaml_name = _ocaml_name;
            Alias.is_func_param = _is_func_param;
            Alias.ocaml_type = _ocaml_type;
            Alias.json_name = _json_name;
          }))
and parse_piqi_list x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
       let (_protobuf_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 179842426 x) in
       let (_typename, x) =
         Piqirun.parse_required_field 218690234 parse_typename x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_piq_format, x) =
         Piqirun.parse_optional_field 296833484 parse_piq_format x in
       let (_ocaml_array, x) =
         incr_count_if_true (Piqirun.parse_flag 333250744 x) in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_wire_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 422905280 x) in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Piqi_list.unparsed_piq_ast = _unparsed_piq_ast;
            Piqi_list.protobuf_name = _protobuf_name;
            Piqi_list.protobuf_custom = _protobuf_custom;
            Piqi_list.proto_name = _proto_name;
            Piqi_list.name = _name;
            Piqi_list.piqtype = _piqtype;
            Piqi_list.protobuf_packed = _protobuf_packed;
            Piqi_list.typename = _typename;
            Piqi_list.parent = _parent;
            Piqi_list.piq_format = _piq_format;
            Piqi_list.ocaml_array = _ocaml_array;
            Piqi_list.ocaml_name = _ocaml_name;
            Piqi_list.is_func_param = _is_func_param;
            Piqi_list.proto_custom = _proto_custom;
            Piqi_list.wire_packed = _wire_packed;
            Piqi_list.json_name = _json_name;
          }))
and parse_piqi x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_ast, x) =
         Piqirun.parse_optional_field 4849474 parse_piq_ast x in
       let (_modname, x) =
         Piqirun.parse_optional_field 13841580 parse_word x in
       let (_imported_typedef, x) =
         Piqirun.parse_repeated_field 43698114 parse_typedef x in
       let (_extended_func, x) =
         Piqirun.parse_repeated_field 79393432 parse_func x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_resolved_import, x) =
         Piqirun.parse_repeated_field 114029658 parse_import x in
       let (_extend, x) =
         Piqirun.parse_repeated_field 119198170 parse_extend x in
       let (_import, x) =
         Piqirun.parse_repeated_field 142778725 parse_import x in
       let (_included_piqi, x) =
         Piqirun.parse_repeated_field 146026754 parse_piqi x in
       let (_extended_typedef, x) =
         Piqirun.parse_repeated_field 150338679 parse_typedef x in
       let (_custom_field, x) =
         Piqirun.parse_repeated_field 162247646 parse_word x in
       let (_resolved_func, x) =
         Piqirun.parse_repeated_field 268445433 parse_func x in
       let (_includ, x) =
         Piqirun.parse_repeated_field 301399592 parse_includ x in
       let (_func_typedef, x) =
         Piqirun.parse_repeated_field 301864450 parse_typedef x in
       let (_proto_package, x) =
         Piqirun.parse_optional_field 333467105 parse_string x in
       let (_func, x) =
         Piqirun.parse_repeated_field 340962072 parse_func x in
       let (_ocaml_module, x) =
         Piqirun.parse_optional_field 375807149 parse_string x in
       let (_protobuf_package, x) =
         Piqirun.parse_optional_field 376215364 parse_string x in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_typedef, x) =
         Piqirun.parse_repeated_field 416823115 parse_typedef x in
       let (_extended_import, x) =
         Piqirun.parse_repeated_field 430482873 parse_import x in
       let (_resolved_typedef, x) =
         Piqirun.parse_repeated_field 448232118 parse_typedef x in
       let (_original_piqi, x) =
         Piqirun.parse_optional_field 455316941 parse_piqi x in
       let (_extended_func_typedef, x) =
         Piqirun.parse_repeated_field 512364886 parse_typedef x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Piqi.unparsed_piq_ast = _unparsed_piq_ast;
            Piqi.ast = _ast;
            Piqi.modname = _modname;
            Piqi.imported_typedef = _imported_typedef;
            Piqi.extended_func = _extended_func;
            Piqi.protobuf_custom = _protobuf_custom;
            Piqi.resolved_import = _resolved_import;
            Piqi.extend = _extend;
            Piqi.import = _import;
            Piqi.included_piqi = _included_piqi;
            Piqi.extended_typedef = _extended_typedef;
            Piqi.custom_field = _custom_field;
            Piqi.resolved_func = _resolved_func;
            Piqi.includ = _includ;
            Piqi.func_typedef = _func_typedef;
            Piqi.proto_package = _proto_package;
            Piqi.func = _func;
            Piqi.ocaml_module = _ocaml_module;
            Piqi.protobuf_package = _protobuf_package;
            Piqi.proto_custom = _proto_custom;
            Piqi.typedef = _typedef;
            Piqi.extended_import = _extended_import;
            Piqi.resolved_typedef = _resolved_typedef;
            Piqi.original_piqi = _original_piqi;
            Piqi.extended_func_typedef = _extended_func_typedef;
          }))
and parse_import x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_modname, x) =
         Piqirun.parse_required_field 13841580 parse_word x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqi, x) =
         Piqirun.parse_optional_field 173536529 parse_piqi x in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Import.unparsed_piq_ast = _unparsed_piq_ast;
            Import.modname = _modname;
            Import.name = _name;
            Import.piqi = _piqi;
            Import.ocaml_name = _ocaml_name;
          }))
and parse_any x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_ref, x) = Piqirun.parse_optional_field 5691731 parse_int x in
       let (_xml, x) = Piqirun.parse_optional_field 5991895 parse_string x in
       let (_protobuf, x) =
         Piqirun.parse_optional_field 6461771 parse_binary x in
       let (_json, x) =
         Piqirun.parse_optional_field 107495976 parse_string x in
       let (_typename, x) =
         Piqirun.parse_optional_field 218690234 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Any.ref = _ref;
            Any.xml = _xml;
            Any.protobuf = _protobuf;
            Any.json = _json;
            Any.typename = _typename;
          }))
and parse_func x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_resolved_input, x) =
         Piqirun.parse_optional_field 95864501 parse_typedef x in
       let (_name, x) =
         Piqirun.parse_required_field 150958667 parse_name x in
       let (_resolved_output, x) =
         Piqirun.parse_optional_field 181035510 parse_typedef x in
       let (_output, x) =
         Piqirun.parse_optional_field 209784577 parse_function_param x in
       let (_error, x) =
         Piqirun.parse_optional_field 321506248 parse_function_param x in
       let (_ocaml_name, x) =
         Piqirun.parse_optional_field 351856652 parse_string x in
       let (_resolved_error, x) =
         Piqirun.parse_optional_field 448974451 parse_typedef x in
       let (_input, x) =
         Piqirun.parse_optional_field 505267210 parse_function_param x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Func.unparsed_piq_ast = _unparsed_piq_ast;
            Func.resolved_input = _resolved_input;
            Func.name = _name;
            Func.resolved_output = _resolved_output;
            Func.output = _output;
            Func.error = _error;
            Func.ocaml_name = _ocaml_name;
            Func.resolved_error = _resolved_error;
            Func.input = _input;
          }))
and parse_piqi_bundle x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_piqi, x) = Piqirun.parse_repeated_field 1 parse_piqi x
       in (Piqirun.check_unparsed_fields x; { Piqi_bundle.piqi = _piqi; }))
and parse_includ x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_modname, x) = Piqirun.parse_required_field 13841580 parse_word x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Includ.unparsed_piq_ast = _unparsed_piq_ast;
            Includ.modname = _modname;
          }))
and parse_function_param x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 150958667 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `name res
       | 502036113 ->
           let res =
             let count = curr_count () in refer count (parse_record x)
           in `record res
       | 484589701 ->
           let res =
             let count = curr_count () in refer count (parse_variant x)
           in `variant res
       | 51800833 ->
           let res = let count = curr_count () in refer count (parse_enum x)
           in `enum res
       | 129178718 ->
           let res =
             let count = curr_count () in refer count (parse_piqi_list x)
           in `list res
       | 26300816 ->
           let res = let count = curr_count () in refer count (parse_alias x)
           in `alias res
       | _ -> Piqirun.error_variant x code)
and parse_extend x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_override, x) =
         incr_count_if_true (Piqirun.parse_flag 153625164 x) in
       let (_what, x) =
         Piqirun.parse_repeated_field 251110212 parse_extend_target x in
       let (_piqi_with, x) =
         Piqirun.parse_repeated_field 251164166 parse_piqi_any x in
       let (_quote, x) =
         Piqirun.parse_repeated_field 365880944 parse_piqi_any x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Extend.unparsed_piq_ast = _unparsed_piq_ast;
            Extend.override = _override;
            Extend.what = _what;
            Extend.piqi_with = _piqi_with;
            Extend.quote = _quote;
          }))
and parse_extend_target x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 416823115 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `typedef res
       | 150958667 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `name res
       | 9671866 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `field res
       | 192598901 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `option res
       | 142778725 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `import res
       | 340962072 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `func res
       | _ -> Piqirun.error_variant x code)
and parse_namespace x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 173536529 ->
           let res = let count = curr_count () in refer count (parse_piqi x)
           in `piqi res
       | 142778725 ->
           let res =
             let count = curr_count () in refer count (parse_import x)
           in `import res
       | _ -> Piqirun.error_variant x code)
and parse_piqtype x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 416823115 -> (parse_typedef x :> piqtype)
       | 198318774 -> (parse_piqi_type x :> piqtype)
       | _ -> Piqirun.error_variant x code)
and parse_piq_ast x = Piq_ast.ast_of_bool (parse_bool x)
and packed_parse_piq_ast x = Piq_ast.ast_of_bool (packed_parse_bool x)
and parse_pib_typehint x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_piqi_type, x) = Piqirun.parse_required_field 1 parse_string x in
       let (_typename, x) =
         Piqirun.parse_required_field 2 parse_typename x in
       let (_code, x) = Piqirun.parse_required_field 3 parse_uint x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Pib_typehint.piqi_type = _piqi_type;
            Pib_typehint.typename = _typename;
            Pib_typehint.code = _code;
          }))
  
let next_count = Piqloc.next_ocount
  
let refer obj =
  let count = next_count ()
  in if not (Obj.is_int (Obj.repr obj)) then Piqloc.addref obj count else ()
  
let reference f code x = (refer x; f code x)
  
let reference1 f x = (refer x; f x)
  
let reference_if_true f code x = if x then reference f code x else f code x
  
let rec gen__piq_format code (x : Piqtype.piq_format) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `word -> (refer x; Piqirun.gen_bool_field 251462090 true)
        | `text -> (refer x; Piqirun.gen_bool_field 217697453 true)) ])
and gen__protobuf_int32 code x =
  reference Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x =
  reference1 Piqirun.int32_to_packed_signed_varint x
and gen__protobuf_int64 code x =
  reference Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x =
  reference1 Piqirun.int64_to_packed_signed_varint x
and gen__protobuf_wire_type code x =
  (refer x;
   Piqirun.int32_to_signed_varint code
     (match x with
      | `varint -> 329594984l
      | `zigzag_varint -> 99211597l
      | `fixed32 -> 136997651l
      | `fixed64 -> 136998322l
      | `signed_varint -> 441915897l
      | `signed_fixed32 -> 488499298l
      | `signed_fixed64 -> 488499969l
      | `block -> 352089421l))
and packed_gen__protobuf_wire_type x =
  (refer x;
   Piqirun.int32_to_packed_signed_varint
     (match x with
      | `varint -> 329594984l
      | `zigzag_varint -> 99211597l
      | `fixed32 -> 136997651l
      | `fixed64 -> 136998322l
      | `signed_varint -> 441915897l
      | `signed_fixed32 -> 488499298l
      | `signed_fixed64 -> 488499969l
      | `block -> 352089421l))
and gen__bool code x = reference Piqirun.bool_to_varint code x
and packed_gen__bool x = reference1 Piqirun.bool_to_packed_varint x
and gen__string code x = reference Piqirun.string_to_block code x
and gen__binary code x = reference Piqirun.string_to_block code x
and gen__piqi_any code x = (fun code x -> gen__any code x) code x
and gen__int code x = reference Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = reference1 Piqirun.int_to_packed_zigzag_varint x
and gen__uint code x = reference Piqirun.int_to_varint code x
and packed_gen__uint x = reference1 Piqirun.int_to_packed_varint x
and gen__int32 code x = reference Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = reference1 Piqirun.int32_to_packed_zigzag_varint x
and gen__uint32 code x = reference Piqirun.int32_to_varint code x
and packed_gen__uint32 x = reference1 Piqirun.int32_to_packed_varint x
and gen__int64 code x = reference Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = reference1 Piqirun.int64_to_packed_zigzag_varint x
and gen__uint64 code x = reference Piqirun.int64_to_varint code x
and packed_gen__uint64 x = reference1 Piqirun.int64_to_packed_varint x
and gen__float64 code x = reference Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = reference1 Piqirun.float_to_packed_fixed64 x
and gen__float32 code x = reference Piqirun.float_to_fixed32 code x
and packed_gen__float32 x = reference1 Piqirun.float_to_packed_fixed32 x
and gen__int32_fixed code x =
  reference Piqirun.int32_to_signed_fixed32 code x
and packed_gen__int32_fixed x =
  reference1 Piqirun.int32_to_packed_signed_fixed32 x
and gen__uint32_fixed code x = reference Piqirun.int32_to_fixed32 code x
and packed_gen__uint32_fixed x = reference1 Piqirun.int32_to_packed_fixed32 x
and gen__int64_fixed code x =
  reference Piqirun.int64_to_signed_fixed64 code x
and packed_gen__int64_fixed x =
  reference1 Piqirun.int64_to_packed_signed_fixed64 x
and gen__uint64_fixed code x = reference Piqirun.int64_to_fixed64 code x
and packed_gen__uint64_fixed x = reference1 Piqirun.int64_to_packed_fixed64 x
and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x
and gen__word code x = gen__string code x
and gen__name code x = gen__word code x
and gen__typedef code (x : Piqtype.typedef) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `record x -> gen__record 502036113 x
        | `variant x -> gen__variant 484589701 x
        | `enum x -> gen__enum 51800833 x
        | `alias x -> gen__alias 26300816 x
        | `list x -> gen__piqi_list 129178718 x) ])
and gen__piqi_type code x =
  (refer x;
   Piqirun.int32_to_signed_varint code
     (match x with
      | `int -> 5246191l
      | `float -> 43435420l
      | `bool -> 18580522l
      | `string -> 288368849l
      | `binary -> 218872833l
      | `any -> 4848364l))
and packed_gen__piqi_type x =
  (refer x;
   Piqirun.int32_to_packed_signed_varint
     (match x with
      | `int -> 5246191l
      | `float -> 43435420l
      | `bool -> 18580522l
      | `string -> 288368849l
      | `binary -> 218872833l
      | `any -> 4848364l))
and gen__typename code x = gen__name code x
and gen__record code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Record.unparsed_piq_ast in
   let _field =
     Piqirun.gen_repeated_field 9671866 gen__field x.Record.field in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Record.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string
       x.Record.protobuf_custom in
   let _wire_field =
     Piqirun.gen_repeated_field 112412530 gen__field x.Record.wire_field in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Record.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Record.name in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Record.parent in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Record.ocaml_name in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Record.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Record.proto_custom in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Record.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _field; _protobuf_name; _protobuf_custom;
         _wire_field; _proto_name; _name; _parent; _ocaml_name;
         _is_func_param; _proto_custom; _json_name ])
and gen__field code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Field.unparsed_piq_ast in
   let _code = Piqirun.gen_optional_field 29667629 gen__int32 x.Field.code in
   let _deprecated =
     reference_if_true Piqirun.gen_flag 69402483 x.Field.deprecated in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Field.protobuf_name in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Field.proto_name in
   let _mode =
     Piqirun.gen_required_field 140563299 gen__field_mode x.Field.mode in
   let _name = Piqirun.gen_optional_field 150958667 gen__name x.Field.name in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Field.piqtype in
   let _alt_name =
     Piqirun.gen_optional_field 177782575 gen__word x.Field.alt_name in
   let _protobuf_packed =
     reference_if_true Piqirun.gen_flag 179842426 x.Field.protobuf_packed in
   let _getopt_letter =
     Piqirun.gen_optional_field 215188758 gen__word x.Field.getopt_letter in
   let _typename =
     Piqirun.gen_optional_field 218690234 gen__typename x.Field.typename in
   let _piq_format =
     Piqirun.gen_optional_field 296833484 gen__piq_format x.Field.piq_format in
   let _ocaml_array =
     reference_if_true Piqirun.gen_flag 333250744 x.Field.ocaml_array in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Field.ocaml_name in
   let _wire_packed =
     reference_if_true Piqirun.gen_flag 422905280 x.Field.wire_packed in
   let _getopt_doc =
     Piqirun.gen_optional_field 442330184 gen__string x.Field.getopt_doc in
   let _default =
     Piqirun.gen_optional_field 465819841 gen__piqi_any x.Field.default in
   let _ocaml_optional =
     reference_if_true Piqirun.gen_flag 488413665 x.Field.ocaml_optional in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Field.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _code; _deprecated; _protobuf_name; _proto_name;
         _mode; _name; _piqtype; _alt_name; _protobuf_packed; _getopt_letter;
         _typename; _piq_format; _ocaml_array; _ocaml_name; _wire_packed;
         _getopt_doc; _default; _ocaml_optional; _json_name ])
and gen__field_mode code x =
  (refer x;
   Piqirun.int32_to_signed_varint code
     (match x with
      | `required -> 308449631l
      | `optional -> 510570400l
      | `repeated -> 274054266l))
and packed_gen__field_mode x =
  (refer x;
   Piqirun.int32_to_packed_signed_varint
     (match x with
      | `required -> 308449631l
      | `optional -> 510570400l
      | `repeated -> 274054266l))
and gen__variant code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Variant.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Variant.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string
       x.Variant.protobuf_custom in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Variant.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Variant.name in
   let _option =
     Piqirun.gen_repeated_field 192598901 gen__option x.Variant.option in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Variant.parent in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Variant.ocaml_name in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Variant.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Variant.proto_custom in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Variant.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _protobuf_custom; _proto_name;
         _name; _option; _parent; _ocaml_name; _is_func_param; _proto_custom;
         _json_name ])
and gen__option code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Option.unparsed_piq_ast in
   let _code =
     Piqirun.gen_optional_field 29667629 gen__int32 x.Option.code in
   let _deprecated =
     reference_if_true Piqirun.gen_flag 69402483 x.Option.deprecated in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Option.protobuf_name in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Option.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Option.name in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Option.piqtype in
   let _alt_name =
     Piqirun.gen_optional_field 177782575 gen__word x.Option.alt_name in
   let _getopt_letter =
     Piqirun.gen_optional_field 215188758 gen__word x.Option.getopt_letter in
   let _typename =
     Piqirun.gen_optional_field 218690234 gen__typename x.Option.typename in
   let _piq_format =
     Piqirun.gen_optional_field 296833484 gen__piq_format x.Option.piq_format in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Option.ocaml_name in
   let _getopt_doc =
     Piqirun.gen_optional_field 442330184 gen__string x.Option.getopt_doc in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Option.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _code; _deprecated; _protobuf_name; _proto_name;
         _name; _piqtype; _alt_name; _getopt_letter; _typename; _piq_format;
         _ocaml_name; _getopt_doc; _json_name ])
and gen__enum code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Enum.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Enum.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string x.Enum.protobuf_custom in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Enum.proto_name in
   let _name = Piqirun.gen_optional_field 150958667 gen__name x.Enum.name in
   let _option =
     Piqirun.gen_repeated_field 192598901 gen__option x.Enum.option in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Enum.parent in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Enum.ocaml_name in
   let _protobuf_prefix =
     Piqirun.gen_optional_field 366391188 gen__string x.Enum.protobuf_prefix in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Enum.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Enum.proto_custom in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Enum.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _protobuf_custom; _proto_name;
         _name; _option; _parent; _ocaml_name; _protobuf_prefix;
         _is_func_param; _proto_custom; _json_name ])
and gen__alias code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Alias.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Alias.protobuf_name in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Alias.proto_name in
   let _name = Piqirun.gen_optional_field 150958667 gen__name x.Alias.name in
   let _protobuf_type =
     Piqirun.gen_optional_field 157803580 gen__string x.Alias.protobuf_type in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Alias.piqtype in
   let _protobuf_wire_type =
     Piqirun.gen_optional_field 198202944 gen__protobuf_wire_type
       x.Alias.protobuf_wire_type in
   let _piqi_type =
     Piqirun.gen_optional_field 198318774 gen__piqi_type x.Alias.piqi_type in
   let _typename =
     Piqirun.gen_optional_field 218690234 gen__typename x.Alias.typename in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Alias.parent in
   let _piq_format =
     Piqirun.gen_optional_field 296833484 gen__piq_format x.Alias.piq_format in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Alias.ocaml_name in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Alias.is_func_param in
   let _ocaml_type =
     Piqirun.gen_optional_field 419588219 gen__string x.Alias.ocaml_type in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Alias.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _proto_name; _name;
         _protobuf_type; _piqtype; _protobuf_wire_type; _piqi_type;
         _typename; _parent; _piq_format; _ocaml_name; _is_func_param;
         _ocaml_type; _json_name ])
and gen__piqi_list code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Piqi_list.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string
       x.Piqi_list.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string
       x.Piqi_list.protobuf_custom in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Piqi_list.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Piqi_list.name in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Piqi_list.piqtype in
   let _protobuf_packed =
     reference_if_true Piqirun.gen_flag 179842426 x.Piqi_list.protobuf_packed in
   let _typename =
     Piqirun.gen_required_field 218690234 gen__typename x.Piqi_list.typename in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Piqi_list.parent in
   let _piq_format =
     Piqirun.gen_optional_field 296833484 gen__piq_format
       x.Piqi_list.piq_format in
   let _ocaml_array =
     reference_if_true Piqirun.gen_flag 333250744 x.Piqi_list.ocaml_array in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Piqi_list.ocaml_name in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Piqi_list.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string
       x.Piqi_list.proto_custom in
   let _wire_packed =
     reference_if_true Piqirun.gen_flag 422905280 x.Piqi_list.wire_packed in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Piqi_list.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _protobuf_custom; _proto_name;
         _name; _piqtype; _protobuf_packed; _typename; _parent; _piq_format;
         _ocaml_array; _ocaml_name; _is_func_param; _proto_custom;
         _wire_packed; _json_name ])
and gen__piqi code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Piqi.unparsed_piq_ast in
   let _ast = Piqirun.gen_optional_field 4849474 gen__piq_ast x.Piqi.ast in
   let _modname =
     Piqirun.gen_optional_field 13841580 gen__word x.Piqi.modname in
   let _imported_typedef =
     Piqirun.gen_repeated_field 43698114 gen__typedef x.Piqi.imported_typedef in
   let _extended_func =
     Piqirun.gen_repeated_field 79393432 gen__func x.Piqi.extended_func in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string x.Piqi.protobuf_custom in
   let _resolved_import =
     Piqirun.gen_repeated_field 114029658 gen__import x.Piqi.resolved_import in
   let _extend =
     Piqirun.gen_repeated_field 119198170 gen__extend x.Piqi.extend in
   let _import =
     Piqirun.gen_repeated_field 142778725 gen__import x.Piqi.import in
   let _included_piqi =
     Piqirun.gen_repeated_field 146026754 gen__piqi x.Piqi.included_piqi in
   let _extended_typedef =
     Piqirun.gen_repeated_field 150338679 gen__typedef
       x.Piqi.extended_typedef in
   let _custom_field =
     Piqirun.gen_repeated_field 162247646 gen__word x.Piqi.custom_field in
   let _resolved_func =
     Piqirun.gen_repeated_field 268445433 gen__func x.Piqi.resolved_func in
   let _includ =
     Piqirun.gen_repeated_field 301399592 gen__includ x.Piqi.includ in
   let _func_typedef =
     Piqirun.gen_repeated_field 301864450 gen__typedef x.Piqi.func_typedef in
   let _proto_package =
     Piqirun.gen_optional_field 333467105 gen__string x.Piqi.proto_package in
   let _func = Piqirun.gen_repeated_field 340962072 gen__func x.Piqi.func in
   let _ocaml_module =
     Piqirun.gen_optional_field 375807149 gen__string x.Piqi.ocaml_module in
   let _protobuf_package =
     Piqirun.gen_optional_field 376215364 gen__string x.Piqi.protobuf_package in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Piqi.proto_custom in
   let _typedef =
     Piqirun.gen_repeated_field 416823115 gen__typedef x.Piqi.typedef in
   let _extended_import =
     Piqirun.gen_repeated_field 430482873 gen__import x.Piqi.extended_import in
   let _resolved_typedef =
     Piqirun.gen_repeated_field 448232118 gen__typedef
       x.Piqi.resolved_typedef in
   let _original_piqi =
     Piqirun.gen_optional_field 455316941 gen__piqi x.Piqi.original_piqi in
   let _extended_func_typedef =
     Piqirun.gen_repeated_field 512364886 gen__typedef
       x.Piqi.extended_func_typedef
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _ast; _modname; _imported_typedef;
         _extended_func; _protobuf_custom; _resolved_import; _extend;
         _import; _included_piqi; _extended_typedef; _custom_field;
         _resolved_func; _includ; _func_typedef; _proto_package; _func;
         _ocaml_module; _protobuf_package; _proto_custom; _typedef;
         _extended_import; _resolved_typedef; _original_piqi;
         _extended_func_typedef ])
and gen__import code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Import.unparsed_piq_ast in
   let _modname =
     Piqirun.gen_required_field 13841580 gen__word x.Import.modname in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Import.name in
   let _piqi =
     Piqirun.gen_optional_field 173536529 gen__piqi x.Import.piqi in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Import.ocaml_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _modname; _name; _piqi; _ocaml_name ])
and gen__any code x =
  (refer x;
   let _ref = Piqirun.gen_optional_field 5691731 gen__int x.Any.ref in
   let _xml = Piqirun.gen_optional_field 5991895 gen__string x.Any.xml in
   let _protobuf =
     Piqirun.gen_optional_field 6461771 gen__binary x.Any.protobuf in
   let _json = Piqirun.gen_optional_field 107495976 gen__string x.Any.json in
   let _typename =
     Piqirun.gen_optional_field 218690234 gen__string x.Any.typename
   in Piqirun.gen_record code [ _ref; _xml; _protobuf; _json; _typename ])
and gen__func code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Func.unparsed_piq_ast in
   let _resolved_input =
     Piqirun.gen_optional_field 95864501 gen__typedef x.Func.resolved_input in
   let _name = Piqirun.gen_required_field 150958667 gen__name x.Func.name in
   let _resolved_output =
     Piqirun.gen_optional_field 181035510 gen__typedef x.Func.resolved_output in
   let _output =
     Piqirun.gen_optional_field 209784577 gen__function_param x.Func.output in
   let _error =
     Piqirun.gen_optional_field 321506248 gen__function_param x.Func.error in
   let _ocaml_name =
     Piqirun.gen_optional_field 351856652 gen__string x.Func.ocaml_name in
   let _resolved_error =
     Piqirun.gen_optional_field 448974451 gen__typedef x.Func.resolved_error in
   let _input =
     Piqirun.gen_optional_field 505267210 gen__function_param x.Func.input
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _resolved_input; _name; _resolved_output;
         _output; _error; _ocaml_name; _resolved_error; _input ])
and gen__piqi_bundle code x =
  (refer x;
   let _piqi = Piqirun.gen_repeated_field 1 gen__piqi x.Piqi_bundle.piqi
   in Piqirun.gen_record code [ _piqi ])
and gen__includ code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Includ.unparsed_piq_ast in
   let _modname =
     Piqirun.gen_required_field 13841580 gen__word x.Includ.modname
   in Piqirun.gen_record code [ _unparsed_piq_ast; _modname ])
and gen__function_param code (x : Piqtype.function_param) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `name x -> gen__name 150958667 x
        | `record x -> gen__record 502036113 x
        | `variant x -> gen__variant 484589701 x
        | `enum x -> gen__enum 51800833 x
        | `list x -> gen__piqi_list 129178718 x
        | `alias x -> gen__alias 26300816 x) ])
and gen__extend code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Extend.unparsed_piq_ast in
   let _override =
     reference_if_true Piqirun.gen_flag 153625164 x.Extend.override in
   let _what =
     Piqirun.gen_repeated_field 251110212 gen__extend_target x.Extend.what in
   let _piqi_with =
     Piqirun.gen_repeated_field 251164166 gen__piqi_any x.Extend.piqi_with in
   let _quote =
     Piqirun.gen_repeated_field 365880944 gen__piqi_any x.Extend.quote
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _override; _what; _piqi_with; _quote ])
and gen__extend_target code (x : Piqtype.extend_target) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `typedef x -> gen__name 416823115 x
        | `name x -> gen__name 150958667 x
        | `field x -> gen__name 9671866 x
        | `option x -> gen__name 192598901 x
        | `import x -> gen__name 142778725 x
        | `func x -> gen__name 340962072 x) ])
and gen__namespace code (x : Piqtype.namespace) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `piqi x -> gen__piqi 173536529 x
        | `import x -> gen__import 142778725 x) ])
and gen__piqtype code (x : Piqtype.piqtype) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | (#typedef as x) -> gen__typedef 416823115 x
        | (#piqi_type as x) -> gen__piqi_type 198318774 x) ])
and gen__piq_ast code x = gen__bool code (Piq_ast.ast_to_bool x)
and packed_gen__piq_ast x = packed_gen__bool (Piq_ast.ast_to_bool x)
and gen__pib_typehint code x =
  (refer x;
   let _piqi_type =
     Piqirun.gen_required_field 1 gen__string x.Pib_typehint.piqi_type in
   let _typename =
     Piqirun.gen_required_field 2 gen__typename x.Pib_typehint.typename in
   let _code = Piqirun.gen_required_field 3 gen__uint x.Pib_typehint.code
   in Piqirun.gen_record code [ _piqi_type; _typename; _code ])
  
let gen_piq_format x = gen__piq_format (-1) x
  
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
  
let gen_protobuf_int64 x = gen__protobuf_int64 (-1) x
  
let gen_protobuf_wire_type x = gen__protobuf_wire_type (-1) x
  
let gen_bool x = gen__bool (-1) x
  
let gen_string x = gen__string (-1) x
  
let gen_binary x = gen__binary (-1) x
  
let gen_piqi_any x = gen__piqi_any (-1) x
  
let gen_int x = gen__int (-1) x
  
let gen_uint x = gen__uint (-1) x
  
let gen_int32 x = gen__int32 (-1) x
  
let gen_uint32 x = gen__uint32 (-1) x
  
let gen_int64 x = gen__int64 (-1) x
  
let gen_uint64 x = gen__uint64 (-1) x
  
let gen_float64 x = gen__float64 (-1) x
  
let gen_float32 x = gen__float32 (-1) x
  
let gen_int32_fixed x = gen__int32_fixed (-1) x
  
let gen_uint32_fixed x = gen__uint32_fixed (-1) x
  
let gen_int64_fixed x = gen__int64_fixed (-1) x
  
let gen_uint64_fixed x = gen__uint64_fixed (-1) x
  
let gen_float x = gen__float (-1) x
  
let gen_word x = gen__word (-1) x
  
let gen_name x = gen__name (-1) x
  
let gen_typedef x = gen__typedef (-1) x
  
let gen_piqi_type x = gen__piqi_type (-1) x
  
let gen_typename x = gen__typename (-1) x
  
let gen_record x = gen__record (-1) x
  
let gen_field x = gen__field (-1) x
  
let gen_field_mode x = gen__field_mode (-1) x
  
let gen_variant x = gen__variant (-1) x
  
let gen_option x = gen__option (-1) x
  
let gen_enum x = gen__enum (-1) x
  
let gen_alias x = gen__alias (-1) x
  
let gen_piqi_list x = gen__piqi_list (-1) x
  
let gen_piqi x = gen__piqi (-1) x
  
let gen_import x = gen__import (-1) x
  
let gen_any x = gen__any (-1) x
  
let gen_func x = gen__func (-1) x
  
let gen_piqi_bundle x = gen__piqi_bundle (-1) x
  
let gen_includ x = gen__includ (-1) x
  
let gen_function_param x = gen__function_param (-1) x
  
let gen_extend x = gen__extend (-1) x
  
let gen_extend_target x = gen__extend_target (-1) x
  
let gen_namespace x = gen__namespace (-1) x
  
let gen_piqtype x = gen__piqtype (-1) x
  
let gen_piq_ast x = gen__piq_ast (-1) x
  
let gen_pib_typehint x = gen__pib_typehint (-1) x
  
let rec default_piq_format () = `word
and default_protobuf_int32 () =
  Piqirun.int32_of_signed_varint (Piqirun.parse_default "\b\000")
and default_protobuf_int64 () =
  Piqirun.int64_of_signed_varint (Piqirun.parse_default "\b\000")
and default_protobuf_wire_type () = `varint
and default_bool () = Piqirun.bool_of_varint (Piqirun.parse_default "\b\000")
and default_string () =
  Piqirun.string_of_block (Piqirun.parse_default "\n\000")
and default_binary () =
  Piqirun.string_of_block (Piqirun.parse_default "\n\000")
and default_piqi_any () = default_any ()
and default_int () =
  Piqirun.int_of_zigzag_varint (Piqirun.parse_default "\b\000")
and default_uint () = Piqirun.int_of_varint (Piqirun.parse_default "\b\000")
and default_int32 () =
  Piqirun.int32_of_zigzag_varint (Piqirun.parse_default "\b\000")
and default_uint32 () =
  Piqirun.int32_of_varint (Piqirun.parse_default "\b\000")
and default_int64 () =
  Piqirun.int64_of_zigzag_varint (Piqirun.parse_default "\b\000")
and default_uint64 () =
  Piqirun.int64_of_varint (Piqirun.parse_default "\b\000")
and default_float64 () =
  Piqirun.float_of_fixed64
    (Piqirun.parse_default "\t\000\000\000\000\000\000\000\000")
and default_float32 () =
  Piqirun.float_of_fixed32 (Piqirun.parse_default "\r\000\000\000\000")
and default_int32_fixed () =
  Piqirun.int32_of_signed_fixed32
    (Piqirun.parse_default "\r\000\000\000\000")
and default_uint32_fixed () =
  Piqirun.int32_of_fixed32 (Piqirun.parse_default "\r\000\000\000\000")
and default_int64_fixed () =
  Piqirun.int64_of_signed_fixed64
    (Piqirun.parse_default "\t\000\000\000\000\000\000\000\000")
and default_uint64_fixed () =
  Piqirun.int64_of_fixed64
    (Piqirun.parse_default "\t\000\000\000\000\000\000\000\000")
and default_float () = default_float64 ()
and default_word () = default_string ()
and default_name () = default_word ()
and default_typedef () = `record (default_record ())
and default_piqi_type () = `int
and default_typename () = default_name ()
and default_record () =
  {
    Record.unparsed_piq_ast = None;
    Record.field = [];
    Record.protobuf_name = None;
    Record.protobuf_custom = [];
    Record.wire_field = [];
    Record.proto_name = None;
    Record.name = None;
    Record.parent = None;
    Record.ocaml_name = None;
    Record.is_func_param = false;
    Record.proto_custom = [];
    Record.json_name = None;
  }
and default_field () =
  {
    Field.unparsed_piq_ast = None;
    Field.code = None;
    Field.deprecated = false;
    Field.protobuf_name = None;
    Field.proto_name = None;
    Field.mode =
      parse_field_mode (Piqirun.parse_default "\b\223\162\138\147\001");
    Field.name = None;
    Field.piqtype = None;
    Field.alt_name = None;
    Field.protobuf_packed = false;
    Field.getopt_letter = None;
    Field.typename = None;
    Field.piq_format = None;
    Field.ocaml_array = false;
    Field.ocaml_name = None;
    Field.wire_packed = false;
    Field.getopt_doc = None;
    Field.default = None;
    Field.ocaml_optional = false;
    Field.json_name = None;
  }
and default_field_mode () = `required
and default_variant () =
  {
    Variant.unparsed_piq_ast = None;
    Variant.protobuf_name = None;
    Variant.protobuf_custom = [];
    Variant.proto_name = None;
    Variant.name = None;
    Variant.option = [];
    Variant.parent = None;
    Variant.ocaml_name = None;
    Variant.is_func_param = false;
    Variant.proto_custom = [];
    Variant.json_name = None;
  }
and default_option () =
  {
    Option.unparsed_piq_ast = None;
    Option.code = None;
    Option.deprecated = false;
    Option.protobuf_name = None;
    Option.proto_name = None;
    Option.name = None;
    Option.piqtype = None;
    Option.alt_name = None;
    Option.getopt_letter = None;
    Option.typename = None;
    Option.piq_format = None;
    Option.ocaml_name = None;
    Option.getopt_doc = None;
    Option.json_name = None;
  }
and default_enum () =
  {
    Enum.unparsed_piq_ast = None;
    Enum.protobuf_name = None;
    Enum.protobuf_custom = [];
    Enum.proto_name = None;
    Enum.name = None;
    Enum.option = [];
    Enum.parent = None;
    Enum.ocaml_name = None;
    Enum.protobuf_prefix = None;
    Enum.is_func_param = false;
    Enum.proto_custom = [];
    Enum.json_name = None;
  }
and default_alias () =
  {
    Alias.unparsed_piq_ast = None;
    Alias.protobuf_name = None;
    Alias.proto_name = None;
    Alias.name = None;
    Alias.protobuf_type = None;
    Alias.piqtype = None;
    Alias.protobuf_wire_type = None;
    Alias.piqi_type = None;
    Alias.typename = None;
    Alias.parent = None;
    Alias.piq_format = None;
    Alias.ocaml_name = None;
    Alias.is_func_param = false;
    Alias.ocaml_type = None;
    Alias.json_name = None;
  }
and default_piqi_list () =
  {
    Piqi_list.unparsed_piq_ast = None;
    Piqi_list.protobuf_name = None;
    Piqi_list.protobuf_custom = [];
    Piqi_list.proto_name = None;
    Piqi_list.name = None;
    Piqi_list.piqtype = None;
    Piqi_list.protobuf_packed = false;
    Piqi_list.typename = default_typename ();
    Piqi_list.parent = None;
    Piqi_list.piq_format = None;
    Piqi_list.ocaml_array = false;
    Piqi_list.ocaml_name = None;
    Piqi_list.is_func_param = false;
    Piqi_list.proto_custom = [];
    Piqi_list.wire_packed = false;
    Piqi_list.json_name = None;
  }
and default_piqi () =
  {
    Piqi.unparsed_piq_ast = None;
    Piqi.ast = None;
    Piqi.modname = None;
    Piqi.imported_typedef = [];
    Piqi.extended_func = [];
    Piqi.protobuf_custom = [];
    Piqi.resolved_import = [];
    Piqi.extend = [];
    Piqi.import = [];
    Piqi.included_piqi = [];
    Piqi.extended_typedef = [];
    Piqi.custom_field = [];
    Piqi.resolved_func = [];
    Piqi.includ = [];
    Piqi.func_typedef = [];
    Piqi.proto_package = None;
    Piqi.func = [];
    Piqi.ocaml_module = None;
    Piqi.protobuf_package = None;
    Piqi.proto_custom = [];
    Piqi.typedef = [];
    Piqi.extended_import = [];
    Piqi.resolved_typedef = [];
    Piqi.original_piqi = None;
    Piqi.extended_func_typedef = [];
  }
and default_import () =
  {
    Import.unparsed_piq_ast = None;
    Import.modname = default_word ();
    Import.name = None;
    Import.piqi = None;
    Import.ocaml_name = None;
  }
and default_any () =
  {
    Any.ref = None;
    Any.xml = None;
    Any.protobuf = None;
    Any.json = None;
    Any.typename = None;
  }
and default_func () =
  {
    Func.unparsed_piq_ast = None;
    Func.resolved_input = None;
    Func.name = default_name ();
    Func.resolved_output = None;
    Func.output = None;
    Func.error = None;
    Func.ocaml_name = None;
    Func.resolved_error = None;
    Func.input = None;
  }
and default_piqi_bundle () = { Piqi_bundle.piqi = []; }
and default_includ () =
  { Includ.unparsed_piq_ast = None; Includ.modname = default_word (); }
and default_function_param () = `name (default_name ())
and default_extend () =
  {
    Extend.unparsed_piq_ast = None;
    Extend.override = false;
    Extend.what = [];
    Extend.piqi_with = [];
    Extend.quote = [];
  }
and default_extend_target () = `typedef (default_name ())
and default_namespace () = `piqi (default_piqi ())
and default_piqtype () = (default_typedef () :> piqtype)
and default_piq_ast () = Piq_ast.ast_of_bool (default_bool ())
and default_pib_typehint () =
  {
    Pib_typehint.piqi_type = default_string ();
    Pib_typehint.typename = default_typename ();
    Pib_typehint.code = default_uint ();
  }
  
let parse_piqi_binobj x = Piqirun.parse_binobj parse_piqi x
  
let piqi_lang =
  let piqi_lang_binobj =
    "\226\202\2304\tpiqi-lang\218\244\134\182\012H\170\136\200\184\014B\218\164\238\191\004\npiq-format\170\183\218\222\005\019\232\146\150q\148\135\232\239\001\218\164\238\191\004\004word\170\183\218\222\005\019\232\146\150q\218\178\206\207\001\218\164\238\191\004\004text\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\014protobuf-int32\226\195\252\217\004\005int32\128\228\138\244\005\249\179\220\210\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\014protobuf-int64\226\195\252\217\004\005int64\128\228\138\244\005\249\179\220\210\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int64\218\244\134\182\012\149\002\138\176\205\197\001\142\002\218\164\238\191\004\018protobuf-wire-type\170\183\218\222\005\021\232\146\150q\208\225\169\186\002\218\164\238\191\004\006varint\170\183\218\222\005\027\232\146\150q\154\229\206^\218\164\238\191\004\rzigzag-varint\170\183\218\222\005\022\232\146\150q\166\172\211\130\001\218\164\238\191\004\007fixed32\170\183\218\222\005\022\232\146\150q\228\182\211\130\001\218\164\238\191\004\007fixed64\170\183\218\222\005\028\232\146\150q\242\231\184\165\003\218\164\238\191\004\rsigned-varint\170\183\218\222\005\029\232\146\150q\196\161\239\209\003\218\164\238\191\004\014signed-fixed32\170\183\218\222\005\029\232\146\150q\130\172\239\209\003\218\164\238\191\004\014signed-fixed64\170\183\218\222\005\020\232\146\150q\154\213\227\207\002\218\164\238\191\004\005block\218\244\134\182\012\024\130\153\170d\019\218\164\238\191\004\004bool\176\171\195\244\005\170\136\238\b\218\244\134\182\012\027\130\153\170d\022\218\164\238\191\004\006string\176\171\195\244\005\209\209\192\137\001\218\244\134\182\012\026\130\153\170d\021\218\164\238\191\004\006binary\176\171\195\244\005\129\248\174h\218\244\134\182\012\028\130\153\170d\023\218\164\238\191\004\bpiqi-any\176\171\195\244\005\236\245\167\002\218\244\134\182\0125\130\153\170d0\218\164\238\191\004\003int\226\195\252\217\004\006sint32\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\135\205\192\012\003int\218\244\134\182\0127\130\153\170d2\218\164\238\191\004\004uint\226\195\252\217\004\006uint32\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\135\205\192\012\003int\218\244\134\182\0129\130\153\170d4\218\164\238\191\004\005int32\226\195\252\217\004\006sint32\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int32\218\244\134\182\012;\130\153\170d6\218\164\238\191\004\006uint32\226\195\252\217\004\006uint32\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int32\218\244\134\182\0129\130\153\170d4\218\164\238\191\004\005int64\226\195\252\217\004\006sint64\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int64\218\244\134\182\012;\130\153\170d6\218\164\238\191\004\006uint64\226\195\252\217\004\006uint64\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int64\218\244\134\182\012;\130\153\170d6\218\164\238\191\004\007float64\226\195\252\217\004\006double\128\228\138\244\005\178\219\169A\176\171\195\244\005\156\139\219\020\218\135\205\192\012\005float\218\244\134\182\012:\130\153\170d5\218\164\238\191\004\007float32\226\195\252\217\004\005float\128\228\138\244\005\147\214\169A\176\171\195\244\005\156\139\219\020\218\135\205\192\012\005float\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\011int32-fixed\226\195\252\217\004\bsfixed32\128\228\138\244\005\226\208\247\232\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\012uint32-fixed\226\195\252\217\004\007fixed32\128\228\138\244\005\147\214\169A\176\171\195\244\005\239\153\192\002\210\171\158\194\006\006uint32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\011int64-fixed\226\195\252\217\004\bsfixed64\128\228\138\244\005\129\214\247\232\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int64\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\012uint64-fixed\226\195\252\217\004\007fixed64\128\228\138\244\005\178\219\169A\176\171\195\244\005\239\153\192\002\210\171\158\194\006\006uint64\218\244\134\182\012&\130\153\170d!\218\164\238\191\004\005float\176\171\195\244\005\156\139\219\020\210\171\158\194\006\007float64\218\244\134\182\012'\130\153\170d\"\218\164\238\191\004\004word\210\171\158\194\006\006string\226\156\170\236\b\006\208\156\160\191\007\001\218\244\134\182\012\025\130\153\170d\020\218\164\238\191\004\004name\210\171\158\194\006\004word\218\244\134\182\012\176\001\170\136\200\184\014\169\001\234\188\204\215\002\012piqi_typedef\218\164\238\191\004\007typedef\170\183\218\222\005\021\232\146\150q\162\218\227\222\003\210\171\158\194\006\006record\170\183\218\222\005\022\232\146\150q\138\130\146\206\003\210\171\158\194\006\007variant\170\183\218\222\005\018\232\146\150q\130\172\1791\210\171\158\194\006\004enum\170\183\218\222\005\019\232\146\150q\160\198\138\025\210\171\158\194\006\005alias\170\183\218\222\005\028\232\146\150q\188\241\152{\210\171\158\194\006\004list\226\128\157\190\n\004list\218\244\134\182\012\187\001\138\176\205\197\001\180\001\218\164\238\191\004\tpiqi-type\170\183\218\222\005\017\232\146\150q\222\179\128\005\218\164\238\191\004\003int\170\183\218\222\005\019\232\146\150q\184\150\182)\218\164\238\191\004\005float\170\183\218\222\005\018\232\146\150q\212\144\220\017\218\164\238\191\004\004bool\170\183\218\222\005\021\232\146\150q\162\163\129\147\002\218\164\238\191\004\006string\170\183\218\222\005\021\232\146\150q\130\240\221\208\001\218\164\238\191\004\006binary\170\183\218\222\005\017\232\146\150q\216\235\207\004\218\164\238\191\004\003any\162\249\213\245\n\npiqi_type_\218\244\134\182\012'\130\153\170d\"\218\164\238\191\004\004type\210\171\158\194\006\004name\226\128\157\190\n\btypename\218\244\134\182\012\162\003\138\233\142\251\014\155\003\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\210\156\t\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\005field\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$7\232\146\150q\236\166\137\131\003\152\247\223\136\002\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012proto-custom\210\171\158\194\006\006string\210\203\242$5\232\146\150q\160\228\152\133\001\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nproto-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\006record\218\244\134\182\012\210\006\138\233\142\251\014\203\006\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$X\232\146\150q\198\205\134\134\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004mode\210\171\158\194\006\nfield-mode\138\140\251\240\r%\218\148\211\024\006\b\223\162\138\147\001\210\171\158\194\006\020piqi-lang/field-mode\210\203\242$.\232\146\150q\130\227\158\188\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007default\210\171\158\194\006\bpiqi-any\210\203\242$\"\232\146\150q\230\253\151B\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeprecated\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$'\232\146\150q\218\196\165\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004code\210\171\158\194\006\005int32\210\203\242$(\232\146\150q\244\181\193\171\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-packed\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$0\232\146\150q\172\148\156\205\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rgetopt-letter\210\171\158\194\006\004word\210\203\242$/\232\146\150q\144\177\235\165\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ngetopt-doc\210\171\158\194\006\006string\210\203\242$5\232\146\150q\160\228\152\133\001\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nproto-name\210\171\158\194\006\006string\210\203\242$*\232\146\150q\128\151\168\147\003\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011wire-packed\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\210\203\242$$\232\146\150q\240\130\232\189\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011ocaml-array\210\203\242$'\232\146\150q\194\231\228\209\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014ocaml-optional\218\164\238\191\004\005field\218\244\134\182\012m\138\176\205\197\001g\218\164\238\191\004\nfield-mode\170\183\218\222\005\023\232\146\150q\190\197\148\166\002\218\164\238\191\004\brequired\170\183\218\222\005\023\232\146\150q\192\190\245\230\003\218\164\238\191\004\boptional\170\183\218\222\005\023\232\146\150q\244\241\173\133\002\218\164\238\191\004\brepeated\218\244\134\182\012\165\003\138\233\142\251\014\158\003\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\031\232\146\150q\234\205\214\183\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006option\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$7\232\146\150q\236\166\137\131\003\152\247\223\136\002\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012proto-custom\210\171\158\194\006\006string\210\203\242$5\232\146\150q\160\228\152\133\001\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nproto-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\007variant\218\244\134\182\012\146\004\138\233\142\251\014\139\004\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$\"\232\146\150q\230\253\151B\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeprecated\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$'\232\146\150q\218\196\165\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004code\210\171\158\194\006\005int32\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$0\232\146\150q\172\148\156\205\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rgetopt-letter\210\171\158\194\006\004word\210\203\242$/\232\146\150q\144\177\235\165\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ngetopt-doc\210\171\158\194\006\006string\210\203\242$5\232\146\150q\160\228\152\133\001\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nproto-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\006option\218\244\134\182\012\234\003\138\233\142\251\014\227\003\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\031\232\146\150q\234\205\214\183\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006option\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$4\232\146\150q\168\190\181\221\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-prefix\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$7\232\146\150q\236\166\137\131\003\152\247\223\136\002\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012proto-custom\210\171\158\194\006\006string\210\203\242$5\232\146\150q\160\228\152\133\001\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nproto-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\234\188\204\215\002\tpiqi_enum\218\164\238\191\004\004enum\218\244\134\182\012\151\004\138\233\142\251\014\144\004\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$\"\232\146\150q\236\234\144\189\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\tpiqi-type\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$2\232\146\150q\248\144\191\150\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-type\210\171\158\194\006\006string\210\203\242$+\232\146\150q\128\217\130\189\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\018protobuf-wire-type\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$5\232\146\150q\160\228\152\133\001\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nproto-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\246\161\147\144\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-type\210\171\158\194\006\006string\218\164\238\191\004\005alias\218\244\134\182\012\220\004\138\233\142\251\014\213\004\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004type\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$(\232\146\150q\244\181\193\171\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-packed\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$7\232\146\150q\236\166\137\131\003\152\247\223\136\002\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012proto-custom\210\171\158\194\006\006string\210\203\242$5\232\146\150q\160\228\152\133\001\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nproto-name\210\171\158\194\006\006string\210\203\242$*\232\146\150q\128\151\168\147\003\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011wire-packed\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\210\203\242$$\232\146\150q\240\130\232\189\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011ocaml-array\218\164\238\191\004\004list\226\128\157\190\n\tpiqi_list\218\244\134\182\012\215\004\138\233\142\251\014\208\004\210\203\242$5\232\146\150q\216\210\153\r\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006module\210\171\158\194\006\004word\226\128\157\190\n\007modname\210\203\242$ \232\146\150q\150\221\193\141\003\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\007typedef\210\203\242$\031\232\146\150q\202\133\149\136\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006import\210\203\242$!\232\146\150q\176\172\149\197\002\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\bfunction\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$5\232\146\150q\136\221\228\230\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016protobuf-package\210\171\158\194\006\006string\210\203\242$/\232\146\150q\188\207\221\154\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012custom-field\210\171\158\194\006\004word\210\203\242$ \232\146\150q\208\248\183\159\002\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\007include\210\203\242$\030\232\146\150q\180\199\214q\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006extend\210\203\242$7\232\146\150q\236\166\137\131\003\152\247\223\136\002\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012proto-custom\210\171\158\194\006\006string\210\203\242$8\232\146\150q\194\183\130\190\002\152\247\223\136\002\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rproto-package\210\171\158\194\006\006string\210\203\242$1\232\146\150q\218\242\178\230\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012ocaml-module\210\171\158\194\006\006string\218\164\238\191\004\004piqi\218\244\134\182\012\163\001\138\233\142\251\014\156\001\210\203\242$5\232\146\150q\216\210\153\r\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006module\210\171\158\194\006\004word\226\128\157\190\n\007modname\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\006import\218\244\134\182\012\214\001\138\233\142\251\014\207\001\210\203\242$7\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004type\210\171\158\194\006\006string\226\128\157\190\n\btypename\210\203\242$,\232\146\150q\150\229\148\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bprotobuf\210\171\158\194\006\006binary\210\203\242$(\232\146\150q\208\136\194f\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004json\210\171\158\194\006\006string\210\203\242$'\232\146\150q\174\183\219\005\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003xml\210\171\158\194\006\006string\218\164\238\191\004\003any\218\244\134\182\012\155\002\138\233\142\251\014\148\002\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$2\232\146\150q\148\144\238\225\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005input\210\171\158\194\006\014function-param\210\203\242$3\232\146\150q\130\188\136\200\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006output\210\171\158\194\006\014function-param\210\203\242$2\232\146\150q\144\175\206\178\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005error\210\171\158\194\006\014function-param\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\bfunction\226\128\157\190\n\004func\218\244\134\182\012D\138\233\142\251\014>\210\203\242$\025\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\004piqi\218\164\238\191\004\tpiqi-list\226\128\157\190\n\011piqi_bundle\218\244\134\182\012Y\138\233\142\251\014S\210\203\242$5\232\146\150q\216\210\153\r\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006module\210\171\158\194\006\004word\226\128\157\190\n\007modname\218\164\238\191\004\007include\226\128\157\190\n\006includ\218\244\134\182\012\190\001\170\136\200\184\014\183\001\218\164\238\191\004\014function-param\170\183\218\222\005\019\232\146\150q\150\201\251\143\001\210\171\158\194\006\004name\170\183\218\222\005\021\232\146\150q\162\218\227\222\003\210\171\158\194\006\006record\170\183\218\222\005\022\232\146\150q\138\130\146\206\003\210\171\158\194\006\007variant\170\183\218\222\005\018\232\146\150q\130\172\1791\210\171\158\194\006\004enum\170\183\218\222\005\028\232\146\150q\188\241\152{\210\171\158\194\006\004list\226\128\157\190\n\004list\170\183\218\222\005\019\232\146\150q\160\198\138\025\210\171\158\194\006\005alias\218\244\134\182\012\228\001\138\233\142\251\014\221\001\210\203\242$0\232\146\150q\136\141\189\239\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\004what\210\171\158\194\006\rextend-target\210\203\242$!\232\146\150q\152\137\193\146\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\boverride\210\203\242$:\232\146\150q\140\216\195\239\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\004with\210\171\158\194\006\bpiqi-any\226\128\157\190\n\tpiqi_with\210\203\242$2\232\146\150q\224\153\247\220\002\152\247\223\136\002\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\bpiqi-any\226\128\157\190\n\005quote\218\164\238\191\004\006extend\218\244\134\182\012\253\001\170\136\200\184\014\246\001\218\164\238\191\004\rextend-target\170\183\218\222\005 \232\146\150q\150\221\193\141\003\218\164\238\191\004\007typedef\210\171\158\194\006\004name\170\183\218\222\005\025\232\146\150q\150\201\251\143\001\152\247\223\136\002\001\210\171\158\194\006\004name\170\183\218\222\005\029\232\146\150q\244\210\156\t\218\164\238\191\004\005field\210\171\158\194\006\004name\170\183\218\222\005\031\232\146\150q\234\205\214\183\001\218\164\238\191\004\006option\210\171\158\194\006\004name\170\183\218\222\005\031\232\146\150q\202\133\149\136\001\218\164\238\191\004\006import\210\171\158\194\006\004name\170\183\218\222\005+\232\146\150q\176\172\149\197\002\218\164\238\191\004\bfunction\210\171\158\194\006\004name\226\128\157\190\n\004func"
  in parse_piqi_binobj piqi_lang_binobj
  
let piqi_spec =
  let piqi_spec_binobj =
    "\226\202\2304\004piqi\218\244\134\182\012H\170\136\200\184\014B\218\164\238\191\004\npiq-format\170\183\218\222\005\019\232\146\150q\148\135\232\239\001\218\164\238\191\004\004word\170\183\218\222\005\019\232\146\150q\218\178\206\207\001\218\164\238\191\004\004text\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\014protobuf-int32\226\195\252\217\004\005int32\128\228\138\244\005\249\179\220\210\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\014protobuf-int64\226\195\252\217\004\005int64\128\228\138\244\005\249\179\220\210\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int64\218\244\134\182\012\149\002\138\176\205\197\001\142\002\218\164\238\191\004\018protobuf-wire-type\170\183\218\222\005\021\232\146\150q\208\225\169\186\002\218\164\238\191\004\006varint\170\183\218\222\005\027\232\146\150q\154\229\206^\218\164\238\191\004\rzigzag-varint\170\183\218\222\005\022\232\146\150q\166\172\211\130\001\218\164\238\191\004\007fixed32\170\183\218\222\005\022\232\146\150q\228\182\211\130\001\218\164\238\191\004\007fixed64\170\183\218\222\005\028\232\146\150q\242\231\184\165\003\218\164\238\191\004\rsigned-varint\170\183\218\222\005\029\232\146\150q\196\161\239\209\003\218\164\238\191\004\014signed-fixed32\170\183\218\222\005\029\232\146\150q\130\172\239\209\003\218\164\238\191\004\014signed-fixed64\170\183\218\222\005\020\232\146\150q\154\213\227\207\002\218\164\238\191\004\005block\218\244\134\182\012\024\130\153\170d\019\218\164\238\191\004\004bool\176\171\195\244\005\170\136\238\b\218\244\134\182\012\027\130\153\170d\022\218\164\238\191\004\006string\176\171\195\244\005\209\209\192\137\001\218\244\134\182\012\026\130\153\170d\021\218\164\238\191\004\006binary\176\171\195\244\005\129\248\174h\218\244\134\182\012\028\130\153\170d\023\218\164\238\191\004\bpiqi-any\176\171\195\244\005\236\245\167\002\218\244\134\182\012,\130\153\170d'\218\164\238\191\004\003int\226\195\252\217\004\006sint32\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\244\134\182\012.\130\153\170d)\218\164\238\191\004\004uint\226\195\252\217\004\006uint32\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\244\134\182\012.\130\153\170d)\218\164\238\191\004\005int32\226\195\252\217\004\006sint32\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\244\134\182\0120\130\153\170d+\218\164\238\191\004\006uint32\226\195\252\217\004\006uint32\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\244\134\182\012.\130\153\170d)\218\164\238\191\004\005int64\226\195\252\217\004\006sint64\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\244\134\182\0120\130\153\170d+\218\164\238\191\004\006uint64\226\195\252\217\004\006uint64\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\244\134\182\0120\130\153\170d+\218\164\238\191\004\007float64\226\195\252\217\004\006double\128\228\138\244\005\178\219\169A\176\171\195\244\005\156\139\219\020\218\244\134\182\012/\130\153\170d*\218\164\238\191\004\007float32\226\195\252\217\004\005float\128\228\138\244\005\147\214\169A\176\171\195\244\005\156\139\219\020\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\011int32-fixed\226\195\252\217\004\bsfixed32\128\228\138\244\005\226\208\247\232\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\012uint32-fixed\226\195\252\217\004\007fixed32\128\228\138\244\005\147\214\169A\176\171\195\244\005\239\153\192\002\210\171\158\194\006\006uint32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\011int64-fixed\226\195\252\217\004\bsfixed64\128\228\138\244\005\129\214\247\232\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int64\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\012uint64-fixed\226\195\252\217\004\007fixed64\128\228\138\244\005\178\219\169A\176\171\195\244\005\239\153\192\002\210\171\158\194\006\006uint64\218\244\134\182\012&\130\153\170d!\218\164\238\191\004\005float\176\171\195\244\005\156\139\219\020\210\171\158\194\006\007float64\218\244\134\182\012'\130\153\170d\"\218\164\238\191\004\004word\210\171\158\194\006\006string\226\156\170\236\b\006\208\156\160\191\007\001\218\244\134\182\012\025\130\153\170d\020\218\164\238\191\004\004name\210\171\158\194\006\004word\218\244\134\182\012\166\001\170\136\200\184\014\159\001\234\188\204\215\002\012piqi_typedef\218\164\238\191\004\007typedef\170\183\218\222\005\021\232\146\150q\162\218\227\222\003\210\171\158\194\006\006record\170\183\218\222\005\022\232\146\150q\138\130\146\206\003\210\171\158\194\006\007variant\170\183\218\222\005\018\232\146\150q\130\172\1791\210\171\158\194\006\004enum\170\183\218\222\005\019\232\146\150q\160\198\138\025\210\171\158\194\006\005alias\170\183\218\222\005\018\232\146\150q\188\241\152{\210\171\158\194\006\004list\218\244\134\182\012\187\001\138\176\205\197\001\180\001\218\164\238\191\004\tpiqi-type\170\183\218\222\005\017\232\146\150q\222\179\128\005\218\164\238\191\004\003int\170\183\218\222\005\019\232\146\150q\184\150\182)\218\164\238\191\004\005float\170\183\218\222\005\018\232\146\150q\212\144\220\017\218\164\238\191\004\004bool\170\183\218\222\005\021\232\146\150q\162\163\129\147\002\218\164\238\191\004\006string\170\183\218\222\005\021\232\146\150q\130\240\221\208\001\218\164\238\191\004\006binary\170\183\218\222\005\017\232\146\150q\216\235\207\004\218\164\238\191\004\003any\162\249\213\245\n\npiqi_type_\218\244\134\182\012\025\130\153\170d\020\218\164\238\191\004\004type\210\171\158\194\006\004name\218\244\134\182\012\248\001\138\233\142\251\014\241\001\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\210\156\t\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\005field\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\218\164\238\191\004\006record\218\244\134\182\012\219\004\138\233\142\251\014\212\004\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$S\232\146\150q\198\205\134\134\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004mode\210\171\158\194\006\nfield-mode\138\140\251\240\r \218\148\211\024\006\b\223\162\138\147\001\210\171\158\194\006\015piqi/field-mode\210\203\242$.\232\146\150q\130\227\158\188\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007default\210\171\158\194\006\bpiqi-any\210\203\242$\"\232\146\150q\230\253\151B\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeprecated\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$'\232\146\150q\218\196\165\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004code\210\171\158\194\006\005int32\210\203\242$(\232\146\150q\244\181\193\171\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-packed\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$0\232\146\150q\172\148\156\205\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rgetopt-letter\210\171\158\194\006\004word\210\203\242$/\232\146\150q\144\177\235\165\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ngetopt-doc\210\171\158\194\006\006string\218\164\238\191\004\005field\218\244\134\182\012m\138\176\205\197\001g\218\164\238\191\004\nfield-mode\170\183\218\222\005\023\232\146\150q\190\197\148\166\002\218\164\238\191\004\brequired\170\183\218\222\005\023\232\146\150q\192\190\245\230\003\218\164\238\191\004\boptional\170\183\218\222\005\023\232\146\150q\244\241\173\133\002\218\164\238\191\004\brepeated\218\244\134\182\012\251\001\138\233\142\251\014\244\001\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\031\232\146\150q\234\205\214\183\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006option\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\218\164\238\191\004\007variant\218\244\134\182\012\164\003\138\233\142\251\014\157\003\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$\"\232\146\150q\230\253\151B\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeprecated\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$'\232\146\150q\218\196\165\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004code\210\171\158\194\006\005int32\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$0\232\146\150q\172\148\156\205\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rgetopt-letter\210\171\158\194\006\004word\210\203\242$/\232\146\150q\144\177\235\165\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ngetopt-doc\210\171\158\194\006\006string\218\164\238\191\004\006option\218\244\134\182\012\192\002\138\233\142\251\014\185\002\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\031\232\146\150q\234\205\214\183\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006option\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$4\232\146\150q\168\190\181\221\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-prefix\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\234\188\204\215\002\tpiqi_enum\218\164\238\191\004\004enum\218\244\134\182\012\245\002\138\233\142\251\014\238\002\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$\"\232\146\150q\236\234\144\189\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\tpiqi-type\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$2\232\146\150q\248\144\191\150\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-type\210\171\158\194\006\006string\210\203\242$+\232\146\150q\128\217\130\189\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\018protobuf-wire-type\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\218\164\238\191\004\005alias\218\244\134\182\012\203\002\138\233\142\251\014\196\002\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004type\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$(\232\146\150q\244\181\193\171\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-packed\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\218\164\238\191\004\004list\218\244\134\182\012\159\002\138\233\142\251\014\152\002\210\203\242$(\232\146\150q\216\210\153\r\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006module\210\171\158\194\006\004word\210\203\242$ \232\146\150q\150\221\193\141\003\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\007typedef\210\203\242$\031\232\146\150q\202\133\149\136\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006import\210\203\242$!\232\146\150q\176\172\149\197\002\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\bfunction\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$5\232\146\150q\136\221\228\230\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016protobuf-package\210\171\158\194\006\006string\218\164\238\191\004\004piqi\218\244\134\182\012a\138\233\142\251\014[\210\203\242$(\232\146\150q\216\210\153\r\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006module\210\171\158\194\006\004word\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\218\164\238\191\004\006import\218\244\134\182\012\200\001\138\233\142\251\014\193\001\210\203\242$)\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004type\210\171\158\194\006\006string\210\203\242$,\232\146\150q\150\229\148\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bprotobuf\210\171\158\194\006\006binary\210\203\242$(\232\146\150q\208\136\194f\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004json\210\171\158\194\006\006string\210\203\242$'\232\146\150q\174\183\219\005\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003xml\210\171\158\194\006\006string\218\164\238\191\004\003any\218\244\134\182\012\191\001\138\233\142\251\014\184\001\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$(\232\146\150q\148\144\238\225\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005input\210\171\158\194\006\004type\210\203\242$)\232\146\150q\130\188\136\200\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006output\210\171\158\194\006\004type\210\203\242$(\232\146\150q\144\175\206\178\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005error\210\171\158\194\006\004type\218\164\238\191\004\bfunction\218\244\134\182\0123\138\233\142\251\014-\210\203\242$\025\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\004piqi\218\164\238\191\004\tpiqi-list"
  in parse_piqi_binobj piqi_spec_binobj
  

