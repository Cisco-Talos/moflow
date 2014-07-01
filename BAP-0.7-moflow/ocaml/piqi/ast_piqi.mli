(** Convert AST programs to the Piqi serialization format, which can
    convert to protobuffers, xml, and json.

    @author Edward J. Schwartz
 *)

(** [to_pb p] converts [p] to protobuffer format. *)
val to_pb : Ast.program -> string

(** [to_json p] converts [p] to JSON format. *)
val to_json : Ast.program -> string

(** [to_xml p] converts [p] to XML format. *)
val to_xml : Ast.program -> string
