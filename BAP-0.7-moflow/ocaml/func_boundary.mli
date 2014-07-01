(** Function boundary identification for x86

    @author Tiffany (Youzhi) Bao
*)
open Asmir
open Type
open Ast

(** [start_addresses p] identifies a list of function start addresses
    in [p] using heuristics. Raises [Invalid_argument] if called on a
    non-x86 program. *)
val start_addresses : asmprogram -> addr list

val get_function_ranges : asmprogram -> (string * addr * addr) list
