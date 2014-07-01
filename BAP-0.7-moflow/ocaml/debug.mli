(** Debugging module.

    This module contains functions for printing debugging output.

    Debugging output is controlled via environment variables:
    - [BAP_LOGFILE]
    If set, debug messages will be written to the file
    $BAP_LOGFILE. The default is to write to standard error. Alternatively,
    the set_logfile function changes the file at run-time.

    - [BAP_DEBUG_MODULES]
    Specifies which modules should have debugging enabled or disabled. The list
    is separated by colons ([:]). {i modulename} or [!]{i modulename} turn
    debugging on or off respectively for {i modulename}. If {i modulename} is
    the empty string, it matches all modules. The leftmost match wins. Default
    behavior is undefined and subject to change. Note that {i modulename}
    refers to the name used for debug printing, which is not necesarily
    identical to the module name in OCaml.

    - [BAP_WARN_MODULES]
    Specifies which modules should have warning enabled or
    disabled. The format is like that of [BAP_DEBUG_MODULES].
    Default behavior is on (unlike debugging).

    - [BAP_DEBUG_TIMESTAMPS]
    Specified how (and whether) timestamps will be printed with each
    debug message. Supported values are [unix], [iso], [elapsed], and [none].

    @author David Brumley
*)

val set_logfile : out_channel -> unit
(** Sets the file to which the debug messages will be written *)

(** A DEBUG module is used by client code to add debugging code that
    can be easily enabled or disabled through environment variables. *)
module type DEBUG =
sig
  val debug : unit -> bool
  (** Returns [true] iff debugging is enabled *)

  val warn : bool
  (** Returns [true] iff warnings are enabled *)

  val pdebug : string -> unit
  (** Prints given string as a debug message. *)

  val dprintf : ('a, unit, string, unit) format4 -> 'a
  (** Prints given format string as a debug message. *)

  val dtrace : before:('a->unit) -> f:('a->'b) -> after:('b->unit) -> 'a -> 'b
    (** [dtrace before f after] will return [f()], but will call
	[before] first and after calling [f] it will pass the result
	to [after]. Also, while [f] is running, indentation for all
	debugging functions will be increased.  *)

  val pwarn : string -> unit
  (** Prints given string as a warning message. *)

  val wprintf : ('a, unit, string, unit) format4 -> 'a
  (** Prints given format string as a warning message. *)
end

(** Input information to make a DEBUG module *)
module type DEBUG_MOD_INFO =
sig
  val name : string
  (** The name to prefix all debug and warning messages with *)

  val default : [ `Debug | `NoDebug ]
  (** Specifies whether debugging is enabled or disabled by default. *)
end

(** To force debugging off, use the NoDebug module in place of a regluar DEBUG module *)
module NoDebug : DEBUG

(** Functor to create a DEBUG module.
**)
module Make : functor (Module : DEBUG_MOD_INFO) -> DEBUG

(* {3 Sample Code} *)

(**
    Sample code to create a DEBUG module for the "Foo" module that enables
    debugging by default:

{[module D = Debug.Make(struct let name = "Foo" and default = `Debug end)
open D]}
*)
