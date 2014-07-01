exception TraceException of string

val default_frames_per_toc_entry : int64
val default_auto_finish : bool

type frame = Frame_piqi.frame

class writer : ?arch : Arch.bfd_architecture -> ?machine:int64 -> ?frames_per_toc_entry : int64 -> ?auto_finish : bool -> string ->
object
  method add : frame -> unit
  method finish : unit
  method has_finished : bool
end

class reader : string ->
object
  method get_num_frames : int64
  method get_frames_per_toc_entry : int64
  method get_arch : Arch.bfd_architecture
  method get_machine : int64
  method get_trace_version : int64
  method seek : int64 -> unit
  method get_frame : frame
  method get_frames : int64 -> frame list
  method end_of_trace : bool
  method close_fd : unit -> unit
end
