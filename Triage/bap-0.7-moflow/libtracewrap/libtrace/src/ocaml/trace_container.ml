(** $Id: trace_container.ml 6415 2012-06-29 01:42:57Z edmcman $
    Trace container implementation.

    Bugs:
    We keep track of things as Int64's, but ML's IO only uses ints.
*)

exception TraceException of string

type frame = Frame_piqi.frame

(* XXX: Auto-pull this from C++ header file *)
let default_frames_per_toc_entry = 10000L
and default_auto_finish = false
and default_arch = Arch.Bfd_arch_i386
and default_machine = 0L

(* Internal definitions *)
let magic_number = 7456879624156307493L
and magic_number_offset = 0L
and trace_version_offset = 8L
and bfd_arch_offset = 16L
and bfd_machine_offset = 24L
and num_trace_frames_offset = 32L
and toc_offset_offset = 40L
and first_frame_offset = 48L

let out_trace_version = 1L
and lowest_supported_version = 1L
and highest_supported_version = 1L

let write_i64 oc i64 =
  let output = BatIO.output_string () in
  let () = BatIO.write_i64 output i64 in
  let binary = BatIO.close_out output in
  output_string oc binary

let read_i64 ic =
  let s = String.create 8 in
  (* Read 8 bytes into s *)
  let () = really_input ic s 0 8 in
  let input = BatIO.input_string s in
  let i = BatIO.read_i64 input in
  let () = BatIO.close_in input in
  i

(** [foldn f i n] is f (... (f (f i n) (n-1)) ...) 0 *)
let rec foldn64 ?(t=0L) f i n =
  let (-) = Int64.sub in
  match n-t with
  | 0L -> f i n
  | _ when n>t -> foldn64 ~t f (f i n) (n-1L)
  | n when n == -1L -> i (* otags has trouble with '-1L' *)
  | w -> raise (Invalid_argument "negative index number in foldn64")

(* End helpers *)

class writer ?(arch=default_arch) ?(machine=default_machine) ?(frames_per_toc_entry = default_frames_per_toc_entry) ?(auto_finish=default_auto_finish) filename =
  (* Open the trace file *)
  let oc = open_out_bin filename in
  (* Seek to the first frame *)
  let () = LargeFile.seek_out oc first_frame_offset in
object(self)

  val mutable toc = []
  val mutable num_frames = 0L
  val frames_per_toc_entry = frames_per_toc_entry
  val auto_finish = auto_finish
  val mutable is_finished = false

  initializer Gc.finalise (fun self -> if not self#has_finished then self#finish) self

  method add (frame:frame) =
    if num_frames <> 0L && Int64.rem num_frames frames_per_toc_entry = 0L then
      (* Put a toc entry *)
      toc <- (LargeFile.pos_out oc) :: toc;

    let () = num_frames <- Int64.succ num_frames in

    (* Convert to string so we know length *)
    let s = Frame_piqi_ext.gen_frame frame `pb in
    let len = Int64.of_int (String.length s) in
    if len <= 0L then
      raise (TraceException "Attempt to add zero-length frame to trace");

    (* Write the length in binary *)
    let () = write_i64 oc len in

    let old_offset = LargeFile.pos_out oc in
    (* Finally write the serialized string out. *)
    let () = output_string oc s in

    (* Double-check our size. *)
    assert ((Int64.add old_offset len)
        = (LargeFile.pos_out oc));

  method finish =
    if is_finished then raise (TraceException "finish called twice");

    let toc_offset = LargeFile.pos_out oc in
    (* Make sure the toc is the right size. *)
    let () = assert ((num_frames = 0L) || (Int64.div (Int64.pred num_frames) frames_per_toc_entry) = Int64.of_int (List.length toc)) in
    (* Write frames per toc entry. *)
    let () = write_i64 oc frames_per_toc_entry in
    (* Write toc to file. *)
    let () = List.iter (fun offset -> write_i64 oc offset) (List.rev toc) in
    (* Now we need to write the magic number, number of trace frames,
       and the offset of field m at the start of the trace. *)
    (* Magic number. *)
    let () = LargeFile.seek_out oc magic_number_offset in
    let () = write_i64 oc magic_number in
    (* Trace version. *)
    let () = LargeFile.seek_out oc trace_version_offset in
    let () = write_i64 oc out_trace_version in
    (* CPU architecture. *)
    let () = LargeFile.seek_out oc bfd_arch_offset in
    (* Goodbye type safety! *)
    let () = write_i64 oc (Int64.of_int (Obj.magic arch)) in
    (* Machine type. *)
    let () = LargeFile.seek_out oc bfd_machine_offset in
    let () = write_i64 oc machine in
    (* Number of trace frames. *)
    let () = LargeFile.seek_out oc num_trace_frames_offset in
    let () = write_i64 oc num_frames in
    (* Offset of toc. *)
    let () = LargeFile.seek_out oc toc_offset_offset in
    let () = write_i64 oc toc_offset in
    (* Finally close the final and mark us as finished. *)
    let () = close_out oc in
    is_finished <- true

    method has_finished = is_finished

end

class reader filename =
  let ic = open_in_bin filename in
  (* Verify magic number *)
  let () = if read_i64 ic <> magic_number then
      raise (TraceException "Magic number is incorrect") in
  (* Trace version *)
  let trace_version = read_i64 ic in
  let () = if trace_version < lowest_supported_version ||
      trace_version > highest_supported_version then
    raise (TraceException "Unsupported trace version") in
  (* Read arch type, break type safety *)
  let archnum = read_i64 ic in
  let () = if not (archnum < (Int64.of_int (Obj.magic Arch.Bfd_arch_last))) then
      raise (TraceException "Invalid architecture") in
  let arch : Arch.bfd_architecture = Obj.magic (Int64.to_int archnum) in
  let machine = read_i64 ic in
  (* Read number of trace frames. *)
  let num_frames = read_i64 ic in
  (* Find offset of toc. *)
  let toc_offset = read_i64 ic in
  (* Find the toc. *)
  let () = LargeFile.seek_in ic toc_offset in
  (* Read number of frames per toc entry. *)
  let frames_per_toc_entry = read_i64 ic in
  (* Read each toc entry. *)
  let toc_size = Int64.div (Int64.pred num_frames) frames_per_toc_entry in
  let toc =
    let toc_rev = foldn64
      (fun acc n ->
        if n = 0L then acc else
          (read_i64 ic) :: acc
      ) [] toc_size in
    Array.of_list (List.rev toc_rev)
  in
  (* We should be at the end of the file now. *)
  let () = assert ((LargeFile.pos_in ic) = (LargeFile.in_channel_length ic)) in
  object(self)
    val mutable current_frame = 0L

    method get_num_frames = num_frames

    method get_frames_per_toc_entry = frames_per_toc_entry

    method get_arch = arch

    method get_machine = machine

    method get_trace_version = trace_version

    method seek frame_number =
      (* First, make sure the frame is in range. *)
      let () = self#check_end_of_trace_num frame_number "seek to non-existant frame" in

      (* Find the closest toc entry, if any. *)
      let toc_number = Int64.div frame_number frames_per_toc_entry in

      current_frame <- (match toc_number with
      | 0L -> let () = LargeFile.seek_in ic first_frame_offset in
              0L
      | _ -> let () = LargeFile.seek_in ic (Array.get toc (Int64.to_int (Int64.pred toc_number))) in
             Int64.mul toc_number frames_per_toc_entry);

      while current_frame <> frame_number do
        (* Read frame length and skip that far ahead. *)
        let frame_len = read_i64 ic in
        let () = LargeFile.seek_in ic (Int64.add (LargeFile.pos_in ic) frame_len) in
        current_frame <- Int64.succ current_frame
      done

    method get_frame : frame =
      let () = self#check_end_of_trace "get_frame on non-existant frame" in
      let frame_len = read_i64 ic in
      if (frame_len <= 0L) then
        raise (TraceException (Printf.sprintf "Read zero-length frame at offset %#Lx" (LargeFile.pos_in ic)));
      let buf = String.create (Int64.to_int frame_len) in
      (* Read the frame info buf. *)
      let () = really_input ic buf 0 (Int64.to_int frame_len) in
      let f = Frame_piqi.parse_frame (Piqirun.init_from_string buf) in
      let () = current_frame <- Int64.succ current_frame in
      f

    method get_frames requested_frames =
      let () = self#check_end_of_trace "get_frame on non-existant frame" in
      (* The number of frames we copy is bounded by the number of
         frames left in the trace. *)
      let 
          num_frames = min requested_frames (Int64.sub num_frames current_frame)
      in
      List.rev (foldn64 ~t:1L (fun l n -> self#get_frame :: l) [] num_frames)

    method end_of_trace =
      self#end_of_trace_num current_frame

    method private end_of_trace_num frame_num =
      if Int64.succ frame_num > num_frames then
        true
      else
        false

    method private check_end_of_trace_num frame_num msg =
      if self#end_of_trace_num frame_num then
        raise (TraceException msg)

    method private check_end_of_trace msg =
      if self#end_of_trace then
        raise (TraceException msg)

    initializer self#seek 0L
  end
