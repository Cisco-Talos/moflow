module rec Frame_piqi :
             sig
               type uint64 = int64
               
               type binary = string
               
               type thread_id = Frame_piqi.uint64
               
               type address = Frame_piqi.uint64
               
               type bit_length = int
               
               type taint_id = Frame_piqi.uint64
               
               type exception_number = Frame_piqi.uint64
               
               type argument = int64
               
               type frame =
                 [
                   | `std_frame of Frame_piqi.std_frame
                   | `syscall_frame of Frame_piqi.syscall_frame
                   | `exception_frame of Frame_piqi.exception_frame
                   | `taint_intro_frame of Frame_piqi.taint_intro_frame
                   | `modload_frame of Frame_piqi.modload_frame
                   | `key_frame of Frame_piqi.key_frame
                   | `block_frame of Frame_piqi.block_frame
                   | `call_frame of Frame_piqi.call_frame
                   | `ret_frame of Frame_piqi.ret_frame
                 ]
               
               type operand_info_specific =
                 [
                   | `mem_operand of Frame_piqi.mem_operand
                   | `reg_operand of Frame_piqi.reg_operand
                 ]
               
               type taint_info =
                 [
                   | `no_taint
                   | `taint_id of Frame_piqi.taint_id
                   | `taint_multiple
                 ]
               
               type value_source_tag =
                 [ | `no_thread_id | `thread_id of Frame_piqi.thread_id
                 ]
               
               type operand_list = Frame_piqi.operand_info list
               
               type operand_info = Operand_info.t
               
               type reg_operand = Reg_operand.t
               
               type mem_operand = Mem_operand.t
               
               type operand_usage = Operand_usage.t
               
               type std_frame = Std_frame.t
               
               type syscall_frame = Syscall_frame.t
               
               type argument_list = Frame_piqi.argument list
               
               type exception_frame = Exception_frame.t
               
               type taint_intro_frame = Taint_intro_frame.t
               
               type taint_intro_list = Frame_piqi.taint_intro list
               
               type taint_intro = Taint_intro.t
               
               type modload_frame = Modload_frame.t
               
               type key_frame = Key_frame.t
               
               type tagged_value_lists = Frame_piqi.tagged_value_list list
               
               type tagged_value_list = Tagged_value_list.t
               
               type value_list = Frame_piqi.value_info list
               
               type value_info = Value_info.t
               
               type block_frame = Block_frame.t
               
               type call_frame = Call_frame.t
               
               type ret_frame = Ret_frame.t
               
             end = Frame_piqi
and
  Operand_info :
    sig
      type t =
        { mutable operand_info_specific : Frame_piqi.operand_info_specific;
          mutable bit_length : Frame_piqi.bit_length;
          mutable operand_usage : Frame_piqi.operand_usage;
          mutable taint_info : Frame_piqi.taint_info;
          mutable value : Frame_piqi.binary
        }
      
    end = Operand_info
and Reg_operand : sig type t = { mutable name : string }
                       end = Reg_operand
and
  Mem_operand : sig type t = { mutable address : Frame_piqi.address }
                     end =
    Mem_operand
and
  Operand_usage :
    sig
      type t =
        { mutable read : bool; mutable written : bool; mutable index : bool;
          mutable base : bool
        }
      
    end = Operand_usage
and
  Std_frame :
    sig
      type t =
        { mutable address : Frame_piqi.address;
          mutable thread_id : Frame_piqi.thread_id;
          mutable rawbytes : Frame_piqi.binary;
          mutable operand_list : Frame_piqi.operand_list
        }
      
    end = Std_frame
and
  Syscall_frame :
    sig
      type t =
        { mutable address : Frame_piqi.address;
          mutable thread_id : Frame_piqi.thread_id;
          mutable number : Frame_piqi.uint64;
          mutable argument_list : Frame_piqi.argument_list
        }
      
    end = Syscall_frame
and
  Exception_frame :
    sig
      type t =
        { mutable exception_number : Frame_piqi.exception_number;
          mutable thread_id : Frame_piqi.thread_id option;
          mutable from_addr : Frame_piqi.address option;
          mutable to_addr : Frame_piqi.address option
        }
      
    end = Exception_frame
and
  Taint_intro_frame :
    sig type t = { mutable taint_intro_list : Frame_piqi.taint_intro_list }
        
    end = Taint_intro_frame
and
  Taint_intro :
    sig
      type t =
        { mutable addr : Frame_piqi.address;
          mutable taint_id : Frame_piqi.taint_id;
          mutable value : Frame_piqi.binary option;
          mutable source_name : string; mutable offset : Frame_piqi.address
        }
      
    end = Taint_intro
and
  Modload_frame :
    sig
      type t =
        { mutable module_name : string;
          mutable low_address : Frame_piqi.address;
          mutable high_address : Frame_piqi.address
        }
      
    end = Modload_frame
and
  Key_frame :
    sig
      type t = { mutable tagged_value_lists : Frame_piqi.tagged_value_lists }
      
    end = Key_frame
and
  Tagged_value_list :
    sig
      type t =
        { mutable value_source_tag : Frame_piqi.value_source_tag;
          mutable value_list : Frame_piqi.value_list
        }
      
    end = Tagged_value_list
and
  Value_info :
    sig
      type t =
        { mutable operand_info_specific : Frame_piqi.operand_info_specific;
          mutable bit_length : Frame_piqi.bit_length;
          mutable taint_info : Frame_piqi.taint_info option;
          mutable value : Frame_piqi.binary
        }
      
    end = Value_info
and
  Block_frame :
    sig
      type t =
        { mutable address : Frame_piqi.address;
          mutable thread_id : Frame_piqi.thread_id
        }
      
    end = Block_frame
and
  Call_frame :
    sig
      type t =
        { mutable address : Frame_piqi.address;
          mutable target : Frame_piqi.address;
          mutable thread_id : Frame_piqi.thread_id
        }
      
    end = Call_frame
and
  Ret_frame :
    sig
      type t =
        { mutable address : Frame_piqi.address;
          mutable target : Frame_piqi.address;
          mutable thread_id : Frame_piqi.thread_id
        }
      
    end = Ret_frame
  
include Frame_piqi
  
let rec parse_uint64 x = Piqirun.int64_of_varint x
and packed_parse_uint64 x = Piqirun.int64_of_packed_varint x
and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x
and parse_binary x = Piqirun.string_of_block x
and parse_string x = Piqirun.string_of_block x
and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x
and parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x
and parse_frame x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 1 -> let res = parse_std_frame x in `std_frame res
    | 2 -> let res = parse_syscall_frame x in `syscall_frame res
    | 3 -> let res = parse_exception_frame x in `exception_frame res
    | 4 -> let res = parse_taint_intro_frame x in `taint_intro_frame res
    | 5 -> let res = parse_modload_frame x in `modload_frame res
    | 6 -> let res = parse_key_frame x in `key_frame res
    | 7 -> let res = parse_block_frame x in `block_frame res
    | 8 -> let res = parse_call_frame x in `call_frame res
    | 9 -> let res = parse_ret_frame x in `ret_frame res
    | _ -> Piqirun.error_variant x code
and parse_thread_id x = parse_uint64 x
and packed_parse_thread_id x = packed_parse_uint64 x
and parse_address x = parse_uint64 x
and packed_parse_address x = packed_parse_uint64 x
and parse_bit_length x = parse_int x
and packed_parse_bit_length x = packed_parse_int x
and parse_taint_id x = parse_uint64 x
and packed_parse_taint_id x = packed_parse_uint64 x
and parse_exception_number x = parse_uint64 x
and packed_parse_exception_number x = packed_parse_uint64 x
and parse_operand_list x = Piqirun.parse_list parse_operand_info x
and parse_operand_info x =
  let x = Piqirun.parse_record x in
  let (_operand_info_specific, x) =
    Piqirun.parse_required_field 1 parse_operand_info_specific x in
  let (_bit_length, x) = Piqirun.parse_required_field 2 parse_bit_length x in
  let (_operand_usage, x) =
    Piqirun.parse_required_field 3 parse_operand_usage x in
  let (_taint_info, x) = Piqirun.parse_required_field 4 parse_taint_info x in
  let (_value, x) = Piqirun.parse_required_field 5 parse_binary x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Operand_info.operand_info_specific = _operand_info_specific;
       Operand_info.bit_length = _bit_length;
       Operand_info.operand_usage = _operand_usage;
       Operand_info.taint_info = _taint_info;
       Operand_info.value = _value;
     })
and parse_operand_info_specific x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 1 -> let res = parse_mem_operand x in `mem_operand res
    | 2 -> let res = parse_reg_operand x in `reg_operand res
    | _ -> Piqirun.error_variant x code
and parse_reg_operand x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_required_field 1 parse_string x
  in (Piqirun.check_unparsed_fields x; { Reg_operand.name = _name; })
and parse_mem_operand x =
  let x = Piqirun.parse_record x in
  let (_address, x) = Piqirun.parse_required_field 1 parse_address x
  in (Piqirun.check_unparsed_fields x; { Mem_operand.address = _address; })
and parse_operand_usage x =
  let x = Piqirun.parse_record x in
  let (_read, x) = Piqirun.parse_required_field 1 parse_bool x in
  let (_written, x) = Piqirun.parse_required_field 2 parse_bool x in
  let (_index, x) = Piqirun.parse_required_field 3 parse_bool x in
  let (_base, x) = Piqirun.parse_required_field 4 parse_bool x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Operand_usage.read = _read;
       Operand_usage.written = _written;
       Operand_usage.index = _index;
       Operand_usage.base = _base;
     })
and parse_taint_info x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 1 when x = (Piqirun.Varint 1) -> `no_taint
    | 2 -> let res = parse_taint_id x in `taint_id res
    | 3 when x = (Piqirun.Varint 1) -> `taint_multiple
    | _ -> Piqirun.error_variant x code
and parse_std_frame x =
  let x = Piqirun.parse_record x in
  let (_address, x) = Piqirun.parse_required_field 1 parse_address x in
  let (_thread_id, x) = Piqirun.parse_required_field 2 parse_thread_id x in
  let (_rawbytes, x) = Piqirun.parse_required_field 3 parse_binary x in
  let (_operand_list, x) =
    Piqirun.parse_required_field 4 parse_operand_list x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Std_frame.address = _address;
       Std_frame.thread_id = _thread_id;
       Std_frame.rawbytes = _rawbytes;
       Std_frame.operand_list = _operand_list;
     })
and parse_syscall_frame x =
  let x = Piqirun.parse_record x in
  let (_address, x) = Piqirun.parse_required_field 1 parse_address x in
  let (_thread_id, x) = Piqirun.parse_required_field 2 parse_thread_id x in
  let (_number, x) = Piqirun.parse_required_field 3 parse_uint64 x in
  let (_argument_list, x) =
    Piqirun.parse_required_field 4 parse_argument_list x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Syscall_frame.address = _address;
       Syscall_frame.thread_id = _thread_id;
       Syscall_frame.number = _number;
       Syscall_frame.argument_list = _argument_list;
     })
and parse_argument_list x = Piqirun.parse_list parse_argument x
and parse_argument x = parse_int64 x
and packed_parse_argument x = packed_parse_int64 x
and parse_exception_frame x =
  let x = Piqirun.parse_record x in
  let (_exception_number, x) =
    Piqirun.parse_required_field 1 parse_exception_number x in
  let (_thread_id, x) = Piqirun.parse_optional_field 2 parse_thread_id x in
  let (_from_addr, x) = Piqirun.parse_optional_field 3 parse_address x in
  let (_to_addr, x) = Piqirun.parse_optional_field 4 parse_address x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Exception_frame.exception_number = _exception_number;
       Exception_frame.thread_id = _thread_id;
       Exception_frame.from_addr = _from_addr;
       Exception_frame.to_addr = _to_addr;
     })
and parse_taint_intro_frame x =
  let x = Piqirun.parse_record x in
  let (_taint_intro_list, x) =
    Piqirun.parse_required_field 1 parse_taint_intro_list x
  in
    (Piqirun.check_unparsed_fields x;
     { Taint_intro_frame.taint_intro_list = _taint_intro_list; })
and parse_taint_intro_list x = Piqirun.parse_list parse_taint_intro x
and parse_taint_intro x =
  let x = Piqirun.parse_record x in
  let (_addr, x) = Piqirun.parse_required_field 1 parse_address x in
  let (_taint_id, x) = Piqirun.parse_required_field 2 parse_taint_id x in
  let (_value, x) = Piqirun.parse_optional_field 3 parse_binary x in
  let (_source_name, x) = Piqirun.parse_required_field 4 parse_string x in
  let (_offset, x) = Piqirun.parse_required_field 5 parse_address x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Taint_intro.addr = _addr;
       Taint_intro.taint_id = _taint_id;
       Taint_intro.value = _value;
       Taint_intro.source_name = _source_name;
       Taint_intro.offset = _offset;
     })
and parse_modload_frame x =
  let x = Piqirun.parse_record x in
  let (_module_name, x) = Piqirun.parse_required_field 1 parse_string x in
  let (_low_address, x) = Piqirun.parse_required_field 2 parse_address x in
  let (_high_address, x) = Piqirun.parse_required_field 3 parse_address x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Modload_frame.module_name = _module_name;
       Modload_frame.low_address = _low_address;
       Modload_frame.high_address = _high_address;
     })
and parse_key_frame x =
  let x = Piqirun.parse_record x in
  let (_tagged_value_lists, x) =
    Piqirun.parse_required_field 1 parse_tagged_value_lists x
  in
    (Piqirun.check_unparsed_fields x;
     { Key_frame.tagged_value_lists = _tagged_value_lists; })
and parse_tagged_value_lists x = Piqirun.parse_list parse_tagged_value_list x
and parse_tagged_value_list x =
  let x = Piqirun.parse_record x in
  let (_value_source_tag, x) =
    Piqirun.parse_required_field 1 parse_value_source_tag x in
  let (_value_list, x) = Piqirun.parse_required_field 2 parse_value_list x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Tagged_value_list.value_source_tag = _value_source_tag;
       Tagged_value_list.value_list = _value_list;
     })
and parse_value_source_tag x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 1 when x = (Piqirun.Varint 1) -> `no_thread_id
    | 2 -> let res = parse_thread_id x in `thread_id res
    | _ -> Piqirun.error_variant x code
and parse_value_list x = Piqirun.parse_list parse_value_info x
and parse_value_info x =
  let x = Piqirun.parse_record x in
  let (_operand_info_specific, x) =
    Piqirun.parse_required_field 1 parse_operand_info_specific x in
  let (_bit_length, x) = Piqirun.parse_required_field 2 parse_bit_length x in
  let (_taint_info, x) = Piqirun.parse_optional_field 3 parse_taint_info x in
  let (_value, x) = Piqirun.parse_required_field 4 parse_binary x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Value_info.operand_info_specific = _operand_info_specific;
       Value_info.bit_length = _bit_length;
       Value_info.taint_info = _taint_info;
       Value_info.value = _value;
     })
and parse_block_frame x =
  let x = Piqirun.parse_record x in
  let (_address, x) = Piqirun.parse_required_field 1 parse_address x in
  let (_thread_id, x) = Piqirun.parse_required_field 2 parse_thread_id x
  in
    (Piqirun.check_unparsed_fields x;
     { Block_frame.address = _address; Block_frame.thread_id = _thread_id; })
and parse_call_frame x =
  let x = Piqirun.parse_record x in
  let (_address, x) = Piqirun.parse_required_field 1 parse_address x in
  let (_target, x) = Piqirun.parse_required_field 2 parse_address x in
  let (_thread_id, x) = Piqirun.parse_required_field 3 parse_thread_id x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Call_frame.address = _address;
       Call_frame.target = _target;
       Call_frame.thread_id = _thread_id;
     })
and parse_ret_frame x =
  let x = Piqirun.parse_record x in
  let (_address, x) = Piqirun.parse_required_field 1 parse_address x in
  let (_target, x) = Piqirun.parse_required_field 2 parse_address x in
  let (_thread_id, x) = Piqirun.parse_required_field 3 parse_thread_id x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Ret_frame.address = _address;
       Ret_frame.target = _target;
       Ret_frame.thread_id = _thread_id;
     })
  
let rec gen__uint64 code x = Piqirun.int64_to_varint code x
and packed_gen__uint64 x = Piqirun.int64_to_packed_varint x
and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x
and gen__binary code x = Piqirun.string_to_block code x
and gen__string code x = Piqirun.string_to_block code x
and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x
and gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x
and gen__frame code (x : Frame_piqi.frame) =
  Piqirun.gen_record code
    [ (match x with
       | `std_frame x -> gen__std_frame 1 x
       | `syscall_frame x -> gen__syscall_frame 2 x
       | `exception_frame x -> gen__exception_frame 3 x
       | `taint_intro_frame x -> gen__taint_intro_frame 4 x
       | `modload_frame x -> gen__modload_frame 5 x
       | `key_frame x -> gen__key_frame 6 x
       | `block_frame x -> gen__block_frame 7 x
       | `call_frame x -> gen__call_frame 8 x
       | `ret_frame x -> gen__ret_frame 9 x) ]
and gen__thread_id code x = gen__uint64 code x
and packed_gen__thread_id x = packed_gen__uint64 x
and gen__address code x = gen__uint64 code x
and packed_gen__address x = packed_gen__uint64 x
and gen__bit_length code x = gen__int code x
and packed_gen__bit_length x = packed_gen__int x
and gen__taint_id code x = gen__uint64 code x
and packed_gen__taint_id x = packed_gen__uint64 x
and gen__exception_number code x = gen__uint64 code x
and packed_gen__exception_number x = packed_gen__uint64 x
and gen__operand_list code x = Piqirun.gen_list gen__operand_info code x
and gen__operand_info code x =
  let _operand_info_specific =
    Piqirun.gen_required_field 1 gen__operand_info_specific
      x.Operand_info.operand_info_specific in
  let _bit_length =
    Piqirun.gen_required_field 2 gen__bit_length x.Operand_info.bit_length in
  let _operand_usage =
    Piqirun.gen_required_field 3 gen__operand_usage
      x.Operand_info.operand_usage in
  let _taint_info =
    Piqirun.gen_required_field 4 gen__taint_info x.Operand_info.taint_info in
  let _value = Piqirun.gen_required_field 5 gen__binary x.Operand_info.value
  in
    Piqirun.gen_record code
      [ _operand_info_specific; _bit_length; _operand_usage; _taint_info;
        _value ]
and gen__operand_info_specific code (x : Frame_piqi.operand_info_specific) =
  Piqirun.gen_record code
    [ (match x with
       | `mem_operand x -> gen__mem_operand 1 x
       | `reg_operand x -> gen__reg_operand 2 x) ]
and gen__reg_operand code x =
  let _name = Piqirun.gen_required_field 1 gen__string x.Reg_operand.name
  in Piqirun.gen_record code [ _name ]
and gen__mem_operand code x =
  let _address =
    Piqirun.gen_required_field 1 gen__address x.Mem_operand.address
  in Piqirun.gen_record code [ _address ]
and gen__operand_usage code x =
  let _read = Piqirun.gen_required_field 1 gen__bool x.Operand_usage.read in
  let _written =
    Piqirun.gen_required_field 2 gen__bool x.Operand_usage.written in
  let _index =
    Piqirun.gen_required_field 3 gen__bool x.Operand_usage.index in
  let _base = Piqirun.gen_required_field 4 gen__bool x.Operand_usage.base
  in Piqirun.gen_record code [ _read; _written; _index; _base ]
and gen__taint_info code (x : Frame_piqi.taint_info) =
  Piqirun.gen_record code
    [ (match x with
       | `no_taint -> Piqirun.gen_bool_field 1 true
       | `taint_id x -> gen__taint_id 2 x
       | `taint_multiple -> Piqirun.gen_bool_field 3 true) ]
and gen__std_frame code x =
  let _address =
    Piqirun.gen_required_field 1 gen__address x.Std_frame.address in
  let _thread_id =
    Piqirun.gen_required_field 2 gen__thread_id x.Std_frame.thread_id in
  let _rawbytes =
    Piqirun.gen_required_field 3 gen__binary x.Std_frame.rawbytes in
  let _operand_list =
    Piqirun.gen_required_field 4 gen__operand_list x.Std_frame.operand_list
  in
    Piqirun.gen_record code
      [ _address; _thread_id; _rawbytes; _operand_list ]
and gen__syscall_frame code x =
  let _address =
    Piqirun.gen_required_field 1 gen__address x.Syscall_frame.address in
  let _thread_id =
    Piqirun.gen_required_field 2 gen__thread_id x.Syscall_frame.thread_id in
  let _number =
    Piqirun.gen_required_field 3 gen__uint64 x.Syscall_frame.number in
  let _argument_list =
    Piqirun.gen_required_field 4 gen__argument_list
      x.Syscall_frame.argument_list
  in
    Piqirun.gen_record code [ _address; _thread_id; _number; _argument_list ]
and gen__argument_list code x = Piqirun.gen_list gen__argument code x
and gen__argument code x = gen__int64 code x
and packed_gen__argument x = packed_gen__int64 x
and gen__exception_frame code x =
  let _exception_number =
    Piqirun.gen_required_field 1 gen__exception_number
      x.Exception_frame.exception_number in
  let _thread_id =
    Piqirun.gen_optional_field 2 gen__thread_id x.Exception_frame.thread_id in
  let _from_addr =
    Piqirun.gen_optional_field 3 gen__address x.Exception_frame.from_addr in
  let _to_addr =
    Piqirun.gen_optional_field 4 gen__address x.Exception_frame.to_addr
  in
    Piqirun.gen_record code
      [ _exception_number; _thread_id; _from_addr; _to_addr ]
and gen__taint_intro_frame code x =
  let _taint_intro_list =
    Piqirun.gen_required_field 1 gen__taint_intro_list
      x.Taint_intro_frame.taint_intro_list
  in Piqirun.gen_record code [ _taint_intro_list ]
and gen__taint_intro_list code x = Piqirun.gen_list gen__taint_intro code x
and gen__taint_intro code x =
  let _addr = Piqirun.gen_required_field 1 gen__address x.Taint_intro.addr in
  let _taint_id =
    Piqirun.gen_required_field 2 gen__taint_id x.Taint_intro.taint_id in
  let _value =
    Piqirun.gen_optional_field 3 gen__binary x.Taint_intro.value in
  let _source_name =
    Piqirun.gen_required_field 4 gen__string x.Taint_intro.source_name in
  let _offset =
    Piqirun.gen_required_field 5 gen__address x.Taint_intro.offset
  in
    Piqirun.gen_record code
      [ _addr; _taint_id; _value; _source_name; _offset ]
and gen__modload_frame code x =
  let _module_name =
    Piqirun.gen_required_field 1 gen__string x.Modload_frame.module_name in
  let _low_address =
    Piqirun.gen_required_field 2 gen__address x.Modload_frame.low_address in
  let _high_address =
    Piqirun.gen_required_field 3 gen__address x.Modload_frame.high_address
  in Piqirun.gen_record code [ _module_name; _low_address; _high_address ]
and gen__key_frame code x =
  let _tagged_value_lists =
    Piqirun.gen_required_field 1 gen__tagged_value_lists
      x.Key_frame.tagged_value_lists
  in Piqirun.gen_record code [ _tagged_value_lists ]
and gen__tagged_value_lists code x =
  Piqirun.gen_list gen__tagged_value_list code x
and gen__tagged_value_list code x =
  let _value_source_tag =
    Piqirun.gen_required_field 1 gen__value_source_tag
      x.Tagged_value_list.value_source_tag in
  let _value_list =
    Piqirun.gen_required_field 2 gen__value_list
      x.Tagged_value_list.value_list
  in Piqirun.gen_record code [ _value_source_tag; _value_list ]
and gen__value_source_tag code (x : Frame_piqi.value_source_tag) =
  Piqirun.gen_record code
    [ (match x with
       | `no_thread_id -> Piqirun.gen_bool_field 1 true
       | `thread_id x -> gen__thread_id 2 x) ]
and gen__value_list code x = Piqirun.gen_list gen__value_info code x
and gen__value_info code x =
  let _operand_info_specific =
    Piqirun.gen_required_field 1 gen__operand_info_specific
      x.Value_info.operand_info_specific in
  let _bit_length =
    Piqirun.gen_required_field 2 gen__bit_length x.Value_info.bit_length in
  let _taint_info =
    Piqirun.gen_optional_field 3 gen__taint_info x.Value_info.taint_info in
  let _value = Piqirun.gen_required_field 4 gen__binary x.Value_info.value
  in
    Piqirun.gen_record code
      [ _operand_info_specific; _bit_length; _taint_info; _value ]
and gen__block_frame code x =
  let _address =
    Piqirun.gen_required_field 1 gen__address x.Block_frame.address in
  let _thread_id =
    Piqirun.gen_required_field 2 gen__thread_id x.Block_frame.thread_id
  in Piqirun.gen_record code [ _address; _thread_id ]
and gen__call_frame code x =
  let _address =
    Piqirun.gen_required_field 1 gen__address x.Call_frame.address in
  let _target =
    Piqirun.gen_required_field 2 gen__address x.Call_frame.target in
  let _thread_id =
    Piqirun.gen_required_field 3 gen__thread_id x.Call_frame.thread_id
  in Piqirun.gen_record code [ _address; _target; _thread_id ]
and gen__ret_frame code x =
  let _address =
    Piqirun.gen_required_field 1 gen__address x.Ret_frame.address in
  let _target =
    Piqirun.gen_required_field 2 gen__address x.Ret_frame.target in
  let _thread_id =
    Piqirun.gen_required_field 3 gen__thread_id x.Ret_frame.thread_id
  in Piqirun.gen_record code [ _address; _target; _thread_id ]
  
let gen_uint64 x = gen__uint64 (-1) x
  
let gen_int x = gen__int (-1) x
  
let gen_binary x = gen__binary (-1) x
  
let gen_string x = gen__string (-1) x
  
let gen_bool x = gen__bool (-1) x
  
let gen_int64 x = gen__int64 (-1) x
  
let gen_frame x = gen__frame (-1) x
  
let gen_thread_id x = gen__thread_id (-1) x
  
let gen_address x = gen__address (-1) x
  
let gen_bit_length x = gen__bit_length (-1) x
  
let gen_taint_id x = gen__taint_id (-1) x
  
let gen_exception_number x = gen__exception_number (-1) x
  
let gen_operand_list x = gen__operand_list (-1) x
  
let gen_operand_info x = gen__operand_info (-1) x
  
let gen_operand_info_specific x = gen__operand_info_specific (-1) x
  
let gen_reg_operand x = gen__reg_operand (-1) x
  
let gen_mem_operand x = gen__mem_operand (-1) x
  
let gen_operand_usage x = gen__operand_usage (-1) x
  
let gen_taint_info x = gen__taint_info (-1) x
  
let gen_std_frame x = gen__std_frame (-1) x
  
let gen_syscall_frame x = gen__syscall_frame (-1) x
  
let gen_argument_list x = gen__argument_list (-1) x
  
let gen_argument x = gen__argument (-1) x
  
let gen_exception_frame x = gen__exception_frame (-1) x
  
let gen_taint_intro_frame x = gen__taint_intro_frame (-1) x
  
let gen_taint_intro_list x = gen__taint_intro_list (-1) x
  
let gen_taint_intro x = gen__taint_intro (-1) x
  
let gen_modload_frame x = gen__modload_frame (-1) x
  
let gen_key_frame x = gen__key_frame (-1) x
  
let gen_tagged_value_lists x = gen__tagged_value_lists (-1) x
  
let gen_tagged_value_list x = gen__tagged_value_list (-1) x
  
let gen_value_source_tag x = gen__value_source_tag (-1) x
  
let gen_value_list x = gen__value_list (-1) x
  
let gen_value_info x = gen__value_info (-1) x
  
let gen_block_frame x = gen__block_frame (-1) x
  
let gen_call_frame x = gen__call_frame (-1) x
  
let gen_ret_frame x = gen__ret_frame (-1) x
  
let piqi =
  [ "\226\202\2304\005frame\160\148\209H\129\248\174h\218\244\134\182\012\149\002\170\136\200\184\014\142\002\218\164\238\191\004\005frame\170\183\218\222\005\020\232\146\150q\002\210\171\158\194\006\tstd-frame\170\183\218\222\005\024\232\146\150q\004\210\171\158\194\006\rsyscall-frame\170\183\218\222\005\026\232\146\150q\006\210\171\158\194\006\015exception-frame\170\183\218\222\005\028\232\146\150q\b\210\171\158\194\006\017taint-intro-frame\170\183\218\222\005\024\232\146\150q\n\210\171\158\194\006\rmodload-frame\170\183\218\222\005\020\232\146\150q\012\210\171\158\194\006\tkey-frame\170\183\218\222\005\022\232\146\150q\014\210\171\158\194\006\011block-frame\170\183\218\222\005\021\232\146\150q\016\210\171\158\194\006\ncall-frame\170\183\218\222\005\020\232\146\150q\018\210\171\158\194\006\tret-frame\218\244\134\182\012 \130\153\170d\027\218\164\238\191\004\tthread-id\210\171\158\194\006\006uint64\218\244\134\182\012\030\130\153\170d\025\218\164\238\191\004\007address\210\171\158\194\006\006uint64\218\244\134\182\012\030\130\153\170d\025\218\164\238\191\004\nbit-length\210\171\158\194\006\003int\218\244\134\182\012\031\130\153\170d\026\218\164\238\191\004\btaint-id\210\171\158\194\006\006uint64\218\244\134\182\012'\130\153\170d\"\218\164\238\191\004\016exception-number\210\171\158\194\006\006uint64\218\244\134\182\012*\242\197\227\236\003$\218\164\238\191\004\012operand-list\210\171\158\194\006\012operand-info\218\244\134\182\012\226\001\138\233\142\251\014\219\001\210\203\242$*\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\021operand-info-specific\210\203\242$\031\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\nbit-length\210\203\242$\"\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\roperand-usage\210\203\242$\031\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\ntaint-info\210\203\242$&\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005value\210\171\158\194\006\006binary\218\164\238\191\004\012operand-info\218\244\134\182\012Y\170\136\200\184\014S\218\164\238\191\004\021operand-info-specific\170\183\218\222\005\022\232\146\150q\002\210\171\158\194\006\011mem-operand\170\183\218\222\005\022\232\146\150q\004\210\171\158\194\006\011reg-operand\218\244\134\182\012A\138\233\142\251\014;\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004name\210\171\158\194\006\006string\218\164\238\191\004\011reg-operand\218\244\134\182\0128\138\233\142\251\0142\210\203\242$\028\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\007address\218\164\238\191\004\011mem-operand\218\244\134\182\012\190\001\138\233\142\251\014\183\001\210\203\242$#\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004read\210\171\158\194\006\004bool\210\203\242$&\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007written\210\171\158\194\006\004bool\210\203\242$$\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\004bool\210\203\242$#\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004base\210\171\158\194\006\004bool\218\164\238\191\004\roperand-usage\218\244\134\182\012g\170\136\200\184\014a\218\164\238\191\004\ntaint-info\170\183\218\222\005\019\232\146\150q\002\218\164\238\191\004\bno-taint\170\183\218\222\005\019\232\146\150q\004\210\171\158\194\006\btaint-id\170\183\218\222\005\025\232\146\150q\006\218\164\238\191\004\014taint-multiple\218\244\134\182\012\174\001\138\233\142\251\014\167\001\210\203\242$\028\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\007address\210\203\242$\030\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\tthread-id\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brawbytes\210\171\158\194\006\006binary\210\203\242$!\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\012operand-list\218\164\238\191\004\tstd-frame\218\244\134\182\012\177\001\138\233\142\251\014\170\001\210\203\242$\028\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\007address\210\203\242$\030\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\tthread-id\210\203\242$'\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006number\210\171\158\194\006\006uint64\210\203\242$\"\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\rargument-list\218\164\238\191\004\rsyscall-frame\218\244\134\182\012'\242\197\227\236\003!\218\164\238\191\004\rargument-list\210\171\158\194\006\bargument\218\244\134\182\012\030\130\153\170d\025\218\164\238\191\004\bargument\210\171\158\194\006\005int64\218\244\134\182\012\199\001\138\233\142\251\014\192\001\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\016exception-number\210\203\242$\030\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\tthread-id\210\203\242$+\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tfrom-addr\210\171\158\194\006\007address\210\203\242$)\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007to-addr\210\171\158\194\006\007address\218\164\238\191\004\015exception-frame\218\244\134\182\012G\138\233\142\251\014A\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\016taint-intro-list\218\164\238\191\004\017taint-intro-frame\218\244\134\182\012-\242\197\227\236\003'\218\164\238\191\004\016taint-intro-list\210\171\158\194\006\011taint-intro\218\244\134\182\012\238\001\138\233\142\251\014\231\001\210\203\242$&\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004addr\210\171\158\194\006\007address\210\203\242$\029\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\btaint-id\210\203\242$&\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005value\210\171\158\194\006\006binary\210\203\242$,\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011source-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006offset\210\171\158\194\006\007address\218\164\238\191\004\011taint-intro\218\244\134\182\012\176\001\138\233\142\251\014\169\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011module-name\210\171\158\194\006\006string\210\203\242$-\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011low-address\210\171\158\194\006\007address\210\203\242$.\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012high-address\210\171\158\194\006\007address\218\164\238\191\004\rmodload-frame\218\244\134\182\012A\138\233\142\251\014;\210\203\242$'\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\018tagged-value-lists\218\164\238\191\004\tkey-frame\218\244\134\182\0125\242\197\227\236\003/\218\164\238\191\004\018tagged-value-lists\210\171\158\194\006\017tagged-value-list\218\244\134\182\012\129\001\138\233\142\251\014{\210\203\242$;\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\016value-source-tag\210\171\158\194\006\016value-source-tag\210\203\242$\031\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\nvalue-list\218\164\238\191\004\017tagged-value-list\218\244\134\182\012S\170\136\200\184\014M\218\164\238\191\004\016value-source-tag\170\183\218\222\005\023\232\146\150q\002\218\164\238\191\004\012no-thread-id\170\183\218\222\005\020\232\146\150q\004\210\171\158\194\006\tthread-id\218\244\134\182\012&\242\197\227\236\003 \218\164\238\191\004\nvalue-list\210\171\158\194\006\nvalue-info\218\244\134\182\012\185\001\138\233\142\251\014\178\001\210\203\242$*\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\021operand-info-specific\210\203\242$\031\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\nbit-length\210\203\242$\031\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\ntaint-info\210\203\242$&\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005value\210\171\158\194\006\006binary\218\164\238\191\004\nvalue-info\218\244\134\182\012h\138\233\142\251\014b\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007address\210\171\158\194\006\007address\210\203\242$\030\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\tthread-id\218\164\238\191\004\011block-frame\218\244\134\182\012\149\001\138\233\142\251\014\142\001\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007address\210\171\158\194\006\007address\210\203\242$(\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006target\210\171\158\194\006\007address\210\203\242$\030\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\tthread-id\218\164\238\191\004\ncall-frame\218\244\134\182\012\148\001\138\233\142\251\014\141\001\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007address\210\171\158\194\006\007address\210\203\242$(\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006target\210\171\158\194\006\007address\210\203\242$\030\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\tthread-id\218\164\238\191\004\tret-frame" ]
  

