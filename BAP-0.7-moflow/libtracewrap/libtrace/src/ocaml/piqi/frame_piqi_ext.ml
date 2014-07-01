let piqi = Frame_piqi.piqi
  
let _ = Piqirun_ext.init_piqi piqi
  
let _uint64_piqtype = Piqirun_ext.find_piqtype "uint64"
  
let _int_piqtype = Piqirun_ext.find_piqtype "int"
  
let _binary_piqtype = Piqirun_ext.find_piqtype "binary"
  
let _string_piqtype = Piqirun_ext.find_piqtype "string"
  
let _bool_piqtype = Piqirun_ext.find_piqtype "bool"
  
let _int64_piqtype = Piqirun_ext.find_piqtype "int64"
  
let _frame_piqtype = Piqirun_ext.find_piqtype "frame/frame"
  
let _thread_id_piqtype = Piqirun_ext.find_piqtype "frame/thread-id"
  
let _address_piqtype = Piqirun_ext.find_piqtype "frame/address"
  
let _bit_length_piqtype = Piqirun_ext.find_piqtype "frame/bit-length"
  
let _taint_id_piqtype = Piqirun_ext.find_piqtype "frame/taint-id"
  
let _exception_number_piqtype =
  Piqirun_ext.find_piqtype "frame/exception-number"
  
let _operand_list_piqtype = Piqirun_ext.find_piqtype "frame/operand-list"
  
let _operand_info_piqtype = Piqirun_ext.find_piqtype "frame/operand-info"
  
let _operand_info_specific_piqtype =
  Piqirun_ext.find_piqtype "frame/operand-info-specific"
  
let _reg_operand_piqtype = Piqirun_ext.find_piqtype "frame/reg-operand"
  
let _mem_operand_piqtype = Piqirun_ext.find_piqtype "frame/mem-operand"
  
let _operand_usage_piqtype = Piqirun_ext.find_piqtype "frame/operand-usage"
  
let _taint_info_piqtype = Piqirun_ext.find_piqtype "frame/taint-info"
  
let _std_frame_piqtype = Piqirun_ext.find_piqtype "frame/std-frame"
  
let _syscall_frame_piqtype = Piqirun_ext.find_piqtype "frame/syscall-frame"
  
let _argument_list_piqtype = Piqirun_ext.find_piqtype "frame/argument-list"
  
let _argument_piqtype = Piqirun_ext.find_piqtype "frame/argument"
  
let _exception_frame_piqtype =
  Piqirun_ext.find_piqtype "frame/exception-frame"
  
let _taint_intro_frame_piqtype =
  Piqirun_ext.find_piqtype "frame/taint-intro-frame"
  
let _taint_intro_list_piqtype =
  Piqirun_ext.find_piqtype "frame/taint-intro-list"
  
let _taint_intro_piqtype = Piqirun_ext.find_piqtype "frame/taint-intro"
  
let _modload_frame_piqtype = Piqirun_ext.find_piqtype "frame/modload-frame"
  
let _key_frame_piqtype = Piqirun_ext.find_piqtype "frame/key-frame"
  
let _tagged_value_lists_piqtype =
  Piqirun_ext.find_piqtype "frame/tagged-value-lists"
  
let _tagged_value_list_piqtype =
  Piqirun_ext.find_piqtype "frame/tagged-value-list"
  
let _value_source_tag_piqtype =
  Piqirun_ext.find_piqtype "frame/value-source-tag"
  
let _value_list_piqtype = Piqirun_ext.find_piqtype "frame/value-list"
  
let _value_info_piqtype = Piqirun_ext.find_piqtype "frame/value-info"
  
let _block_frame_piqtype = Piqirun_ext.find_piqtype "frame/block-frame"
  
let _call_frame_piqtype = Piqirun_ext.find_piqtype "frame/call-frame"
  
let _ret_frame_piqtype = Piqirun_ext.find_piqtype "frame/ret-frame"
  
let parse_uint64 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _uint64_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_uint64 buf
  
let parse_int ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_int buf
  
let parse_binary ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _binary_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_binary buf
  
let parse_string ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _string_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_string buf
  
let parse_bool ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _bool_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_bool buf
  
let parse_int64 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int64_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_int64 buf
  
let parse_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_frame buf
  
let parse_thread_id ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _thread_id_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_thread_id buf
  
let parse_address ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _address_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_address buf
  
let parse_bit_length ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _bit_length_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_bit_length buf
  
let parse_taint_id ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _taint_id_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_taint_id buf
  
let parse_exception_number ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _exception_number_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_exception_number buf
  
let parse_operand_list ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _operand_list_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_operand_list buf
  
let parse_operand_info ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _operand_info_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_operand_info buf
  
let parse_operand_info_specific ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _operand_info_specific_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_operand_info_specific buf
  
let parse_reg_operand ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _reg_operand_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_reg_operand buf
  
let parse_mem_operand ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _mem_operand_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_mem_operand buf
  
let parse_operand_usage ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _operand_usage_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_operand_usage buf
  
let parse_taint_info ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _taint_info_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_taint_info buf
  
let parse_std_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _std_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_std_frame buf
  
let parse_syscall_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _syscall_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_syscall_frame buf
  
let parse_argument_list ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _argument_list_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_argument_list buf
  
let parse_argument ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _argument_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_argument buf
  
let parse_exception_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _exception_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_exception_frame buf
  
let parse_taint_intro_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _taint_intro_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_taint_intro_frame buf
  
let parse_taint_intro_list ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _taint_intro_list_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_taint_intro_list buf
  
let parse_taint_intro ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _taint_intro_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_taint_intro buf
  
let parse_modload_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _modload_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_modload_frame buf
  
let parse_key_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _key_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_key_frame buf
  
let parse_tagged_value_lists ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _tagged_value_lists_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_tagged_value_lists buf
  
let parse_tagged_value_list ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _tagged_value_list_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_tagged_value_list buf
  
let parse_value_source_tag ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _value_source_tag_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Frame_piqi.parse_value_source_tag buf
  
let parse_value_list ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _value_list_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_value_list buf
  
let parse_value_info ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _value_info_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_value_info buf
  
let parse_block_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _block_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_block_frame buf
  
let parse_call_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _call_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_call_frame buf
  
let parse_ret_frame ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _ret_frame_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Frame_piqi.parse_ret_frame buf
  
let gen_uint64 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_uint64 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _uint64_piqtype `pb format x_pb ?opts
  
let gen_int ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_int x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int_piqtype `pb format x_pb ?opts
  
let gen_binary ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_binary x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _binary_piqtype `pb format x_pb ?opts
  
let gen_string ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_string x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _string_piqtype `pb format x_pb ?opts
  
let gen_bool ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_bool x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _bool_piqtype `pb format x_pb ?opts
  
let gen_int64 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_int64 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int64_piqtype `pb format x_pb ?opts
  
let gen_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _frame_piqtype `pb format x_pb ?opts
  
let gen_thread_id ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_thread_id x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _thread_id_piqtype `pb format x_pb ?opts
  
let gen_address ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_address x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _address_piqtype `pb format x_pb ?opts
  
let gen_bit_length ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_bit_length x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _bit_length_piqtype `pb format x_pb ?opts
  
let gen_taint_id ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_taint_id x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _taint_id_piqtype `pb format x_pb ?opts
  
let gen_exception_number ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_exception_number x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _exception_number_piqtype `pb format x_pb ?opts
  
let gen_operand_list ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_operand_list x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _operand_list_piqtype `pb format x_pb ?opts
  
let gen_operand_info ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_operand_info x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _operand_info_piqtype `pb format x_pb ?opts
  
let gen_operand_info_specific ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_operand_info_specific x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _operand_info_specific_piqtype `pb format x_pb ?opts
  
let gen_reg_operand ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_reg_operand x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _reg_operand_piqtype `pb format x_pb ?opts
  
let gen_mem_operand ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_mem_operand x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _mem_operand_piqtype `pb format x_pb ?opts
  
let gen_operand_usage ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_operand_usage x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _operand_usage_piqtype `pb format x_pb ?opts
  
let gen_taint_info ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_taint_info x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _taint_info_piqtype `pb format x_pb ?opts
  
let gen_std_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_std_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _std_frame_piqtype `pb format x_pb ?opts
  
let gen_syscall_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_syscall_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _syscall_frame_piqtype `pb format x_pb ?opts
  
let gen_argument_list ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_argument_list x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _argument_list_piqtype `pb format x_pb ?opts
  
let gen_argument ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_argument x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _argument_piqtype `pb format x_pb ?opts
  
let gen_exception_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_exception_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _exception_frame_piqtype `pb format x_pb ?opts
  
let gen_taint_intro_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_taint_intro_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _taint_intro_frame_piqtype `pb format x_pb ?opts
  
let gen_taint_intro_list ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_taint_intro_list x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _taint_intro_list_piqtype `pb format x_pb ?opts
  
let gen_taint_intro ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_taint_intro x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _taint_intro_piqtype `pb format x_pb ?opts
  
let gen_modload_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_modload_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _modload_frame_piqtype `pb format x_pb ?opts
  
let gen_key_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_key_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _key_frame_piqtype `pb format x_pb ?opts
  
let gen_tagged_value_lists ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_tagged_value_lists x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _tagged_value_lists_piqtype `pb format x_pb ?opts
  
let gen_tagged_value_list ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_tagged_value_list x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _tagged_value_list_piqtype `pb format x_pb ?opts
  
let gen_value_source_tag ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_value_source_tag x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _value_source_tag_piqtype `pb format x_pb ?opts
  
let gen_value_list ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_value_list x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _value_list_piqtype `pb format x_pb ?opts
  
let gen_value_info ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_value_info x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _value_info_piqtype `pb format x_pb ?opts
  
let gen_block_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_block_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _block_frame_piqtype `pb format x_pb ?opts
  
let gen_call_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_call_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _call_frame_piqtype `pb format x_pb ?opts
  
let gen_ret_frame ?opts x (format : Piqirun_ext.output_format) =
  let buf = Frame_piqi.gen_ret_frame x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _ret_frame_piqtype `pb format x_pb ?opts
  
let print_uint64 x = Pervasives.print_endline (gen_uint64 x `piq)
  
let prerr_uint64 x = Pervasives.prerr_endline (gen_uint64 x `piq)
  
let print_int x = Pervasives.print_endline (gen_int x `piq)
  
let prerr_int x = Pervasives.prerr_endline (gen_int x `piq)
  
let print_binary x = Pervasives.print_endline (gen_binary x `piq)
  
let prerr_binary x = Pervasives.prerr_endline (gen_binary x `piq)
  
let print_string x = Pervasives.print_endline (gen_string x `piq)
  
let prerr_string x = Pervasives.prerr_endline (gen_string x `piq)
  
let print_bool x = Pervasives.print_endline (gen_bool x `piq)
  
let prerr_bool x = Pervasives.prerr_endline (gen_bool x `piq)
  
let print_int64 x = Pervasives.print_endline (gen_int64 x `piq)
  
let prerr_int64 x = Pervasives.prerr_endline (gen_int64 x `piq)
  
let print_frame x = Pervasives.print_endline (gen_frame x `piq)
  
let prerr_frame x = Pervasives.prerr_endline (gen_frame x `piq)
  
let print_thread_id x = Pervasives.print_endline (gen_thread_id x `piq)
  
let prerr_thread_id x = Pervasives.prerr_endline (gen_thread_id x `piq)
  
let print_address x = Pervasives.print_endline (gen_address x `piq)
  
let prerr_address x = Pervasives.prerr_endline (gen_address x `piq)
  
let print_bit_length x = Pervasives.print_endline (gen_bit_length x `piq)
  
let prerr_bit_length x = Pervasives.prerr_endline (gen_bit_length x `piq)
  
let print_taint_id x = Pervasives.print_endline (gen_taint_id x `piq)
  
let prerr_taint_id x = Pervasives.prerr_endline (gen_taint_id x `piq)
  
let print_exception_number x =
  Pervasives.print_endline (gen_exception_number x `piq)
  
let prerr_exception_number x =
  Pervasives.prerr_endline (gen_exception_number x `piq)
  
let print_operand_list x = Pervasives.print_endline (gen_operand_list x `piq)
  
let prerr_operand_list x = Pervasives.prerr_endline (gen_operand_list x `piq)
  
let print_operand_info x = Pervasives.print_endline (gen_operand_info x `piq)
  
let prerr_operand_info x = Pervasives.prerr_endline (gen_operand_info x `piq)
  
let print_operand_info_specific x =
  Pervasives.print_endline (gen_operand_info_specific x `piq)
  
let prerr_operand_info_specific x =
  Pervasives.prerr_endline (gen_operand_info_specific x `piq)
  
let print_reg_operand x = Pervasives.print_endline (gen_reg_operand x `piq)
  
let prerr_reg_operand x = Pervasives.prerr_endline (gen_reg_operand x `piq)
  
let print_mem_operand x = Pervasives.print_endline (gen_mem_operand x `piq)
  
let prerr_mem_operand x = Pervasives.prerr_endline (gen_mem_operand x `piq)
  
let print_operand_usage x =
  Pervasives.print_endline (gen_operand_usage x `piq)
  
let prerr_operand_usage x =
  Pervasives.prerr_endline (gen_operand_usage x `piq)
  
let print_taint_info x = Pervasives.print_endline (gen_taint_info x `piq)
  
let prerr_taint_info x = Pervasives.prerr_endline (gen_taint_info x `piq)
  
let print_std_frame x = Pervasives.print_endline (gen_std_frame x `piq)
  
let prerr_std_frame x = Pervasives.prerr_endline (gen_std_frame x `piq)
  
let print_syscall_frame x =
  Pervasives.print_endline (gen_syscall_frame x `piq)
  
let prerr_syscall_frame x =
  Pervasives.prerr_endline (gen_syscall_frame x `piq)
  
let print_argument_list x =
  Pervasives.print_endline (gen_argument_list x `piq)
  
let prerr_argument_list x =
  Pervasives.prerr_endline (gen_argument_list x `piq)
  
let print_argument x = Pervasives.print_endline (gen_argument x `piq)
  
let prerr_argument x = Pervasives.prerr_endline (gen_argument x `piq)
  
let print_exception_frame x =
  Pervasives.print_endline (gen_exception_frame x `piq)
  
let prerr_exception_frame x =
  Pervasives.prerr_endline (gen_exception_frame x `piq)
  
let print_taint_intro_frame x =
  Pervasives.print_endline (gen_taint_intro_frame x `piq)
  
let prerr_taint_intro_frame x =
  Pervasives.prerr_endline (gen_taint_intro_frame x `piq)
  
let print_taint_intro_list x =
  Pervasives.print_endline (gen_taint_intro_list x `piq)
  
let prerr_taint_intro_list x =
  Pervasives.prerr_endline (gen_taint_intro_list x `piq)
  
let print_taint_intro x = Pervasives.print_endline (gen_taint_intro x `piq)
  
let prerr_taint_intro x = Pervasives.prerr_endline (gen_taint_intro x `piq)
  
let print_modload_frame x =
  Pervasives.print_endline (gen_modload_frame x `piq)
  
let prerr_modload_frame x =
  Pervasives.prerr_endline (gen_modload_frame x `piq)
  
let print_key_frame x = Pervasives.print_endline (gen_key_frame x `piq)
  
let prerr_key_frame x = Pervasives.prerr_endline (gen_key_frame x `piq)
  
let print_tagged_value_lists x =
  Pervasives.print_endline (gen_tagged_value_lists x `piq)
  
let prerr_tagged_value_lists x =
  Pervasives.prerr_endline (gen_tagged_value_lists x `piq)
  
let print_tagged_value_list x =
  Pervasives.print_endline (gen_tagged_value_list x `piq)
  
let prerr_tagged_value_list x =
  Pervasives.prerr_endline (gen_tagged_value_list x `piq)
  
let print_value_source_tag x =
  Pervasives.print_endline (gen_value_source_tag x `piq)
  
let prerr_value_source_tag x =
  Pervasives.prerr_endline (gen_value_source_tag x `piq)
  
let print_value_list x = Pervasives.print_endline (gen_value_list x `piq)
  
let prerr_value_list x = Pervasives.prerr_endline (gen_value_list x `piq)
  
let print_value_info x = Pervasives.print_endline (gen_value_info x `piq)
  
let prerr_value_info x = Pervasives.prerr_endline (gen_value_info x `piq)
  
let print_block_frame x = Pervasives.print_endline (gen_block_frame x `piq)
  
let prerr_block_frame x = Pervasives.prerr_endline (gen_block_frame x `piq)
  
let print_call_frame x = Pervasives.print_endline (gen_call_frame x `piq)
  
let prerr_call_frame x = Pervasives.prerr_endline (gen_call_frame x `piq)
  
let print_ret_frame x = Pervasives.print_endline (gen_ret_frame x `piq)
  
let prerr_ret_frame x = Pervasives.prerr_endline (gen_ret_frame x `piq)
  

