let piqi = Stmt_piqi.piqi

let _ = Piqirun_ext.init_piqi piqi



let _string_piqtype = Piqirun_ext.find_piqtype "string"
let _int_piqtype = Piqirun_ext.find_piqtype "int"
let _int64_piqtype = Piqirun_ext.find_piqtype "int64"
let _uint64_piqtype = Piqirun_ext.find_piqtype "uint64"
let _bool_piqtype = Piqirun_ext.find_piqtype "bool"
let _program_piqtype = Piqirun_ext.find_piqtype "stmt/program"
let _stmt_piqtype = Piqirun_ext.find_piqtype "stmt/stmt"
let _move_piqtype = Piqirun_ext.find_piqtype "stmt/move"
let _jmp_piqtype = Piqirun_ext.find_piqtype "stmt/jmp"
let _cjmp_piqtype = Piqirun_ext.find_piqtype "stmt/cjmp"
let _label_stmt_piqtype = Piqirun_ext.find_piqtype "stmt/label-stmt"
let _halt_piqtype = Piqirun_ext.find_piqtype "stmt/halt"
let _assert_stmt_piqtype = Piqirun_ext.find_piqtype "stmt/assert-stmt"
let _assume_piqtype = Piqirun_ext.find_piqtype "stmt/assume"
let _comment_piqtype = Piqirun_ext.find_piqtype "stmt/comment"
let _special_piqtype = Piqirun_ext.find_piqtype "stmt/special"
let _typ_piqtype = Piqirun_ext.find_piqtype "stmt/typ"
let _reg_piqtype = Piqirun_ext.find_piqtype "stmt/reg"
let _tmem_piqtype = Piqirun_ext.find_piqtype "stmt/tmem"
let _array_piqtype = Piqirun_ext.find_piqtype "stmt/array"
let _label_piqtype = Piqirun_ext.find_piqtype "stmt/label"
let _cast_type_piqtype = Piqirun_ext.find_piqtype "stmt/cast-type"
let _binop_type_piqtype = Piqirun_ext.find_piqtype "stmt/binop-type"
let _unop_type_piqtype = Piqirun_ext.find_piqtype "stmt/unop-type"
let _var_piqtype = Piqirun_ext.find_piqtype "stmt/var"
let _lab_piqtype = Piqirun_ext.find_piqtype "stmt/lab"
let _attribute_piqtype = Piqirun_ext.find_piqtype "stmt/attribute"
let _attributes_piqtype = Piqirun_ext.find_piqtype "stmt/attributes"
let _asm_piqtype = Piqirun_ext.find_piqtype "stmt/asm"
let _address_piqtype = Piqirun_ext.find_piqtype "stmt/address"
let _liveout_piqtype = Piqirun_ext.find_piqtype "stmt/liveout"
let _synthetic_piqtype = Piqirun_ext.find_piqtype "stmt/synthetic"
let _strattr_piqtype = Piqirun_ext.find_piqtype "stmt/strattr"
let _thread_id_piqtype = Piqirun_ext.find_piqtype "stmt/thread-id"
let _operand_info_specific_piqtype = Piqirun_ext.find_piqtype "stmt/operand-info-specific"
let _reg_operand_piqtype = Piqirun_ext.find_piqtype "stmt/reg-operand"
let _bit_length_piqtype = Piqirun_ext.find_piqtype "stmt/bit-length"
let _taint_id_piqtype = Piqirun_ext.find_piqtype "stmt/taint-id"
let _operand_usage_piqtype = Piqirun_ext.find_piqtype "stmt/operand-usage"
let _mem_operand_piqtype = Piqirun_ext.find_piqtype "stmt/mem-operand"
let _taint_info_piqtype = Piqirun_ext.find_piqtype "stmt/taint-info"
let _context_piqtype = Piqirun_ext.find_piqtype "stmt/context"
let _exp_piqtype = Piqirun_ext.find_piqtype "stmt/exp"
let _load_piqtype = Piqirun_ext.find_piqtype "stmt/load"
let _store_piqtype = Piqirun_ext.find_piqtype "stmt/store"
let _binop_piqtype = Piqirun_ext.find_piqtype "stmt/binop"
let _unop_piqtype = Piqirun_ext.find_piqtype "stmt/unop"
let _inte_piqtype = Piqirun_ext.find_piqtype "stmt/inte"
let _cast_piqtype = Piqirun_ext.find_piqtype "stmt/cast"
let _let_exp_piqtype = Piqirun_ext.find_piqtype "stmt/let-exp"
let _unknown_piqtype = Piqirun_ext.find_piqtype "stmt/unknown"
let _ite_piqtype = Piqirun_ext.find_piqtype "stmt/ite"
let _extract_piqtype = Piqirun_ext.find_piqtype "stmt/extract"
let _concat_piqtype = Piqirun_ext.find_piqtype "stmt/concat"


let parse_string ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _string_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_string buf
let parse_int ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _int_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_int buf
let parse_int64 ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _int64_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_int64 buf
let parse_uint64 ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _uint64_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_uint64 buf
let parse_bool ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _bool_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_bool buf
let parse_program ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _program_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_program buf
let parse_stmt ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _stmt_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_stmt buf
let parse_move ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _move_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_move buf
let parse_jmp ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _jmp_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_jmp buf
let parse_cjmp ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _cjmp_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_cjmp buf
let parse_label_stmt ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _label_stmt_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_label_stmt buf
let parse_halt ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _halt_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_halt buf
let parse_assert_stmt ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _assert_stmt_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_assert_stmt buf
let parse_assume ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _assume_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_assume buf
let parse_comment ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _comment_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_comment buf
let parse_special ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _special_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_special buf
let parse_typ ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _typ_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_typ buf
let parse_reg ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _reg_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_reg buf
let parse_tmem ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _tmem_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_tmem buf
let parse_array ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _array_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_array buf
let parse_label ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _label_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_label buf
let parse_cast_type ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _cast_type_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_cast_type buf
let parse_binop_type ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _binop_type_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_binop_type buf
let parse_unop_type ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _unop_type_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_unop_type buf
let parse_var ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _var_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_var buf
let parse_lab ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _lab_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_lab buf
let parse_attribute ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _attribute_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_attribute buf
let parse_attributes ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _attributes_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_attributes buf
let parse_asm ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _asm_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_asm buf
let parse_address ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _address_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_address buf
let parse_liveout ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _liveout_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_liveout buf
let parse_synthetic ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _synthetic_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_synthetic buf
let parse_strattr ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _strattr_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_strattr buf
let parse_thread_id ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _thread_id_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_thread_id buf
let parse_operand_info_specific ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _operand_info_specific_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_operand_info_specific buf
let parse_reg_operand ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _reg_operand_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_reg_operand buf
let parse_bit_length ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _bit_length_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_bit_length buf
let parse_taint_id ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_id_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_taint_id buf
let parse_operand_usage ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _operand_usage_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_operand_usage buf
let parse_mem_operand ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _mem_operand_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_mem_operand buf
let parse_taint_info ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_info_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_taint_info buf
let parse_context ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _context_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_context buf
let parse_exp ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _exp_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_exp buf
let parse_load ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _load_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_load buf
let parse_store ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _store_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_store buf
let parse_binop ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _binop_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_binop buf
let parse_unop ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _unop_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_unop buf
let parse_inte ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _inte_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_inte buf
let parse_cast ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _cast_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_cast buf
let parse_let_exp ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _let_exp_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_let_exp buf
let parse_unknown ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _unknown_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_unknown buf
let parse_ite ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _ite_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_ite buf
let parse_extract ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _extract_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_extract buf
let parse_concat ?opts x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _concat_piqtype format `pb x ?opts in let buf = Piqirun.init_from_string x_pb in Stmt_piqi.parse_concat buf


let gen_string ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_string x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _string_piqtype `pb format x_pb ?opts
let gen_int ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_int x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _int_piqtype `pb format x_pb ?opts
let gen_int64 ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_int64 x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _int64_piqtype `pb format x_pb ?opts
let gen_uint64 ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_uint64 x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _uint64_piqtype `pb format x_pb ?opts
let gen_bool ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_bool x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _bool_piqtype `pb format x_pb ?opts
let gen_program ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_program x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _program_piqtype `pb format x_pb ?opts
let gen_stmt ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_stmt x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _stmt_piqtype `pb format x_pb ?opts
let gen_move ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_move x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _move_piqtype `pb format x_pb ?opts
let gen_jmp ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_jmp x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _jmp_piqtype `pb format x_pb ?opts
let gen_cjmp ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_cjmp x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _cjmp_piqtype `pb format x_pb ?opts
let gen_label_stmt ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_label_stmt x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _label_stmt_piqtype `pb format x_pb ?opts
let gen_halt ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_halt x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _halt_piqtype `pb format x_pb ?opts
let gen_assert_stmt ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_assert_stmt x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _assert_stmt_piqtype `pb format x_pb ?opts
let gen_assume ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_assume x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _assume_piqtype `pb format x_pb ?opts
let gen_comment ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_comment x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _comment_piqtype `pb format x_pb ?opts
let gen_special ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_special x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _special_piqtype `pb format x_pb ?opts
let gen_typ ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_typ x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _typ_piqtype `pb format x_pb ?opts
let gen_reg ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_reg x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _reg_piqtype `pb format x_pb ?opts
let gen_tmem ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_tmem x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _tmem_piqtype `pb format x_pb ?opts
let gen_array ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_array x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _array_piqtype `pb format x_pb ?opts
let gen_label ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_label x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _label_piqtype `pb format x_pb ?opts
let gen_cast_type ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_cast_type x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _cast_type_piqtype `pb format x_pb ?opts
let gen_binop_type ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_binop_type x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _binop_type_piqtype `pb format x_pb ?opts
let gen_unop_type ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_unop_type x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _unop_type_piqtype `pb format x_pb ?opts
let gen_var ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_var x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _var_piqtype `pb format x_pb ?opts
let gen_lab ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_lab x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _lab_piqtype `pb format x_pb ?opts
let gen_attribute ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_attribute x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _attribute_piqtype `pb format x_pb ?opts
let gen_attributes ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_attributes x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _attributes_piqtype `pb format x_pb ?opts
let gen_asm ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_asm x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _asm_piqtype `pb format x_pb ?opts
let gen_address ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_address x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _address_piqtype `pb format x_pb ?opts
let gen_liveout ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_liveout x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _liveout_piqtype `pb format x_pb ?opts
let gen_synthetic ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_synthetic x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _synthetic_piqtype `pb format x_pb ?opts
let gen_strattr ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_strattr x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _strattr_piqtype `pb format x_pb ?opts
let gen_thread_id ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_thread_id x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _thread_id_piqtype `pb format x_pb ?opts
let gen_operand_info_specific ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_operand_info_specific x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _operand_info_specific_piqtype `pb format x_pb ?opts
let gen_reg_operand ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_reg_operand x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _reg_operand_piqtype `pb format x_pb ?opts
let gen_bit_length ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_bit_length x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _bit_length_piqtype `pb format x_pb ?opts
let gen_taint_id ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_taint_id x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_id_piqtype `pb format x_pb ?opts
let gen_operand_usage ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_operand_usage x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _operand_usage_piqtype `pb format x_pb ?opts
let gen_mem_operand ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_mem_operand x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _mem_operand_piqtype `pb format x_pb ?opts
let gen_taint_info ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_taint_info x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_info_piqtype `pb format x_pb ?opts
let gen_context ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_context x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _context_piqtype `pb format x_pb ?opts
let gen_exp ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_exp x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _exp_piqtype `pb format x_pb ?opts
let gen_load ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_load x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _load_piqtype `pb format x_pb ?opts
let gen_store ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_store x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _store_piqtype `pb format x_pb ?opts
let gen_binop ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_binop x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _binop_piqtype `pb format x_pb ?opts
let gen_unop ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_unop x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _unop_piqtype `pb format x_pb ?opts
let gen_inte ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_inte x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _inte_piqtype `pb format x_pb ?opts
let gen_cast ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_cast x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _cast_piqtype `pb format x_pb ?opts
let gen_let_exp ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_let_exp x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _let_exp_piqtype `pb format x_pb ?opts
let gen_unknown ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_unknown x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _unknown_piqtype `pb format x_pb ?opts
let gen_ite ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_ite x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _ite_piqtype `pb format x_pb ?opts
let gen_extract ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_extract x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _extract_piqtype `pb format x_pb ?opts
let gen_concat ?opts x (format :Piqirun_ext.output_format) = let buf =  Stmt_piqi.gen_concat x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _concat_piqtype `pb format x_pb ?opts


let print_string x = Pervasives.print_endline (gen_string x `piq)
 let prerr_string x = Pervasives.prerr_endline (gen_string x `piq)
let print_int x = Pervasives.print_endline (gen_int x `piq)
 let prerr_int x = Pervasives.prerr_endline (gen_int x `piq)
let print_int64 x = Pervasives.print_endline (gen_int64 x `piq)
 let prerr_int64 x = Pervasives.prerr_endline (gen_int64 x `piq)
let print_uint64 x = Pervasives.print_endline (gen_uint64 x `piq)
 let prerr_uint64 x = Pervasives.prerr_endline (gen_uint64 x `piq)
let print_bool x = Pervasives.print_endline (gen_bool x `piq)
 let prerr_bool x = Pervasives.prerr_endline (gen_bool x `piq)
let print_program x = Pervasives.print_endline (gen_program x `piq)
 let prerr_program x = Pervasives.prerr_endline (gen_program x `piq)
let print_stmt x = Pervasives.print_endline (gen_stmt x `piq)
 let prerr_stmt x = Pervasives.prerr_endline (gen_stmt x `piq)
let print_move x = Pervasives.print_endline (gen_move x `piq)
 let prerr_move x = Pervasives.prerr_endline (gen_move x `piq)
let print_jmp x = Pervasives.print_endline (gen_jmp x `piq)
 let prerr_jmp x = Pervasives.prerr_endline (gen_jmp x `piq)
let print_cjmp x = Pervasives.print_endline (gen_cjmp x `piq)
 let prerr_cjmp x = Pervasives.prerr_endline (gen_cjmp x `piq)
let print_label_stmt x = Pervasives.print_endline (gen_label_stmt x `piq)
 let prerr_label_stmt x = Pervasives.prerr_endline (gen_label_stmt x `piq)
let print_halt x = Pervasives.print_endline (gen_halt x `piq)
 let prerr_halt x = Pervasives.prerr_endline (gen_halt x `piq)
let print_assert_stmt x = Pervasives.print_endline (gen_assert_stmt x `piq)
 let prerr_assert_stmt x = Pervasives.prerr_endline (gen_assert_stmt x `piq)
let print_assume x = Pervasives.print_endline (gen_assume x `piq)
 let prerr_assume x = Pervasives.prerr_endline (gen_assume x `piq)
let print_comment x = Pervasives.print_endline (gen_comment x `piq)
 let prerr_comment x = Pervasives.prerr_endline (gen_comment x `piq)
let print_special x = Pervasives.print_endline (gen_special x `piq)
 let prerr_special x = Pervasives.prerr_endline (gen_special x `piq)
let print_typ x = Pervasives.print_endline (gen_typ x `piq)
 let prerr_typ x = Pervasives.prerr_endline (gen_typ x `piq)
let print_reg x = Pervasives.print_endline (gen_reg x `piq)
 let prerr_reg x = Pervasives.prerr_endline (gen_reg x `piq)
let print_tmem x = Pervasives.print_endline (gen_tmem x `piq)
 let prerr_tmem x = Pervasives.prerr_endline (gen_tmem x `piq)
let print_array x = Pervasives.print_endline (gen_array x `piq)
 let prerr_array x = Pervasives.prerr_endline (gen_array x `piq)
let print_label x = Pervasives.print_endline (gen_label x `piq)
 let prerr_label x = Pervasives.prerr_endline (gen_label x `piq)
let print_cast_type x = Pervasives.print_endline (gen_cast_type x `piq)
 let prerr_cast_type x = Pervasives.prerr_endline (gen_cast_type x `piq)
let print_binop_type x = Pervasives.print_endline (gen_binop_type x `piq)
 let prerr_binop_type x = Pervasives.prerr_endline (gen_binop_type x `piq)
let print_unop_type x = Pervasives.print_endline (gen_unop_type x `piq)
 let prerr_unop_type x = Pervasives.prerr_endline (gen_unop_type x `piq)
let print_var x = Pervasives.print_endline (gen_var x `piq)
 let prerr_var x = Pervasives.prerr_endline (gen_var x `piq)
let print_lab x = Pervasives.print_endline (gen_lab x `piq)
 let prerr_lab x = Pervasives.prerr_endline (gen_lab x `piq)
let print_attribute x = Pervasives.print_endline (gen_attribute x `piq)
 let prerr_attribute x = Pervasives.prerr_endline (gen_attribute x `piq)
let print_attributes x = Pervasives.print_endline (gen_attributes x `piq)
 let prerr_attributes x = Pervasives.prerr_endline (gen_attributes x `piq)
let print_asm x = Pervasives.print_endline (gen_asm x `piq)
 let prerr_asm x = Pervasives.prerr_endline (gen_asm x `piq)
let print_address x = Pervasives.print_endline (gen_address x `piq)
 let prerr_address x = Pervasives.prerr_endline (gen_address x `piq)
let print_liveout x = Pervasives.print_endline (gen_liveout x `piq)
 let prerr_liveout x = Pervasives.prerr_endline (gen_liveout x `piq)
let print_synthetic x = Pervasives.print_endline (gen_synthetic x `piq)
 let prerr_synthetic x = Pervasives.prerr_endline (gen_synthetic x `piq)
let print_strattr x = Pervasives.print_endline (gen_strattr x `piq)
 let prerr_strattr x = Pervasives.prerr_endline (gen_strattr x `piq)
let print_thread_id x = Pervasives.print_endline (gen_thread_id x `piq)
 let prerr_thread_id x = Pervasives.prerr_endline (gen_thread_id x `piq)
let print_operand_info_specific x = Pervasives.print_endline (gen_operand_info_specific x `piq)
 let prerr_operand_info_specific x = Pervasives.prerr_endline (gen_operand_info_specific x `piq)
let print_reg_operand x = Pervasives.print_endline (gen_reg_operand x `piq)
 let prerr_reg_operand x = Pervasives.prerr_endline (gen_reg_operand x `piq)
let print_bit_length x = Pervasives.print_endline (gen_bit_length x `piq)
 let prerr_bit_length x = Pervasives.prerr_endline (gen_bit_length x `piq)
let print_taint_id x = Pervasives.print_endline (gen_taint_id x `piq)
 let prerr_taint_id x = Pervasives.prerr_endline (gen_taint_id x `piq)
let print_operand_usage x = Pervasives.print_endline (gen_operand_usage x `piq)
 let prerr_operand_usage x = Pervasives.prerr_endline (gen_operand_usage x `piq)
let print_mem_operand x = Pervasives.print_endline (gen_mem_operand x `piq)
 let prerr_mem_operand x = Pervasives.prerr_endline (gen_mem_operand x `piq)
let print_taint_info x = Pervasives.print_endline (gen_taint_info x `piq)
 let prerr_taint_info x = Pervasives.prerr_endline (gen_taint_info x `piq)
let print_context x = Pervasives.print_endline (gen_context x `piq)
 let prerr_context x = Pervasives.prerr_endline (gen_context x `piq)
let print_exp x = Pervasives.print_endline (gen_exp x `piq)
 let prerr_exp x = Pervasives.prerr_endline (gen_exp x `piq)
let print_load x = Pervasives.print_endline (gen_load x `piq)
 let prerr_load x = Pervasives.prerr_endline (gen_load x `piq)
let print_store x = Pervasives.print_endline (gen_store x `piq)
 let prerr_store x = Pervasives.prerr_endline (gen_store x `piq)
let print_binop x = Pervasives.print_endline (gen_binop x `piq)
 let prerr_binop x = Pervasives.prerr_endline (gen_binop x `piq)
let print_unop x = Pervasives.print_endline (gen_unop x `piq)
 let prerr_unop x = Pervasives.prerr_endline (gen_unop x `piq)
let print_inte x = Pervasives.print_endline (gen_inte x `piq)
 let prerr_inte x = Pervasives.prerr_endline (gen_inte x `piq)
let print_cast x = Pervasives.print_endline (gen_cast x `piq)
 let prerr_cast x = Pervasives.prerr_endline (gen_cast x `piq)
let print_let_exp x = Pervasives.print_endline (gen_let_exp x `piq)
 let prerr_let_exp x = Pervasives.prerr_endline (gen_let_exp x `piq)
let print_unknown x = Pervasives.print_endline (gen_unknown x `piq)
 let prerr_unknown x = Pervasives.prerr_endline (gen_unknown x `piq)
let print_ite x = Pervasives.print_endline (gen_ite x `piq)
 let prerr_ite x = Pervasives.prerr_endline (gen_ite x `piq)
let print_extract x = Pervasives.print_endline (gen_extract x `piq)
 let prerr_extract x = Pervasives.prerr_endline (gen_extract x `piq)
let print_concat x = Pervasives.print_endline (gen_concat x `piq)
 let prerr_concat x = Pervasives.prerr_endline (gen_concat x `piq)


