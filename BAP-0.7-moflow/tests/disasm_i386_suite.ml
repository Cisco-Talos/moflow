open OUnit


let parse_nop _ = 
  let nop _ = '\x90' in
  let (l,op,i64) = Disasm_i386.parse_instr nop Int64.zero in
  let m = "instruction: " ^ (Disasm_i386.ToStr.op2str op) ^ " is not a Nop!" in
  assert_equal ~msg:m Disasm_i386.Nop op;;

let suite = "Disasm_i386" >:::
  [
	"parse_nop" >:: parse_nop;
  ]
