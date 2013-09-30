{
  open Yices_grammar
  (* TODO: add support for memories *)

  exception LexError of string
}

let digit = ['0'-'9''A'-'F''a'-'f']
let varname = ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_''['']']*

rule token = parse
  | [' ' '\t' '\n']  { token lexbuf }
  | '='              { EQUAL }
  | ','              { COMMA }
  | '.'              { PERIOD }
  | ';'              { SEMICOLON }
  | '('              { LBRACKET }
  | ')'              { RBRACKET }
  | "---"            { SDASHES }
  | "----"           { DASHES }
  | "???"            { QUESTIONMARKS }
  | "MODEL"          { MODEL }
  | "ASSERT"         { ASSERT }
  | "sat"            { INVALID }
  | "unsat"          { VALID }
  | "default:"       { DEFAULT }
  | "0hex"           { read_num lexbuf }
  | "0x"             { read_num lexbuf }
  | "0b"             { read_num lexbuf }
  | varname as var   { VAR var }
  | eof              { EOF }
  | _                { token lexbuf }

and read_num = parse
  | digit+ as n      {
    try VAL(Util.big_int_of_string ("0b"^n))
    with Failure "int_of_string" ->
      raise(LexError "Error converting integer");
    }
  | _                { token lexbuf }

