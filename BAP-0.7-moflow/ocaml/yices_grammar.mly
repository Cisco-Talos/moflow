
%token <string> VAR
%token <Big_int_Z.big_int> VAL

%token SEMICOLON
%token LBRACKET RBRACKET
%token EQUAL
%token MODEL
%token ASSERT
%token SDASHES
%token DASHES
%token QUESTIONMARKS
%token INVALID
%token VALID
%token DEFAULT
%token COMMA
%token PERIOD
%token EOF

%start main
%type <(string * Big_int_Z.big_int) list option> main

%%

main:
/* The result comes before or after the assertions depending on version. */
  goodresult MODEL assertions DASHES EOF { Some($3) }
| badresult EOF { None }
  ;

assertions:
  /* empty */ { [] }
  | assertion assertions { match $1 with | None -> $2 | Some(x) -> x::$2 }
  ;

assertion:
  /* Assign a bitvector to a variable */
  LBRACKET EQUAL VAR VAL RBRACKET { Some($3, $4) }
  /* Assign a variable to a variable */
  | LBRACKET EQUAL VAR VAR RBRACKET { None }
  /* Ignore "--- var ---" */
  | SDASHES VAR SDASHES { None }
  /* Ignore "default: val" */
  | DEFAULT VAL { None }
  /* Ignore memory information for now */
  | LBRACKET EQUAL LBRACKET VAR val_opt RBRACKET val_opt RBRACKET { None }
  ;

val_opt:
  VAL { }
  | QUESTIONMARKS { }
;

goodresult:
  INVALID { }
;

badresult:
  VALID { }
;
