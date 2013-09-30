%token <string> VAR
%token <Big_int_Z.big_int> VAL

%token SEMICOLON
%token LBRACKET RBRACKET
%token EQUAL
%token ASSERT
%token INVALID
%token VALID
%token COMMA
%token PERIOD
%token EOF

%start main
%type <(string * Big_int_Z.big_int) list option> main

%%

main:
  /* The result comes before or after the assertions depending on version. */
  assertions goodresult EOF { Some($1) }
| goodresult assertions EOF { Some($2) }
| badresult EOF { None }
  ;

assertions:
    /* empty */ { [] }
  | assertion SEMICOLON assertions { $1 :: $3 }
  ;

assertion:
  ASSERT LBRACKET VAR EQUAL VAL RBRACKET { ($3, $5) }
  ;

goodresult:
  INVALID PERIOD { }
;

badresult:
  VALID PERIOD { }
;
