let regexp number = [ '0'-'9' ]+

let rec token enc = lexer
  | "<utf8>" -> enc := Ulexing.Utf8; token enc lexbuf
  | "<latin1>" -> enc := Ulexing.Latin1; token enc lexbuf
  | xml_letter+ -> Printf.sprintf "word(%s)" (Ulexing.utf8_lexeme lexbuf)
  | number -> "number"
  | eof -> exit 0
  | [1234-1246] -> "bla"
  | "(" -> 
      Ulexing.rollback lexbuf; (* Puts the lexeme back into the buffer *)
      (lexer "(" [^ '(']* ")" -> Ulexing.utf8_lexeme lexbuf) lexbuf
      (* Note the use of an inline lexer *)
  | _ -> "???"


let () =
  let enc = ref Ulexing.Ascii in
  let lexbuf = Ulexing.from_var_enc_string enc "abc<latin1>é<utf8>Ã©(abc)(def)ghi" in
  try
    while true do
      let r = token enc lexbuf in
      Printf.printf "%s\n" r
    done
  with 
    | Ulexing.Error -> 
	Printf.eprintf 
	"Lexing error at offset %i\n" (Ulexing.lexeme_end lexbuf)
    | Ulexing.InvalidCodepoint i -> 
	Printf.eprintf 
	"Invalid code point %i at offset %i\n" i (Ulexing.lexeme_end lexbuf)
