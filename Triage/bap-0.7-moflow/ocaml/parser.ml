(** Bap interface to the parser. *)

open Grammar_private_scope
open Grammar_scope

(* Note: Even though scope is imperative, we need to return it since the
   user might not have specified the input scope, but may be interested
   in the output scope. *)

(* Reference: http://stackoverflow.com/questions/1933101/ocamlyacc-parse-error-what-token *)
let handle_exception lexbuf exn =
  let curr = lexbuf.Lexing.lex_curr_p in
  let line = curr.Lexing.pos_lnum in
  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let tok = Lexing.lexeme lexbuf in
  let tail = Lexer.ruleTail "" lexbuf in
  Printf.fprintf stderr "A parsing exception occured while parsing '%s' on line %d (char %d).\nException occured while parsing: %s\n"
    tok line cnum (tok^tail);
  raise exn

let program_from_lexbuf ?(scope=default_scope ()) l =
  try
    set_scope scope;
    let p = Grammar.program Lexer.token l in
    Parsing.clear_parser();
    p, get_scope()
  with exn -> handle_exception l exn

let program_from_file ?(scope=default_scope ()) f =
  let ic = open_in f in
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
  let p = program_from_lexbuf lb ~scope in
  close_in ic;
  p

let exp_from_lexbuf ?(scope=default_scope ()) l =
  try
    set_scope scope;
    let e = Grammar.expr Lexer.token l in
    Parsing.clear_parser();
    e, get_scope()
  with exn -> handle_exception l exn

let exp_from_string ?(scope=default_scope ()) s =
  exp_from_lexbuf (Lexing.from_string s) ~scope

let exp_from_file ?(scope=default_scope ()) f =
  let ic = open_in f in
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
  let e = exp_from_lexbuf lb ~scope in
  close_in ic;
  e
