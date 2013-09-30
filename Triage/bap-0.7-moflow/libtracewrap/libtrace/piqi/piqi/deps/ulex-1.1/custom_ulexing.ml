(* This example shows how to use ulex with a custom implementation
   for lex buffers. *)

module Ulexing =
struct
  exception Error

  type t = {
    buf : string;
    mutable pos : int;
    mutable mark_pos : int;
    mutable mark_val : int;
    mutable start : int;
  }

  let from_immutable_string s =
    { buf = s; pos = 0; mark_pos = 0; mark_val = 0; start = 0 }

  let start b =
    b.mark_pos <- b.pos;
    b.mark_val <- (-1);
    b.start <- b.pos

  let mark b i =
    b.mark_pos <- b.pos;
    b.mark_val <- i

  let backtrack b =
    b.pos <- b.mark_pos;
    b.mark_val

  let next b =
    if b.pos < String.length b.buf 
    then let c = Char.code b.buf.[b.pos] in b.pos <- b.pos + 1; c
    else (-1)

  let lexeme b =
    String.sub b.buf b.start (b.pos - b.start)
end


let () =
  let rec split = lexer
    | ['a'-'z' 'A'-'Z']* -> print_endline (Ulexing.lexeme lexbuf); split lexbuf
    | eof -> ()
    | _ -> split lexbuf
  in
  split (Ulexing.from_immutable_string "Hello, world !")
