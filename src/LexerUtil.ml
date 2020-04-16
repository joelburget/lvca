open Caml.Lexing

type lex_error =
  { position : Position.t
  ; message : string
  }

let error_to_string : lex_error -> string
  = fun { position; message }
  -> Printf.sprintf "position %s: %s" (Position.to_string position) message

exception LexicalError of lex_error

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p
    <- { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
;;

let error lexbuf message = raise (LexicalError
  { position = Position.of_lexbuf lexbuf; message })
