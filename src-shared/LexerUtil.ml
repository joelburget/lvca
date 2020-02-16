open Lexing

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p
  <- { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
;;

let position lexbuf =
  let p = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
;;

let error lexbuf msg = raise (SyntaxError (position lexbuf ^ " " ^ msg))
