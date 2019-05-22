
(* The type of tokens. *)

type token = 
  | TRUE
  | STRING of (string)
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACK
  | LEFT_PAREN
  | LEFT_BRACK
  | INT of (Bigint.t)
  | ID of (string)
  | FLOAT of (float)
  | FALSE
  | EOF
  | DOT
  | COMMA

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val term: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.Abt.term)
