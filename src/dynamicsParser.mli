
(* The type of tokens. *)

type token = 
  | UNDERSCORE
  | STRING of (string)
  | SEMICOLON
  | RIGHT_S_ARR
  | RIGHT_PAREN
  | RIGHT_OXFORD
  | LEFT_PAREN
  | LEFT_OXFORD
  | LAM
  | INT of (Bigint.t)
  | ID of (string)
  | EQ
  | EOF
  | DOT
  | DEFAULT
  | CASE
  | APP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val dynamics: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.Core.denotation_chart)
