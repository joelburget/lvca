
(* The type of tokens. *)

type token = 
  | SEMICOLON
  | RULE_NAME of (string)
  | RIGHT_PAREN
  | RIGHT_D_ARR
  | LINE
  | LEFT_PAREN
  | LEFT_D_ARR
  | ID of (string)
  | EOF
  | DOT
  | CTX_SEPARATOR
  | CTX

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val term_top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.Statics.term)

val rules: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.Statics.rule list)
