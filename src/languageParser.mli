
(* The type of tokens. *)

type token = 
  | SEMICOLON
  | RULE_NAME of (string)
  | RIGHT_S_ARR
  | RIGHT_PAREN
  | RIGHT_OXFORD
  | RIGHT_D_ARR
  | RIGHT_BRACK
  | LINE
  | LEFT_S_ARR
  | LEFT_PAREN
  | LEFT_OXFORD
  | LEFT_D_ARR
  | LEFT_BRACK
  | ID of (string)
  | EQ
  | EOF
  | DOT
  | CTX_SEPARATOR
  | COMMA
  | BAR
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val languageDef: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.language)
