
(* The type of tokens. *)

type token = 
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACK
  | LEFT_PAREN
  | LEFT_BRACK
  | ID of (string)
  | EOL
  | EOF
  | DOT
  | COMMA
  | BAR
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val languageDef: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.language)
