type token =
  | INT of (Bigint.t)
  | FLOAT of (float)
  | ID of (string)
  | STRING of (string)
  | DOT
  | TRUE
  | FALSE
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | SEMICOLON
  | COMMA
  | EOF

val term :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.Abt.term
