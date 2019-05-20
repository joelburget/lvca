type token =
  | INT of (Bigint.t)
  | FLOAT of (float)
  | ID of (string)
  | STRING of (string)
  | DOT
  | TRUE
  | FALSE
  | NULL
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | SEMICOLON
  | COMMA
  | EOF

val term :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Demo.term
