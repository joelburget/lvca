type token =
  | INT of (int)
  | FLOAT of (float)
  | ID of (string)
  | STRING of (string)
  | TRUE
  | FALSE
  | NULL
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACK
  | RIGHT_BRACK
  | COLON
  | COMMA
  | EOF

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Json.value option
