{
open Regex_Parser
}

(* let other_char = [^ '[' ']' '(' ')' '\\' '.' '|' '*' '+' '?' '-'] *)
let other_char = ['a' - 'z' 'A' - 'Z' '0' - '9' '_']

rule read = parse
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '\\' { BACKSLASH }
  | '.' { DOT }
  | '|' { BAR }
  | '*' { STAR }
  | '+' { PLUS }
  | '?' { QUESTION }
  | '-' { DASH }
  | eof { EOF }
  | other_char { CHAR (Core_kernel.String.get (Lexing.lexeme lexbuf) 0) }
  | _   { LexerUtil.error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
