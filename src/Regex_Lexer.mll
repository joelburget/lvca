{
open Regex_Parser
}

(* let other_char = [^ '[' ']' '(' ')' '\\' '.' '|' '*' '+' '?' '-'] *)
let number_char = ['0' - '9']
let other_char = ['a' - 'z' 'A' - 'Z' '0' - '9' '_']

rule read = parse
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
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
  | number_char { NUMBER_CHAR (Core_kernel.String.get (Lexing.lexeme lexbuf) 0) }
  | other_char { OTHER_CHAR (Core_kernel.String.get (Lexing.lexeme lexbuf) 0) }
  | _   { LexerUtil.error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
