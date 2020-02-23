{
open Regex_Parser
open LexerUtil

module L = Lexing
module B = Buffer
}

let other_char = [^ '[' ']' '(' ')' '\\' '.' '|' '*' '+' '?' '-']

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
  | other_char { CHAR (Core_kernel.String.get (L.lexeme lexbuf) 0) }
  | _   { error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) }
