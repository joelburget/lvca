{
open Lexing
open LanguageParser
}

rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf; EOL }
  | id       { ID (Lexing.lexeme lexbuf) }
  | ":="     { ASSIGN }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | ';'      { SEMICOLON }
  | '.'      { DOT }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
