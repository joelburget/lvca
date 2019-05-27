{
open LanguageParser
open LexerUtil
}

let white = [' ' '\t' '\r' '\n']+
let id = ['a'-'z' 'A'-'Z' '_' '-'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*

rule read = parse
  | white    { read lexbuf }
  | id       { ID (Lexing.lexeme lexbuf) }
  | ":="     { ASSIGN }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | ';'      { SEMICOLON }
  | '.'      { DOT }
  | '|'      { BAR }
  | eof      { EOF }
  | _ { error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
