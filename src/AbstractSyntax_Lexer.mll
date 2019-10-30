{
open AbstractSyntax_Parser
open LexerUtil
}

let white   = [' ' '\t' '\r' '\n']+
let id      = ['a'-'z' 'A'-'Z' '_' '-'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | "//" [^ '\r' '\n']* newline
  | white    { read lexbuf }
  | id       { ID (Lexing.lexeme lexbuf) }
  | ":="     { ASSIGN }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | ';'      { SEMICOLON }
  | '.'      { DOT }
  | '|'      { BAR }
  | eof      { EOF }
  | newline { next_line lexbuf; read lexbuf }
  | _ { error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
