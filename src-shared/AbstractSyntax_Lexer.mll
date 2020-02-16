{
open AbstractSyntax_Parser
open LexerUtil

module B = Buffer
module L = Lexing
}

let white   = [' ' '\t' '\r' '\n']+
let id      = ['a'-'z' 'A'-'Z' '_' '-'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | "//" [^ '\r' '\n']* newline
  | white    { read lexbuf }
  | "import" { IMPORT }
  | "from"   { FROM }
  | "as"     { AS }
  | id       { ID (Lexing.lexeme lexbuf) }
  | ":="     { ASSIGN }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '.'      { DOT }
  | '|'      { BAR }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | eof      { EOF }
  | newline { next_line lexbuf; read lexbuf }
  | _ { error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and read_string buf = parse (* use buf to build up result *)
  | [^'"' '\n' '\\']+
              { B.add_string buf @@ L.lexeme lexbuf
              ; read_string buf lexbuf
              }
  | '\n'      { B.add_string buf @@ L.lexeme lexbuf
              ; L.new_line lexbuf
              ; read_string buf lexbuf
              }
  | '\\' '"'  { B.add_char buf '"'
              ; read_string buf lexbuf
              }
  | '\\' '\\' { B.add_char buf '\\'
              ; read_string buf lexbuf
              }
  | '"'       { STRING (B.contents buf) } (* return *)
  | eof       { error lexbuf "end of input inside of a string" }
  | _         { error lexbuf
                  "found '%s' - don't know how to handle" @@ L.lexeme lexbuf }
