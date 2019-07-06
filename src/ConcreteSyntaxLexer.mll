{
open ConcreteSyntaxParser
open LexerUtil

module L = Lexing
module B = Buffer
}

let nat = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let terminal_id    = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*
let nonterminal_id = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*

rule read = parse
  | white          { read lexbuf }
  | newline        { next_line lexbuf; read lexbuf }
  | nat            { NAT (int_of_string (L.lexeme lexbuf)) }
  | terminal_id    { TERMINAL_ID (L.lexeme lexbuf) }
  | nonterminal_id { NONTERMINAL_ID (L.lexeme lexbuf) }
  | '"'            { read_string (Buffer.create 17) lexbuf }
  | '('            { LEFT_PAREN }
  | ')'            { RIGHT_PAREN }
  | '['            { LEFT_BRACK }
  | ']'            { RIGHT_BRACK }
  | '{'            { LEFT_BRACE }
  | '}'            { RIGHT_BRACE }
  | ';'            { SEMICOLON }
  | '.'            { DOT }
  | ":="           { ASSIGN }
  | '$'            { DOLLAR }
  | '|'            { BAR }
  | eof            { EOF }
  | _ { error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) }

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
  | '\\'      { B.add_char buf '\\'
              ; read_string buf lexbuf
              }
  | '"'       { STRING (B.contents buf) } (* return *)
  | eof       { error lexbuf "end of input inside of a string" }
  | _         { error lexbuf
                  "found '%s' - don't know how to handle" @@ L.lexeme lexbuf }
