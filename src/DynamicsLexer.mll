{
open DynamicsParser
open LexerUtil

module L = Lexing
module B = Buffer
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t' '\r' '\n']+
let id = ['a'-'z' 'A'-'Z' '_' '-'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*

rule read = parse
  | white     { read lexbuf }
  | "[["      { LEFT_OXFORD }
  | "]]"      { RIGHT_OXFORD }
  | "["       { LEFT_BRACKET }
  | "]"       { RIGHT_BRACKET }
  | '"'       { read_string (Buffer.create 17) lexbuf }
  | '_'       { UNDERSCORE }
  | '='       { EQ }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | ';'       { SEMICOLON }
  | ':'       { COLON }
  | ','       { COMMA }
  | '.'       { DOT }
  | "->"      { RIGHT_S_ARR }
  | "#"       { HASH }
  | "app"     { APP }
  | "lam"     { LAM }
  | "case"    { CASE }
  | "default" { DEFAULT }
  | int       { INT (Bigint.of_string (L.lexeme lexbuf)) }
  | id        { ID (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | _ { error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
  (* XXX what about binding / lambdas? *)

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
