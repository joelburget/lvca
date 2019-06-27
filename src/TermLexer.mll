{
open TermParser
open LexerUtil

module L = Lexing
module B = Buffer
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*

rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (Bigint.of_string (L.lexeme lexbuf)) }
  (* | float    { FLOAT (float_of_string (L.lexeme lexbuf)) }
  | "true"   { TRUE }
  | "false"  { FALSE } *)
  | id       { ID (L.lexeme lexbuf) }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | ';'      { SEMICOLON }
  (* | ','      { COMMA } *)
  | '.'      { DOT }
  | _ { error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) }
  | eof      { EOF }

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
