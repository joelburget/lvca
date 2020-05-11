{
open ConcreteSyntax_Parser
open LexerUtil

module L = Lexing
module B = Buffer
}

let nat            = ['0'-'9'] ['0'-'9']*
let white          = [' ' '\t']+
let newline        = '\r' | '\n' | "\r\n"
let terminal_id    = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*
let nonterminal_id = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*
let escaped        = '\\' ['a'-'z' 'A'-'Z']+

rule read = parse
  | "//" [^ '\r' '\n']* newline
  | white          { read lexbuf }
  | newline        { next_line lexbuf; read lexbuf }
  | nat            { NAT (int_of_string (L.lexeme lexbuf)) }
  | '"'            { read_string (Buffer.create 17) lexbuf }
  | '('            { LEFT_PAREN }
  | ')'            { RIGHT_PAREN }
  | '/'            { read_regex (Buffer.create 17) lexbuf }
  | '{'            { LEFT_BRACE }
  | '}'            { RIGHT_BRACE }
  | '['            { LEFT_BRACKET }
  | ']'            { RIGHT_BRACKET }
  | '<'            { LEFT_ANGLE }
  | '>'            { RIGHT_ANGLE }
  | '.'            { DOT }
  | ":="           { ASSIGN }
  | "="            { EQUALS }
  | ':'            { COLON }
  | '|'            { BAR }
  | ';'            { SEMICOLON }
  | '_'            { UNDERSCORE }
  | ','            { COMMA }
  | terminal_id    { TERMINAL_ID (L.lexeme lexbuf) }
  | nonterminal_id { NONTERMINAL_ID (L.lexeme lexbuf) }
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
  | '\\' '\\' { B.add_char buf '\\'
              ; read_string buf lexbuf
              }
  | '"'       { STRING (B.contents buf) } (* return *)
  | eof       { error lexbuf "end of input inside of a string" }
  | _         { error lexbuf
                  "found '%s' in a string" @@ L.lexeme lexbuf }

and read_regex buf = parse
  | [^ '/' '\n' '\\'] +
    { B.add_string buf @@ L.lexeme lexbuf
    ; read_regex buf lexbuf
    }
  | '\\' '\\'
    { B.add_char buf '\\'
    ; read_regex buf lexbuf
    }
  | '\\' (_ as c)
    { B.add_char buf '\\'
    ; B.add_char buf c
    ; read_regex buf lexbuf
    }
  | '/' { REGEX (B.contents buf) }
  | eof { error lexbuf "end of input inside of a regex" }
  | _   { error lexbuf
          (Printf.sprintf "found '%s' in a regex"
            (L.lexeme lexbuf))
        }
