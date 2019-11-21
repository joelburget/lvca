{
open Regex_Parser
open LexerUtil

module L = Lexing
module B = Buffer
}

let chars = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']+
let char_class = '\\' ['w' 's' 'd' 'b']
let escaped = '\\' [^ 'w' 's' 'd' 'b']

rule read = parse
  (* | '"' { read_string (Buffer.create 17) lexbuf } *)
  (* TODO: accept more chars *)
  | chars { CHARS (L.lexeme lexbuf) }
  | char_class { CHARACTER_CLASS (L.lexeme lexbuf) }
  | escaped { ESCAPED (L.lexeme lexbuf) }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { read_character_set (Buffer.create 17) lexbuf }
  | '|' { BAR }
  | '*' { STAR }
  | '+' { PLUS }
  | '?' { QUESTION }
  | '.' { DOT }
  | eof { EOF }
  | _   { error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) }

(*
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
*)

and read_character_set buf = parse
  | [^ ']' '\n'] +
    { B.add_string buf @@ L.lexeme lexbuf
    ; read_character_set buf lexbuf
    }
  | ']' { CHARACTER_SET (B.contents buf) } (* return *)
  | eof { error lexbuf "end of input inside of a character set" }
  | _   { error lexbuf
          "found '%s' - don't know how to handle" @@ L.lexeme lexbuf }
