{
open Statics_Parser
open LexerUtil
}

let white = [' ' '\t' '\r' '\n']+
let opt_white = [' ' '\t' '\r' '\n']*
let id = ['a'-'z' 'A'-'Z' '_' '-'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let named_line = '-' '-'+ opt_white '(' ([^ '\n' ')']+ as name) ')'
let unnamed_line = '-' '-'+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | "//" [^ '\r' '\n']* newline
  | white     { read lexbuf }
  | named_line   { LINE (Some name) }
  | unnamed_line { LINE None }
  | ">>"      { CTX_SEPARATOR }
  | "=>"      { RIGHT_D_ARR }
  | "<="      { LEFT_D_ARR }
  (* | rule_name { RULE_NAME (Lexing.lexeme lexbuf) } (* XXX *) *)
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | ';'       { SEMICOLON }
  | ':'       { COLON }
  | '.'       { DOT }
  | ','       { COMMA }
  | "ctx"     { CTX }
  | id        { ID (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | newline   { next_line lexbuf; read lexbuf }
  | _ { error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
