{
open LanguageParser
open LexerUtil
}

let white = [' ' '\t' '\r' '\n']+
let id = ['a'-'z' 'A'-'Z' '_' '-'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let line = '-' '-'+
let rule_name = '(' [^ '\n' ')']+ ')'

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

  (*
and read_statics = parse
  | white     { read_statics lexbuf }
  | line      { LINE }
  | ">>"      { CTX_SEPARATOR }
  | "=>"      { RIGHT_D_ARR }
  | "<="      { LEFT_D_ARR }
  | rule_name { RULE_NAME (Lexing.lexeme lexbuf) } (* XXX *)
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | ';'       { SEMICOLON }
  | '.'       { DOT }
  | id        { ID (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | _ { error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and read_dynamics = parse
  | white     { read_dynamics lexbuf }
  | "[["      { LEFT_OXFORD }
  | "]]"      { RIGHT_OXFORD }
  | '='       { EQ }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | ';'       { SEMICOLON }
  | id        { ID (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | _ { error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
  (* XXX what about binding / lambdas? *)
  *)
