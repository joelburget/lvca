# 1 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
 
open Regex_Parser
open LexerUtil

module L = Lexing
module B = Buffer

# 10 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\244\255\245\255\246\255\247\255\248\255\249\255\250\255\
    \251\255\252\255\001\000\011\000\253\255\254\255\002\000\251\255\
    \252\255\253\255\003\000\004\000\254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\012\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255";
  Lexing.lex_default =
   "\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\012\000\011\000\000\000\000\000\018\000\000\000\
    \000\000\000\000\018\000\018\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\015\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \009\000\008\000\005\000\004\000\000\000\000\000\002\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\000\000\000\000\013\000\000\000\013\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\013\000\000\000\000\000\000\000\
    \013\000\000\000\000\000\007\000\010\000\000\000\019\000\017\000\
    \255\255\020\000\000\000\013\000\000\000\013\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\013\000\000\000\000\000\000\000\
    \013\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\255\255\016\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\014\000\018\000\019\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\011\000\011\000\011\000\011\000\255\255\
    \255\255\011\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\010\000\255\255\010\000\255\255\255\255\
    \255\255\255\255\011\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\010\000\255\255\255\255\255\255\
    \010\000\255\255\255\255\000\000\000\000\255\255\014\000\014\000\
    \018\000\019\000\255\255\010\000\255\255\010\000\011\000\011\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\010\000\255\255\255\255\255\255\
    \010\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\011\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\010\000\014\000\018\000\019\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\011\000";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read lexbuf =
   __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 14 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
          ( CHARS (L.lexeme lexbuf) )
# 115 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 1 ->
# 15 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
               ( CHARACTER_CLASS (L.lexeme lexbuf) )
# 120 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 2 ->
# 16 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
            ( ESCAPED (L.lexeme lexbuf) )
# 125 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 3 ->
# 17 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( LEFT_PAREN )
# 130 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 4 ->
# 18 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( RIGHT_PAREN )
# 135 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 5 ->
# 19 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( read_character_set (Buffer.create 17) lexbuf )
# 140 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 6 ->
# 20 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( BAR )
# 145 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 7 ->
# 21 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( STAR )
# 150 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 8 ->
# 22 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( PLUS )
# 155 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 9 ->
# 23 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( QUESTION )
# 160 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 10 ->
# 24 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( DOT )
# 165 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 11 ->
# 25 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( EOF )
# 170 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 12 ->
# 26 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) )
# 175 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_character_set buf lexbuf =
   __ocaml_lex_read_character_set_rec buf lexbuf 14
and __ocaml_lex_read_character_set_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 30 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
    ( B.add_string buf @@ L.lexeme lexbuf
    ; read_character_set buf lexbuf
    )
# 189 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 1 ->
# 33 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
              ( B.add_char buf ']'
              ; read_character_set buf lexbuf
              )
# 196 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 2 ->
# 36 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( CHARACTER_SET (B.contents buf) )
# 201 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 3 ->
# 37 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( error lexbuf "end of input inside of a character set" )
# 206 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | 4 ->
# 38 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.mll"
        ( error lexbuf
          "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 212 "/home/joel/code/lvca-bucklescript/src/Regex_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_character_set_rec buf lexbuf __ocaml_lex_state

;;

