# 1 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
 
open LanguageParser
open LexerUtil

# 7 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\246\255\247\255\248\255\249\255\250\255\251\255\252\255\
    \000\000\078\000\002\000\253\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \009\000\001\000\000\000\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\010\000\010\000\010\000\010\000\010\000\000\000\010\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \010\000\000\000\010\000\000\000\000\000\000\000\000\000\000\000\
    \007\000\006\000\000\000\000\000\000\000\009\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\008\000\005\000\000\000\011\000\000\000\000\000\
    \000\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\000\000\000\000\000\000\000\000\009\000\
    \000\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\003\000\000\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\000\000\000\000\000\000\000\000\009\000\000\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\010\000\010\000\000\000\255\255\010\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\010\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\008\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\009\000\000\000\255\255\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\255\255\255\255\255\255\255\255\009\000\255\255\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
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
# 10 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( read lexbuf )
# 125 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 1 ->
# 11 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( ID (Lexing.lexeme lexbuf) )
# 130 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 2 ->
# 12 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( ASSIGN )
# 135 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 3 ->
# 13 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( LEFT_PAREN )
# 140 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 4 ->
# 14 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( RIGHT_PAREN )
# 145 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 5 ->
# 15 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( SEMICOLON )
# 150 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 6 ->
# 16 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( DOT )
# 155 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 7 ->
# 17 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( BAR )
# 160 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 8 ->
# 18 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
             ( EOF )
# 165 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | 9 ->
# 19 "/Users/joel/code/lvca-bucklescript/src/languageLexer.mll"
      ( error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) )
# 170 "/Users/joel/code/lvca-bucklescript/src/languageLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;

