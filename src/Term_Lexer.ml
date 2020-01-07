# 1 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
 
open Term_Parser
open LexerUtil

module L = Lexing
module B = Buffer

# 10 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\242\255\243\255\244\255\245\255\246\255\247\255\248\255\
    \249\255\250\255\251\255\078\000\153\000\163\000\254\255\001\000\
    \003\000\011\000\004\000\255\255\005\000\211\000\250\255\251\255\
    \002\000\254\255\212\000\252\255\253\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\002\000\013\000\255\255\001\000\
    \000\000\013\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \006\000\255\255\000\000\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\018\000\000\000\255\255\026\000\000\000\000\000\
    \255\255\000\000\026\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\016\000\014\000\014\000\016\000\015\000\019\000\019\000\
    \000\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \016\000\000\000\010\000\016\000\028\000\000\000\000\000\000\000\
    \009\000\008\000\000\000\000\000\004\000\013\000\003\000\017\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\018\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\007\000\000\000\006\000\027\000\011\000\
    \000\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\000\000\000\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\000\000\000\000\000\000\000\000\011\000\000\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\025\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\023\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\022\000\255\255";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\015\000\016\000\000\000\018\000\020\000\
    \255\255\018\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\016\000\024\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\017\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\024\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\011\000\255\255\255\255\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\255\255\255\255\255\255\255\255\011\000\255\255\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\021\000\026\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\021\000\026\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\021\000\
    \026\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\021\000\026\000";
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
# 19 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( read lexbuf )
# 168 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 1 ->
# 20 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( next_line lexbuf; read lexbuf )
# 173 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 2 ->
# 21 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( INT (Bigint.of_string (L.lexeme lexbuf)) )
# 178 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 3 ->
# 22 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( ID (L.lexeme lexbuf) )
# 183 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 4 ->
# 23 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( read_string (Buffer.create 17) lexbuf )
# 188 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 5 ->
# 24 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( LEFT_PAREN )
# 193 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 6 ->
# 25 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( RIGHT_PAREN )
# 198 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 7 ->
# 26 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( LEFT_BRACK )
# 203 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 8 ->
# 27 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( RIGHT_BRACK )
# 208 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 9 ->
# 28 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( SEMICOLON )
# 213 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 10 ->
# 29 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( COMMA )
# 218 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 11 ->
# 30 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( DOT )
# 223 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 12 ->
# 31 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( EOF )
# 228 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 13 ->
# 32 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
             ( error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) )
# 233 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 21
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 36 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; read_string buf lexbuf
              )
# 247 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 1 ->
# 39 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; L.new_line lexbuf
              ; read_string buf lexbuf
              )
# 255 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 2 ->
# 43 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
              ( B.add_char buf '"'
              ; read_string buf lexbuf
              )
# 262 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 3 ->
# 46 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
              ( B.add_char buf '\\'
              ; read_string buf lexbuf
              )
# 269 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 4 ->
# 49 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
              ( STRING (B.contents buf) )
# 274 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 5 ->
# 50 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
              ( error lexbuf "end of input inside of a string" )
# 279 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | 6 ->
# 51 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.mll"
              ( error lexbuf
                  "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 285 "/Users/joel/code/lvca-bucklescript/src/Term_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

;;

