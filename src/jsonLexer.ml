# 1 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
 
open Lexing
open JsonParser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

# 16 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\240\255\241\255\242\255\243\255\244\255\245\255\246\255\
    \247\255\248\255\000\000\000\000\000\000\022\000\078\000\100\000\
    \032\000\254\255\001\000\003\000\088\000\132\000\110\000\142\000\
    \001\000\002\000\251\255\000\000\000\000\003\000\250\255\001\000\
    \003\000\249\255\002\000\245\255\003\000\121\000\255\255\248\255\
    \249\255\250\255\251\255\252\255\253\255\254\255";
  Lexing.lex_backtrk =
   "\003\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\014\000\014\000\014\000\014\000\003\000\002\000\
    \014\000\255\255\001\000\000\000\002\000\255\255\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\008\000\009\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\000\000\036\000\000\000\036\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\017\000\017\000\019\000\018\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\009\000\019\000\038\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\016\000\014\000\000\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\004\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\000\000\023\000\000\000\013\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\006\000\000\000\005\000\037\000\255\255\
    \000\000\027\000\000\000\000\000\000\000\013\000\011\000\026\000\
    \030\000\000\000\000\000\000\000\028\000\032\000\010\000\033\000\
    \000\000\000\000\024\000\029\000\012\000\031\000\025\000\000\000\
    \000\000\000\000\000\000\008\000\000\000\007\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\014\000\021\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \045\000\021\000\000\000\000\000\000\000\000\000\000\000\023\000\
    \000\000\023\000\000\000\021\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\044\000\000\000\000\000\
    \000\000\000\000\000\000\043\000\000\000\000\000\000\000\042\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\041\000\
    \000\000\000\000\000\000\040\000\000\000\039\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\035\000\255\255\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\018\000\019\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\019\000\034\000\036\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\013\000\255\255\013\000\255\255\000\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\255\255\000\000\255\255\000\000\034\000\036\000\
    \255\255\011\000\255\255\255\255\255\255\000\000\000\000\025\000\
    \029\000\255\255\255\255\255\255\027\000\031\000\000\000\032\000\
    \255\255\255\255\012\000\028\000\000\000\010\000\024\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\015\000\014\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \037\000\015\000\255\255\255\255\255\255\255\255\255\255\021\000\
    \255\255\021\000\255\255\014\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \255\255\015\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\037\000\255\255\255\255\
    \255\255\255\255\255\255\037\000\255\255\255\255\255\255\037\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\037\000\
    \255\255\255\255\255\255\037\000\255\255\037\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\034\000\036\000\255\255\255\255\255\255\255\255\
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
# 26 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( read lexbuf )
# 162 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 1 ->
# 27 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( next_line lexbuf; read lexbuf )
# 167 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 2 ->
# 28 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( INT (int_of_string (Lexing.lexeme lexbuf)) )
# 172 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 3 ->
# 29 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( FLOAT (float_of_string (Lexing.lexeme lexbuf)) )
# 177 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 4 ->
# 30 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( TRUE )
# 182 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 5 ->
# 31 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( FALSE )
# 187 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 6 ->
# 32 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( NULL )
# 192 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 7 ->
# 33 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( read_string (Buffer.create 17) lexbuf )
# 197 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 8 ->
# 34 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( LEFT_BRACE )
# 202 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 9 ->
# 35 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( RIGHT_BRACE )
# 207 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 10 ->
# 36 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( LEFT_BRACK )
# 212 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 11 ->
# 37 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( RIGHT_BRACK )
# 217 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 12 ->
# 38 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( COLON )
# 222 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 13 ->
# 39 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( COMMA )
# 227 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 14 ->
# 40 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
      ( raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) )
# 232 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 15 ->
# 41 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
             ( EOF )
# 237 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 16 ->
# 42 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
        ( INT (int_of_string (Lexing.lexeme lexbuf)) )
# 242 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 34
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 46 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( STRING (Buffer.contents buf) )
# 254 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 1 ->
# 47 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( Buffer.add_char buf '/'; read_string buf lexbuf )
# 259 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 2 ->
# 48 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( Buffer.add_char buf '\\'; read_string buf lexbuf )
# 264 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 3 ->
# 49 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( Buffer.add_char buf '\b'; read_string buf lexbuf )
# 269 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 4 ->
# 50 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( Buffer.add_char buf '\012'; read_string buf lexbuf )
# 274 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 5 ->
# 51 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( Buffer.add_char buf '\n'; read_string buf lexbuf )
# 279 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 6 ->
# 52 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( Buffer.add_char buf '\r'; read_string buf lexbuf )
# 284 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 7 ->
# 53 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
              ( Buffer.add_char buf '\t'; read_string buf lexbuf )
# 289 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 8 ->
# 55 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
    ( Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    )
# 296 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 9 ->
# 58 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
      ( raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) )
# 301 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | 10 ->
# 59 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.mll"
        ( raise (SyntaxError ("String is not terminated")) )
# 306 "/Users/joel/code/lvca-bucklescript/src/jsonLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

;;

