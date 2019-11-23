# 1 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
 
open ConcreteSyntax_Parser
open LexerUtil

module L = Lexing
module B = Buffer

# 10 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\231\255\232\255\018\000\235\255\236\255\237\255\238\255\
    \239\255\240\255\241\255\000\000\243\255\244\255\245\255\247\255\
    \248\255\249\255\250\255\085\000\163\000\238\000\254\255\001\000\
    \003\000\013\000\082\000\255\255\004\000\242\255\022\000\027\000\
    \027\000\015\000\234\255\040\000\040\000\029\000\233\255\030\001\
    \250\255\251\255\004\000\254\255\031\001\253\255\212\000\250\255\
    \251\255\253\255\017\000\032\001\254\255\083\000\252\255\253\255\
    \254\255\084\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\024\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\024\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\003\000\002\000\255\255\001\000\
    \000\000\009\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\003\000\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\003\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\026\000\000\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\044\000\
    \000\000\000\000\255\255\000\000\044\000\000\000\051\000\000\000\
    \000\000\000\000\255\255\051\000\000\000\057\000\000\000\000\000\
    \000\000\057\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\022\000\022\000\024\000\023\000\027\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\000\000\018\000\024\000\010\000\003\000\045\000\000\000\
    \017\000\016\000\008\000\007\000\000\000\000\000\012\000\025\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\011\000\005\000\026\000\029\000\004\000\006\000\
    \052\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\015\000\027\000\054\000\255\255\028\000\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\014\000\009\000\013\000\031\000\035\000\
    \032\000\033\000\019\000\034\000\030\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\036\000\
    \037\000\038\000\000\000\000\000\000\000\000\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \056\000\255\255\000\000\000\000\019\000\000\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \020\000\000\000\000\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\000\000\047\000\000\000\
    \000\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\000\000\000\000\
    \002\000\000\000\020\000\049\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \043\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \041\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\255\255\055\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\042\000\255\255\255\255\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\048\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\040\000\255\255\
    \255\255";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\023\000\024\000\000\000\028\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\024\000\000\000\000\000\042\000\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\025\000\011\000\000\000\000\000\
    \050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\026\000\053\000\057\000\026\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\030\000\
    \031\000\032\000\019\000\033\000\003\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\035\000\
    \036\000\037\000\255\255\255\255\255\255\255\255\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \053\000\057\000\255\255\255\255\019\000\255\255\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \020\000\255\255\255\255\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\255\255\046\000\255\255\
    \255\255\255\255\255\255\255\255\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\255\255\255\255\
    \000\000\255\255\020\000\046\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \039\000\044\000\051\000\255\255\255\255\255\255\255\255\255\255\
    \046\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \039\000\044\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\051\000\
    \255\255\255\255\026\000\053\000\057\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\039\000\044\000\051\000\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\046\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\039\000\044\000\
    \051\000";
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
# 18 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read lexbuf )
# 200 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 19 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( next_line lexbuf; read lexbuf )
# 205 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 20 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( NAT (int_of_string (L.lexeme lexbuf)) )
# 210 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 21 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( TERMINAL_ID (L.lexeme lexbuf) )
# 215 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 22 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( NONTERMINAL_ID (L.lexeme lexbuf) )
# 220 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 23 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_string (Buffer.create 17) lexbuf )
# 225 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 6 ->
# 24 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_PAREN )
# 230 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 7 ->
# 25 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_PAREN )
# 235 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 8 ->
# 26 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_character_set (Buffer.create 17) lexbuf )
# 240 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 9 ->
# 27 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_regex (Buffer.create 17) lexbuf )
# 245 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 10 ->
# 28 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_BRACE )
# 250 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 11 ->
# 29 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_BRACE )
# 255 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 12 ->
# 30 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( DOT )
# 260 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 13 ->
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( ASSIGN )
# 265 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 14 ->
# 32 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( DOLLAR )
# 270 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 15 ->
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( BAR )
# 275 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 16 ->
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( STAR )
# 280 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 17 ->
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( PLUS )
# 285 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 18 ->
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( QUESTION )
# 290 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 19 ->
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( SEMICOLON )
# 295 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 20 ->
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( GREATER )
# 300 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 21 ->
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_FIXITY )
# 305 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 22 ->
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_FIXITY )
# 310 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 23 ->
# 41 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( EOF )
# 315 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 24 ->
# 42 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
      ( error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) )
# 320 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 39
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 46 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; read_string buf lexbuf
              )
# 334 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 49 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; L.new_line lexbuf
              ; read_string buf lexbuf
              )
# 342 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 53 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_char buf '"'
              ; read_string buf lexbuf
              )
# 349 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_char buf '\\'
              ; read_string buf lexbuf
              )
# 356 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 59 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( STRING (B.contents buf) )
# 361 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( error lexbuf "end of input inside of a string" )
# 366 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 6 ->
# 61 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( error lexbuf
                  "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 372 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

and read_regex buf lexbuf =
   __ocaml_lex_read_regex_rec buf lexbuf 46
and __ocaml_lex_read_regex_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 66 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_string buf @@ L.lexeme lexbuf
    ; read_regex buf lexbuf
    )
# 386 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 70 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_char buf '/'
    ; read_regex buf lexbuf
    )
# 393 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 73 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
         ( REGEX (B.contents buf) )
# 398 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 74 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
         ( B.add_char buf '\\'
         ; read_string buf lexbuf
         )
# 405 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 77 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf "end of input inside of a regex" )
# 410 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 78 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf
          "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 416 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_regex_rec buf lexbuf __ocaml_lex_state

and read_character_set buf lexbuf =
   __ocaml_lex_read_character_set_rec buf lexbuf 53
and __ocaml_lex_read_character_set_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 83 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_string buf @@ L.lexeme lexbuf
    ; read_character_set buf lexbuf
    )
# 430 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 86 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( CHARACTER_SET (B.contents buf) )
# 435 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 87 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf "end of input inside of a character set" )
# 440 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 88 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf
          "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 446 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_character_set_rec buf lexbuf __ocaml_lex_state

;;

