# 1 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
 
open ConcreteSyntax_Parser
open LexerUtil

module L = Lexing
module B = Buffer

# 10 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"
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
# 18 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read lexbuf )
# 200 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 19 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( next_line lexbuf; read lexbuf )
# 205 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 20 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( NAT (int_of_string (L.lexeme lexbuf)) )
# 210 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 21 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( TERMINAL_ID (L.lexeme lexbuf) )
# 215 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 22 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( NONTERMINAL_ID (L.lexeme lexbuf) )
# 220 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 23 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_string (Buffer.create 17) lexbuf )
# 225 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 6 ->
# 24 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_PAREN )
# 230 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 7 ->
# 25 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_PAREN )
# 235 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 8 ->
# 26 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_character_set (Buffer.create 17) lexbuf )
# 240 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 9 ->
# 27 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_regex (Buffer.create 17) lexbuf )
# 245 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 10 ->
# 28 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_BRACE )
# 250 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 11 ->
# 29 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_BRACE )
# 255 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 12 ->
# 30 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( DOT )
# 260 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 13 ->
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( ASSIGN )
# 265 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 14 ->
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( DOLLAR )
# 270 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 15 ->
# 33 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( BAR )
# 275 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 16 ->
# 34 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( STAR )
# 280 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 17 ->
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( PLUS )
# 285 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 18 ->
# 36 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( QUESTION )
# 290 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 19 ->
# 37 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( SEMICOLON )
# 295 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 20 ->
# 38 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( GREATER )
# 300 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 21 ->
# 39 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_FIXITY )
# 305 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 22 ->
# 40 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_FIXITY )
# 310 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 23 ->
# 41 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( EOF )
# 315 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 24 ->
# 42 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
      ( error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) )
# 320 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 39
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 46 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; read_string buf lexbuf
              )
# 334 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 49 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; L.new_line lexbuf
              ; read_string buf lexbuf
              )
# 342 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 53 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_char buf '"'
              ; read_string buf lexbuf
              )
# 349 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 56 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_char buf '\\'
              ; read_string buf lexbuf
              )
# 356 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 59 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( STRING (B.contents buf) )
# 361 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 60 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( error lexbuf "end of input inside of a string" )
# 366 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 6 ->
# 61 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( error lexbuf
                  "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 372 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

and read_regex buf lexbuf =
   __ocaml_lex_read_regex_rec buf lexbuf 46
and __ocaml_lex_read_regex_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 66 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_string buf @@ L.lexeme lexbuf
    ; read_regex buf lexbuf
    )
# 386 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 70 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_char buf '/'
    ; read_regex buf lexbuf
    )
# 393 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 73 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
         ( REGEX (B.contents buf) )
# 398 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 74 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
         ( B.add_char buf '\\'
         ; read_string buf lexbuf
         )
# 405 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 77 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf "end of input inside of a regex" )
# 410 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 78 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf
          "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 416 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_regex_rec buf lexbuf __ocaml_lex_state

and read_character_set buf lexbuf =
   __ocaml_lex_read_character_set_rec buf lexbuf 53
and __ocaml_lex_read_character_set_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 83 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_string buf @@ L.lexeme lexbuf
    ; read_character_set buf lexbuf
    )
# 430 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 86 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( CHARACTER_SET (B.contents buf) )
# 435 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 87 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf "end of input inside of a character set" )
# 440 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 88 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf
          "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 446 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_character_set_rec buf lexbuf __ocaml_lex_state

;;

