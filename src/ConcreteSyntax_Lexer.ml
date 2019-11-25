# 1 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
 
open ConcreteSyntax_Parser
open LexerUtil

module L = Lexing
module B = Buffer

# 10 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\235\255\236\255\018\000\239\255\240\255\241\255\242\255\
    \000\000\244\255\245\255\246\255\248\255\249\255\250\255\085\000\
    \163\000\238\000\254\255\001\000\003\000\013\000\081\000\255\255\
    \004\000\243\255\022\000\027\000\027\000\015\000\238\255\040\000\
    \040\000\029\000\237\255\030\001\250\255\251\255\004\000\254\255\
    \031\001\253\255\212\000\250\255\251\255\253\255\016\000\032\001\
    \254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\020\000\255\255\255\255\255\255\255\255\
    \020\000\255\255\255\255\255\255\255\255\255\255\255\255\004\000\
    \003\000\002\000\255\255\001\000\000\000\008\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\003\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\003\000\000\000\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\022\000\000\000\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\040\000\000\000\000\000\255\255\000\000\
    \040\000\000\000\047\000\000\000\000\000\000\000\255\255\047\000\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\020\000\018\000\018\000\020\000\019\000\023\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\000\000\014\000\020\000\007\000\003\000\041\000\000\000\
    \013\000\012\000\000\000\000\000\000\000\000\000\009\000\021\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\008\000\005\000\022\000\025\000\004\000\048\000\
    \000\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\023\000\000\000\000\000\024\000\000\000\
    \000\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\011\000\006\000\010\000\027\000\031\000\
    \028\000\029\000\015\000\030\000\026\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\032\000\
    \033\000\034\000\000\000\000\000\000\000\000\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \000\000\000\000\000\000\000\000\015\000\000\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \016\000\000\000\000\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\000\000\043\000\000\000\
    \000\000\000\000\000\000\000\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\000\000\000\000\
    \002\000\000\000\016\000\045\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \039\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \037\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\038\000\255\255\255\255\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\044\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\036\000\255\255\
    \255\255";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\019\000\020\000\000\000\024\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\020\000\000\000\000\000\038\000\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\021\000\008\000\000\000\046\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\022\000\255\255\255\255\022\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\026\000\
    \027\000\028\000\015\000\029\000\003\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\031\000\
    \032\000\033\000\255\255\255\255\255\255\255\255\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \255\255\255\255\255\255\255\255\015\000\255\255\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \016\000\255\255\255\255\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\255\255\042\000\255\255\
    \255\255\255\255\255\255\255\255\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\255\255\255\255\
    \000\000\255\255\016\000\042\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \035\000\040\000\047\000\255\255\255\255\255\255\255\255\255\255\
    \042\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\000\040\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\047\000\
    \255\255\022\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\035\000\040\000\047\000\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\042\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\035\000\040\000\
    \047\000";
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
# 197 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 19 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( next_line lexbuf; read lexbuf )
# 202 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 20 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( NAT (int_of_string (L.lexeme lexbuf)) )
# 207 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 21 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( TERMINAL_ID (L.lexeme lexbuf) )
# 212 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 22 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( NONTERMINAL_ID (L.lexeme lexbuf) )
# 217 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 23 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_string (Buffer.create 17) lexbuf )
# 222 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 6 ->
# 24 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_PAREN )
# 227 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 7 ->
# 25 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_PAREN )
# 232 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 8 ->
# 26 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( read_regex (Buffer.create 17) lexbuf )
# 237 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 9 ->
# 27 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_BRACE )
# 242 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 10 ->
# 28 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_BRACE )
# 247 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 11 ->
# 29 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( DOT )
# 252 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 12 ->
# 30 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( ASSIGN )
# 257 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 13 ->
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( DOLLAR )
# 262 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 14 ->
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( BAR )
# 267 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 15 ->
# 33 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( SEMICOLON )
# 272 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 16 ->
# 34 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( GREATER )
# 277 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 17 ->
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( LEFT_FIXITY )
# 282 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 18 ->
# 36 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( RIGHT_FIXITY )
# 287 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 19 ->
# 37 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
                   ( EOF )
# 292 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 20 ->
# 38 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
      ( error lexbuf ("Unexpected char: " ^ L.lexeme lexbuf) )
# 297 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 35
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 42 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; read_string buf lexbuf
              )
# 311 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 45 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; L.new_line lexbuf
              ; read_string buf lexbuf
              )
# 319 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 49 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_char buf '"'
              ; read_string buf lexbuf
              )
# 326 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 52 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( B.add_char buf '\\'
              ; read_string buf lexbuf
              )
# 333 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 55 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( STRING (B.contents buf) )
# 338 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 56 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( error lexbuf "end of input inside of a string" )
# 343 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 6 ->
# 57 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
              ( error lexbuf
                  "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 349 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

and read_regex buf lexbuf =
   __ocaml_lex_read_regex_rec buf lexbuf 42
and __ocaml_lex_read_regex_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 62 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_string buf @@ L.lexeme lexbuf
    ; read_regex buf lexbuf
    )
# 363 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 1 ->
# 66 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
    ( B.add_char buf '/'
    ; read_regex buf lexbuf
    )
# 370 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 2 ->
# 69 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
         ( REGEX (B.contents buf) )
# 375 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 3 ->
# 70 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
         ( B.add_char buf '\\'
         ; read_string buf lexbuf
         )
# 382 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 4 ->
# 73 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf "end of input inside of a regex" )
# 387 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | 5 ->
# 74 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.mll"
        ( error lexbuf
          (Printf.sprintf "found '%s' - don't know how to handle"
            (L.lexeme lexbuf))
        )
# 395 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_regex_rec buf lexbuf __ocaml_lex_state

;;
