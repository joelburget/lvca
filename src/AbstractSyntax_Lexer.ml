# 1 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
 
open AbstractSyntax_Parser
open LexerUtil

module B = Buffer
module L = Lexing

# 10 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\239\255\241\255\242\255\243\255\244\255\245\255\246\255\
    \247\255\248\255\249\255\250\255\000\000\081\000\160\000\238\000\
    \005\000\007\000\001\000\040\000\255\255\001\000\060\001\138\001\
    \216\001\038\002\116\002\194\002\016\003\094\003\251\255\207\003\
    \250\255\251\255\001\000\254\255\208\003\253\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\016\000\003\000\003\000\003\000\
    \000\000\000\000\016\000\255\255\255\255\000\000\003\000\003\000\
    \003\000\003\000\001\000\003\000\003\000\002\000\255\255\255\255\
    \255\255\255\255\003\000\255\255\000\000\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\019\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\036\000\
    \000\000\000\000\255\255\000\000\036\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\016\000\016\000\020\000\000\000\017\000\016\000\016\000\
    \016\000\016\000\016\000\000\000\016\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \016\000\000\000\003\000\037\000\000\000\016\000\000\000\016\000\
    \011\000\010\000\000\000\000\000\006\000\013\000\005\000\018\000\
    \019\000\000\000\020\000\000\000\000\000\021\000\000\000\000\000\
    \000\000\000\000\012\000\007\000\000\000\030\000\000\000\000\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\000\000\000\000\000\000\000\000\013\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\014\000\013\000\
    \013\000\015\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\009\000\004\000\008\000\013\000\000\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\000\000\000\000\000\000\
    \013\000\000\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\013\000\000\000\000\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\000\000\000\000\000\000\000\000\013\000\
    \002\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\027\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\000\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\000\000\000\000\000\000\000\000\013\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\022\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\000\000\000\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000\
    \000\000\000\000\000\000\013\000\000\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\023\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \000\000\000\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\000\000\000\000\000\000\
    \000\000\013\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\024\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\000\000\000\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\000\000\000\000\000\000\000\000\013\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\025\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\000\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\000\000\000\000\000\000\000\000\013\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\026\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\000\000\000\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000\
    \000\000\000\000\000\000\013\000\000\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \000\000\000\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\000\000\000\000\000\000\
    \000\000\013\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\028\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\000\000\000\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\000\000\000\000\000\000\000\000\013\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\029\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\000\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\000\000\000\000\000\000\000\000\013\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\035\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\033\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\034\000\255\255\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
    \255\255";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\021\000\255\255\000\000\016\000\016\000\
    \017\000\017\000\016\000\255\255\017\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\034\000\255\255\016\000\255\255\017\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \018\000\255\255\019\000\255\255\255\255\019\000\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\012\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\013\000\255\255\
    \255\255\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\255\255\255\255\255\255\255\255\
    \013\000\255\255\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\255\255\014\000\255\255\255\255\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\255\255\255\255\255\255\255\255\014\000\
    \000\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\015\000\255\255\255\255\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\255\255\255\255\255\255\255\255\015\000\255\255\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\022\000\255\255\255\255\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\255\255\
    \255\255\255\255\255\255\022\000\255\255\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\023\000\
    \255\255\255\255\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\255\255\255\255\255\255\
    \255\255\023\000\255\255\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\024\000\255\255\255\255\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\255\255\255\255\255\255\255\255\024\000\
    \255\255\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\025\000\255\255\255\255\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\255\255\255\255\255\255\255\255\025\000\255\255\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\026\000\255\255\255\255\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\255\255\
    \255\255\255\255\255\255\026\000\255\255\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\027\000\
    \255\255\255\255\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\255\255\255\255\255\255\
    \255\255\027\000\255\255\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\028\000\255\255\255\255\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\255\255\255\255\255\255\255\255\028\000\
    \255\255\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\029\000\255\255\255\255\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\255\255\255\255\255\255\255\255\029\000\255\255\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\031\000\036\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\031\000\036\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\031\000\036\000\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\031\000\
    \036\000";
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
# 15 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( read lexbuf )
# 363 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 1 ->
# 16 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( IMPORT )
# 368 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 2 ->
# 17 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( FROM )
# 373 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 3 ->
# 18 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( ID (Lexing.lexeme lexbuf) )
# 378 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 4 ->
# 19 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( ASSIGN )
# 383 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 5 ->
# 20 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( LEFT_PAREN )
# 388 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 6 ->
# 21 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( RIGHT_PAREN )
# 393 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 7 ->
# 22 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( LEFT_BRACE )
# 398 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 8 ->
# 23 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( RIGHT_BRACE )
# 403 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 9 ->
# 24 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( SEMICOLON )
# 408 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 10 ->
# 25 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( COMMA )
# 413 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 11 ->
# 26 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( DOT )
# 418 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 12 ->
# 27 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( BAR )
# 423 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 13 ->
# 28 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( read_string (Buffer.create 17) lexbuf )
# 428 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 14 ->
# 29 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
             ( EOF )
# 433 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 15 ->
# 30 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
            ( next_line lexbuf; read lexbuf )
# 438 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 16 ->
# 31 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
      ( error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) )
# 443 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 31
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 35 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; read_string buf lexbuf
              )
# 457 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 1 ->
# 38 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
              ( B.add_string buf @@ L.lexeme lexbuf
              ; L.new_line lexbuf
              ; read_string buf lexbuf
              )
# 465 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 2 ->
# 42 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
              ( B.add_char buf '"'
              ; read_string buf lexbuf
              )
# 472 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 3 ->
# 45 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
              ( B.add_char buf '\\'
              ; read_string buf lexbuf
              )
# 479 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 4 ->
# 48 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
              ( STRING (B.contents buf) )
# 484 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 5 ->
# 49 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
              ( error lexbuf "end of input inside of a string" )
# 489 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | 6 ->
# 50 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.mll"
              ( error lexbuf
                  "found '%s' - don't know how to handle" @@ L.lexeme lexbuf )
# 495 "/Users/joel/code/lvca-bucklescript/src/AbstractSyntax_Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

;;

