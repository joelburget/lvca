type token =
  | INT of (Bigint.t)
  | FLOAT of (float)
  | ID of (string)
  | STRING of (string)
  | DOT
  | TRUE
  | FALSE
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACK
  | RIGHT_BRACK
  | SEMICOLON
  | COMMA
  | EOF

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  261 (* DOT *);
  262 (* TRUE *);
  263 (* FALSE *);
  264 (* LEFT_PAREN *);
  265 (* RIGHT_PAREN *);
  266 (* LEFT_BRACK *);
  267 (* RIGHT_BRACK *);
  268 (* SEMICOLON *);
  269 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* ID *);
  260 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\005\000\
\005\000\003\000\003\000\004\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\003\000\004\000\001\000\003\000\001\000\003\000\001\000\003\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\000\000\013\000\014\000\015\000\000\000\
\016\000\005\000\000\000\000\000\000\000\000\000\001\000\009\000\
\000\000\000\000\000\000\004\000\000\000\002\000\000\000\010\000\
\008\000\006\000"

let yydgoto = "\002\000\
\016\000\017\000\013\000\010\000\018\000"

let yysindex = "\003\000\
\013\255\000\000\000\000\250\254\000\000\000\000\000\000\013\255\
\000\000\000\000\002\255\003\255\010\255\005\255\000\000\000\000\
\023\255\018\255\013\255\000\000\021\255\000\000\021\255\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\255\000\000\017\255\000\000\000\000\
\000\000\025\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\255\255\012\000\017\000\000\000\016\000"

let yytablesize = 269
let yytable = "\009\000\
\003\000\011\000\003\000\001\000\014\000\005\000\012\000\006\000\
\007\000\021\000\015\000\008\000\011\000\003\000\019\000\004\000\
\005\000\012\000\006\000\007\000\020\000\003\000\008\000\014\000\
\005\000\003\000\006\000\007\000\003\000\023\000\008\000\022\000\
\011\000\007\000\026\000\024\000\025\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\003\000"

let yycheck = "\001\000\
\000\000\008\001\001\001\001\000\003\001\004\001\008\000\006\001\
\007\001\005\001\009\001\010\001\008\001\001\001\012\001\003\001\
\004\001\019\000\006\001\007\001\011\001\001\001\010\001\003\001\
\004\001\009\001\006\001\007\001\012\001\012\001\010\001\009\001\
\011\001\009\001\023\000\019\000\021\000\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\011\001\012\001"

let yynames_const = "\
  DOT\000\
  TRUE\000\
  FALSE\000\
  LEFT_PAREN\000\
  RIGHT_PAREN\000\
  LEFT_BRACK\000\
  RIGHT_BRACK\000\
  SEMICOLON\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  ID\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 24 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Term (_1, []) )
# 172 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Types.Abt.scope list) in
    Obj.repr(
# 25 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Term (_1, _3) )
# 180 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 26 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Var _1        )
# 187 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Types.Abt.term list) in
    Obj.repr(
# 27 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Sequence _2   )
# 194 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.primitive) in
    Obj.repr(
# 28 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                                          ( Primitive _1  )
# 201 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'scope) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Types.Abt.scope list) in
    Obj.repr(
# 31 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                             ( _1 :: _3 )
# 209 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.scope list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'scope) in
    Obj.repr(
# 32 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                             ( [_1]     )
# 216 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.scope list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'scope) in
    Obj.repr(
# 35 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( match _3 with | Scope (scope, tm) -> Scope (_1 :: scope, tm) )
# 224 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.Abt.term) in
    Obj.repr(
# 36 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                   ( Scope ([], _1) )
# 231 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Types.Abt.term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Types.Abt.term list) in
    Obj.repr(
# 39 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                         ( _1 :: _3 )
# 239 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Types.Abt.term) in
    Obj.repr(
# 40 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
                         ( [_1]     )
# 246 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.Abt.term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Bigint.t) in
    Obj.repr(
# 43 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimInteger _1    )
# 253 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.primitive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimString  _1    )
# 260 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    true  )
# 266 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "/Users/joel/code/lvca-bucklescript/src/termParser.mly"
           ( PrimBool    false )
# 272 "/Users/joel/code/lvca-bucklescript/src/termParser.ml"
               : Types.primitive))
(* Entry term *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let term (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.Abt.term)
