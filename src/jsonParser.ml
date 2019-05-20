type token =
  | INT of (int)
  | FLOAT of (float)
  | ID of (string)
  | STRING of (string)
  | TRUE
  | FALSE
  | NULL
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACK
  | RIGHT_BRACK
  | COLON
  | COMMA
  | EOF

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  261 (* TRUE *);
  262 (* FALSE *);
  263 (* NULL *);
  264 (* LEFT_BRACE *);
  265 (* RIGHT_BRACE *);
  266 (* LEFT_BRACK *);
  267 (* RIGHT_BRACK *);
  268 (* COLON *);
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
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\005\000\004\000\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\006\000\007\000\005\000\008\000\009\000\010\000\
\000\000\000\000\002\000\016\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\004\000\013\000\
\011\000\014\000"

let yydgoto = "\002\000\
\012\000\017\000\015\000\018\000\016\000"

let yysindex = "\003\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\255\001\255\000\000\000\000\000\000\000\255\004\255\002\255\
\003\255\008\255\001\255\000\000\006\255\001\255\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\255\
\009\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\252\255\002\000\000\000"

let yytablesize = 267
let yytable = "\013\000\
\011\000\003\000\004\000\001\000\005\000\006\000\007\000\008\000\
\009\000\014\000\010\000\019\000\020\000\012\000\021\000\022\000\
\025\000\024\000\023\000\015\000\000\000\000\000\000\000\026\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\000\000\005\000\006\000\007\000\008\000\
\009\000\000\000\010\000"

let yycheck = "\001\000\
\000\000\001\001\002\001\001\000\004\001\005\001\006\001\007\001\
\008\001\004\001\010\001\012\001\009\001\009\001\013\001\013\001\
\021\000\019\000\011\001\011\001\255\255\255\255\255\255\022\000\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\005\001\006\001\007\001\
\008\001\255\255\010\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  NULL\000\
  LEFT_BRACE\000\
  RIGHT_BRACE\000\
  LEFT_BRACK\000\
  RIGHT_BRACK\000\
  COLON\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 21 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
          ( Some _1 )
# 172 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : Json.value option))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
          ( None   )
# 178 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : Json.value option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'obj_fields) in
    Obj.repr(
# 25 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `Assoc _2   )
# 185 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_fields) in
    Obj.repr(
# 26 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `List _2    )
# 192 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 27 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `String _1  )
# 199 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 28 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `Int _1     )
# 206 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 29 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `Float _1   )
# 213 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `Bool true  )
# 219 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `Bool false )
# 225 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( `Null       )
# 231 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'obj_field) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'obj_fields) in
    Obj.repr(
# 35 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( _1 :: _3 )
# 239 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'obj_fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'obj_field) in
    Obj.repr(
# 36 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( [_1] )
# 246 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'obj_fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 41 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( (_1, _3) )
# 254 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'obj_field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_fields) in
    Obj.repr(
# 44 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( _1 :: _3 )
# 262 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'list_fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 45 "/Users/joel/code/lvca-bucklescript/src/jsonParser.mly"
                                         ( [_1] )
# 269 "/Users/joel/code/lvca-bucklescript/src/jsonParser.ml"
               : 'list_fields))
(* Entry prog *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Json.value option)
