
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20190924

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | STRING of (
# 36 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 17 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_BRACKET
    | RIGHT_BRACE
    | MATCH
    | LET
    | LEFT_PAREN
    | LEFT_BRACKET
    | LEFT_BRACE
    | INT of (
# 35 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (Bigint.t)
# 31 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | IN
    | ID of (
# 37 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 37 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | EQ
    | EOF
    | DOT
    | COMMA
    | COLON
    | BAR
    | BACKSLASH
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 3 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  
open Core

exception InvalidSort

(* raises InvalidSort *)
let rec ast_to_sort' : NonBinding.term -> Types.sort
  = function
      | Operator (name, subtms)
      -> SortAp (name, subtms
        |> Tablecloth.List.map ~f:ast_to_sort'
        |> Tablecloth.Array.from_list
      )
      | Sequence _ | Primitive _
      -> raise InvalidSort

(* raises ScopeEncountered, InvalidSort *)
let ast_to_sort : Binding.Nominal.term -> Types.sort
  = fun term -> term |> NonBinding.from_nominal' |> ast_to_sort'

let rec ast_to_core : Binding.Nominal.term -> core
  = function
  | Var v -> Var v
  | Operator (name, subtms)
  -> Operator (name, Tablecloth.List.map subtms ~f:ast_to_core_scope)
  | Sequence subtms -> Sequence (Tablecloth.List.map subtms ~f:ast_to_core)
  | Primitive p -> Primitive p

and ast_to_core_scope : Binding.Nominal.scope -> core_scope
  = fun (Scope (binders, body)) -> Scope (binders, ast_to_core body)

# 87 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | ARROW ->
          22
      | BACKSLASH ->
          21
      | BAR ->
          20
      | COLON ->
          19
      | COMMA ->
          18
      | DOT ->
          17
      | EOF ->
          16
      | EQ ->
          15
      | ID _ ->
          14
      | IN ->
          13
      | INT _ ->
          12
      | LEFT_BRACE ->
          11
      | LEFT_BRACKET ->
          10
      | LEFT_PAREN ->
          9
      | LET ->
          8
      | MATCH ->
          7
      | RIGHT_BRACE ->
          6
      | RIGHT_BRACKET ->
          5
      | RIGHT_PAREN ->
          4
      | SEMICOLON ->
          3
      | STRING _ ->
          2
      | WITH ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | ARROW ->
          Obj.repr ()
      | BACKSLASH ->
          Obj.repr ()
      | BAR ->
          Obj.repr ()
      | COLON ->
          Obj.repr ()
      | COMMA ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | EQ ->
          Obj.repr ()
      | ID _v ->
          Obj.repr _v
      | IN ->
          Obj.repr ()
      | INT _v ->
          Obj.repr _v
      | LEFT_BRACE ->
          Obj.repr ()
      | LEFT_BRACKET ->
          Obj.repr ()
      | LEFT_PAREN ->
          Obj.repr ()
      | LET ->
          Obj.repr ()
      | MATCH ->
          Obj.repr ()
      | RIGHT_BRACE ->
          Obj.repr ()
      | RIGHT_BRACKET ->
          Obj.repr ()
      | RIGHT_PAREN ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
      | STRING _v ->
          Obj.repr _v
      | WITH ->
          Obj.repr ()
  
  and default_reduction =
    (8, "\000\000\000%\000\000\000$\000\000\026\024\005\000\000+#\000\002\000\000\007\000\000-\000\022\000\"\000\004\000\000)\000\000\000\000\000\000\000\000/.\000\030\000\000\016\n\000\028\t\006\000\r\000\000\012\000\000\000 \000\000\011\000\000'\000\000\015\014\017\000\018\001\000\020")
  
  and error =
    (23, "\000\002\128\000\002\000\135\168\016\000\000\002\030\160D\005@\t\n\136\000\000\000\127\239\238PT\128\000\000\000\000\000\000\000\000\000\000\b\b\n\128\000\000\000\000\000\000\016\000\000\000\000\000\000\002\002\002\160\000\000\000\006\000\000\016\021 \000\000\0000\000\128\000\000\000 \001\000\000\000\000\128\000\000\000\000\002\000\016 *\000\000\000\000\000\004\001\015P\"\030\160@\b\000\000\000\128\000\000\b *\000\016\000\000\000\000\000\000\000\000\004\000 \000\000\000\000\000\144\245\002\000\000\000\000\000\001\175\1860\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\004\000C\212\b\000\000\002\000\000\000\001\000\004\005A\000\000\000\016\021\000\002\000\000\000\000\000\b\000!\001P\000\000\000\000\000\000Hz\129\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000P\000\000\000\000")
  
  and start =
    1
  
  and action =
    ((8, "B\006*\000***\000\003D\000\000\000\030*\000\000H\000B*\000rD\000n\000L\000r\000^*\000b**Zhb*\130\000\000Z\000`*\000\000*\000\000\000\134\000x*\000\146\128V\000*\140\000\b*\000n*\000\000\000|\000\000B\000"), (16, "\000\t\000\t\000\t\000\t\000\t\000\t\000\t\000\t\000&\000\t\000\149\000\t\000\t\000\t\000\t\000\t\000\t\000\t\000\n\000\t\000\t\000\t\000i\000\014\001\014\000i\000Q\000i\000\018\000\022\000\146\000\026\000:\000\030\000i\000\"\000\014\000i\000a\000Q\000J\000i\000\150\000\129\000\026\000y\000\030\000\006\000\"\000I\000R\000Y\000\157\000y\000\154\000y\000r\000y\000\029\000\029\000^\000\173\000z\000\250\000\142\000\130\000\158\000q\000\162\000\170\000\190\000\222\000\137\000\230\000\242\000\246\001\006\001\026\001/"))
  
  and lhs =
    (8, "\000\025\025\025\025\024\023\023\022\022\022\022\022\021\020\019\018\017\016\016\015\015\014\014\r\r\012\012\011\011\n\n\t\t\b\007\007\006\006\005\005\004\004\003\003\002\001")
  
  and goto =
    ((8, "\164\000\028\000,\174\020\000\000\007\000\000\000\000\003\000\000\000\000\000\014\000\000\n\000\000\000\015\000\000\000\000\198\000\000<L\132\000\000\206\000\000\000\214\000\000\\\000\000\136\000\000\000\000\000\000l\000\000\000\026\000\152\000\000\000\164\000\000|\000\000\000\000\000\000\000\000"), (8, "\011\012\030\016\r\014\r\014\025\012\018\020\r\014\r\027O\rN\020\023\r\026@\017\0281\000\023\r\026\000\022J1 3\r56\000<1\0003\r56\00091\0003\r56\00071\0003\r56\00021-3\r56\000;1\r3/564HA\r3\00056E\r3\00056\r#CFKMNICF\"*\rI-\000\r\000\017\000\000\000\000\000.\000\000\000\000\000 \000\000\000,"))
  
  and semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _4;
          MenhirLib.EngineTypes.startp = _startpos__4_;
          MenhirLib.EngineTypes.endp = _endpos__4_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = xs;
            MenhirLib.EngineTypes.startp = _startpos_xs_;
            MenhirLib.EngineTypes.endp = _endpos_xs_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _;
              MenhirLib.EngineTypes.semv = _2;
              MenhirLib.EngineTypes.startp = _startpos__2_;
              MenhirLib.EngineTypes.endp = _endpos__2_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = _1;
                MenhirLib.EngineTypes.startp = _startpos__1_;
                MenhirLib.EngineTypes.endp = _endpos__1_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let xs : 'tv_loption_separated_nonempty_list_SEMICOLON_ast_like_scope__ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 37 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 245 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 253 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 257 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 114 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Operator (_1, _3) )
# 262 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 37 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 283 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 291 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 115 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       ( Var _1 )
# 295 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _4;
          MenhirLib.EngineTypes.startp = _startpos__4_;
          MenhirLib.EngineTypes.endp = _endpos__4_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _3;
            MenhirLib.EngineTypes.startp = _startpos__3_;
            MenhirLib.EngineTypes.endp = _endpos__3_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _;
              MenhirLib.EngineTypes.semv = xs;
              MenhirLib.EngineTypes.startp = _startpos_xs_;
              MenhirLib.EngineTypes.endp = _endpos_xs_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = _1;
                MenhirLib.EngineTypes.startp = _startpos__1_;
                MenhirLib.EngineTypes.endp = _endpos__1_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let _3 : 'tv_option_COMMA_ = Obj.magic _3 in
        let xs : 'tv_loption_separated_nonempty_list_COMMA_ast_like__ = Obj.magic xs in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 341 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _2 = 
# 232 "<standard.mly>"
    ( xs )
# 345 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 117 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Sequence _2 )
# 350 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 64 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.primitive)
# 371 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 379 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 118 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
              ( Primitive _1 )
# 383 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 404 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 63 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 412 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 105 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                                ( ast_to_core _1 )
# 416 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _3;
          MenhirLib.EngineTypes.startp = _startpos__3_;
          MenhirLib.EngineTypes.endp = _endpos__3_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = xs;
              MenhirLib.EngineTypes.startp = _startpos_xs_;
              MenhirLib.EngineTypes.endp = _endpos_xs_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 449 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let xs : 'tv_loption_separated_nonempty_list_DOT_pattern__ = Obj.magic xs in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 68 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 459 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _1 = 
# 232 "<standard.mly>"
    ( xs )
# 463 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 123 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Scope (_1, _3) )
# 468 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 489 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 68 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 497 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 125 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Scope ([], _1) )
# 501 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 63 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 522 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_atomic_core = 
# 84 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                ( _1 )
# 530 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _4;
          MenhirLib.EngineTypes.startp = _startpos__4_;
          MenhirLib.EngineTypes.endp = _endpos__4_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _3;
            MenhirLib.EngineTypes.startp = _startpos__3_;
            MenhirLib.EngineTypes.endp = _endpos__3_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _;
              MenhirLib.EngineTypes.semv = _2;
              MenhirLib.EngineTypes.startp = _startpos__2_;
              MenhirLib.EngineTypes.endp = _endpos__2_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = _1;
                MenhirLib.EngineTypes.startp = _startpos__1_;
                MenhirLib.EngineTypes.endp = _endpos__1_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 569 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : unit = Obj.magic _3 in
        let _2 : 'tv_nonempty_list_typed_arg_ = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_atomic_core = 
# 86 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  (
    let sorts, args = Placemat.List.unzip _2 in
    Lambda (sorts, Scope (args, _4))
  )
# 583 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _7;
          MenhirLib.EngineTypes.startp = _startpos__7_;
          MenhirLib.EngineTypes.endp = _endpos__7_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _6;
            MenhirLib.EngineTypes.startp = _startpos__6_;
            MenhirLib.EngineTypes.endp = _endpos__6_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _;
              MenhirLib.EngineTypes.semv = _5;
              MenhirLib.EngineTypes.startp = _startpos__5_;
              MenhirLib.EngineTypes.endp = _endpos__5_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _;
                MenhirLib.EngineTypes.semv = _4;
                MenhirLib.EngineTypes.startp = _startpos__4_;
                MenhirLib.EngineTypes.endp = _endpos__4_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.state = _;
                  MenhirLib.EngineTypes.semv = _3;
                  MenhirLib.EngineTypes.startp = _startpos__3_;
                  MenhirLib.EngineTypes.endp = _endpos__3_;
                  MenhirLib.EngineTypes.next = {
                    MenhirLib.EngineTypes.state = _;
                    MenhirLib.EngineTypes.semv = _2;
                    MenhirLib.EngineTypes.startp = _startpos__2_;
                    MenhirLib.EngineTypes.endp = _endpos__2_;
                    MenhirLib.EngineTypes.next = {
                      MenhirLib.EngineTypes.state = _menhir_s;
                      MenhirLib.EngineTypes.semv = _1;
                      MenhirLib.EngineTypes.startp = _startpos__1_;
                      MenhirLib.EngineTypes.endp = _endpos__1_;
                      MenhirLib.EngineTypes.next = _menhir_stack;
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _7 : unit = Obj.magic _7 in
        let _6 : 'tv_separated_nonempty_list_BAR_case_line_ = Obj.magic _6 in
        let _5 : 'tv_option_BAR_ = Obj.magic _5 in
        let _4 : unit = Obj.magic _4 in
        let _3 : unit = Obj.magic _3 in
        let _2 : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 645 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : 'tv_atomic_core = 
# 91 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Case (_2, _6) )
# 654 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _6;
          MenhirLib.EngineTypes.startp = _startpos__6_;
          MenhirLib.EngineTypes.endp = _endpos__6_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _5;
            MenhirLib.EngineTypes.startp = _startpos__5_;
            MenhirLib.EngineTypes.endp = _endpos__5_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _;
              MenhirLib.EngineTypes.semv = _4;
              MenhirLib.EngineTypes.startp = _startpos__4_;
              MenhirLib.EngineTypes.endp = _endpos__4_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _;
                MenhirLib.EngineTypes.semv = _3;
                MenhirLib.EngineTypes.startp = _startpos__3_;
                MenhirLib.EngineTypes.endp = _endpos__3_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.state = _;
                  MenhirLib.EngineTypes.semv = _2;
                  MenhirLib.EngineTypes.startp = _startpos__2_;
                  MenhirLib.EngineTypes.endp = _endpos__2_;
                  MenhirLib.EngineTypes.next = {
                    MenhirLib.EngineTypes.state = _menhir_s;
                    MenhirLib.EngineTypes.semv = _1;
                    MenhirLib.EngineTypes.startp = _startpos__1_;
                    MenhirLib.EngineTypes.endp = _endpos__1_;
                    MenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _6 : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 705 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _6 in
        let _5 : unit = Obj.magic _5 in
        let _4 : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 711 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : unit = Obj.magic _3 in
        let _2 : (
# 66 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Pattern.t)
# 717 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_atomic_core = 
# 93 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Let (_4, Scope([_2], _6)) )
# 726 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _3;
          MenhirLib.EngineTypes.startp = _startpos__3_;
          MenhirLib.EngineTypes.endp = _endpos__3_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _1;
              MenhirLib.EngineTypes.startp = _startpos__1_;
              MenhirLib.EngineTypes.endp = _endpos__1_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let _2 : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 760 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_atomic_core = 
# 95 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( _2 )
# 769 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 790 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_binding_aware_pattern = 
# 109 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                                ( BindingAwarePattern.from_ast _1 )
# 798 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _3;
          MenhirLib.EngineTypes.startp = _startpos__3_;
          MenhirLib.EngineTypes.endp = _endpos__3_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _1;
              MenhirLib.EngineTypes.startp = _startpos__1_;
              MenhirLib.EngineTypes.endp = _endpos__1_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 831 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_binding_aware_pattern = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_case_line = 
# 101 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                                            ( CaseScope ([_1], _3) )
# 841 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : 'tv_nonempty_list_atomic_core_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 866 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 76 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( match _1 with
      | [] -> failwith "invariant violation: must be a nonempty list"
      | [x] -> x
      | f :: args -> CoreApp (f, args)
  )
# 874 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _3;
          MenhirLib.EngineTypes.startp = _startpos__3_;
          MenhirLib.EngineTypes.endp = _endpos__3_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _1;
              MenhirLib.EngineTypes.startp = _startpos__1_;
              MenhirLib.EngineTypes.endp = _endpos__1_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : (
# 62 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 907 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 37 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 913 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 69 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (string * Core.core)
# 921 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 132 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                       ( (_1, _3) )
# 925 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _2;
          MenhirLib.EngineTypes.startp = _startpos__2_;
          MenhirLib.EngineTypes.endp = _endpos__2_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_list_definition_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 70 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_chart)
# 957 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 135 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                               ( DenotationChart _1 )
# 961 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_list_definition_ = 
# 211 "<standard.mly>"
    ( [] )
# 979 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_list_definition_ = Obj.magic xs in
        let x : (
# 69 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (string * Core.core)
# 1007 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_definition_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1015 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_ast_like__ = 
# 142 "<standard.mly>"
    ( [] )
# 1033 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_COMMA_ast_like_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_ast_like__ = 
# 144 "<standard.mly>"
    ( x )
# 1058 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_DOT_pattern__ = 
# 142 "<standard.mly>"
    ( [] )
# 1076 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_DOT_pattern_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_DOT_pattern__ = 
# 144 "<standard.mly>"
    ( x )
# 1101 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_ast_like_scope__ = 
# 142 "<standard.mly>"
    ( [] )
# 1119 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_SEMICOLON_ast_like_scope_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_ast_like_scope__ = 
# 144 "<standard.mly>"
    ( x )
# 1144 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_atomic_core = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_atomic_core_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 1169 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_nonempty_list_atomic_core_ = Obj.magic xs in
        let x : 'tv_atomic_core = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_atomic_core_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 1201 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_typed_arg = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_typed_arg_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 1226 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_nonempty_list_typed_arg_ = Obj.magic xs in
        let x : 'tv_typed_arg = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_typed_arg_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 1258 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option_BAR_ = 
# 114 "<standard.mly>"
    ( None )
# 1276 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : unit = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_BAR_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1301 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option_COMMA_ = 
# 114 "<standard.mly>"
    ( None )
# 1319 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : unit = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_COMMA_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1344 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 1365 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 66 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Pattern.t)
# 1373 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 107 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                                ( Binding.Nominal.to_pattern_exn _1 )
# 1377 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 35 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (Bigint.t)
# 1398 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 64 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.primitive)
# 1406 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 128 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
           ( PrimInteger _1 )
# 1410 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 36 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 1431 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 64 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.primitive)
# 1439 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 129 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
           ( PrimString  _1 )
# 1443 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_case_line = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_BAR_case_line_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1468 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_BAR_case_line_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_case_line = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_BAR_case_line_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1507 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 1528 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_ast_like_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1536 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_COMMA_ast_like_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 1571 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_ast_like_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1579 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 66 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Pattern.t)
# 1600 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_pattern_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1608 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_DOT_pattern_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 66 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Pattern.t)
# 1643 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_pattern_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1651 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 68 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 1672 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_ast_like_scope_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1680 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_SEMICOLON_ast_like_scope_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 68 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 1715 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_ast_like_scope_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1723 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 67 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 1744 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 65 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.sort)
# 1752 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 104 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                                ( ast_to_sort       _1 )
# 1756 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _;
          MenhirLib.EngineTypes.semv = _5;
          MenhirLib.EngineTypes.startp = _startpos__5_;
          MenhirLib.EngineTypes.endp = _endpos__5_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _4;
            MenhirLib.EngineTypes.startp = _startpos__4_;
            MenhirLib.EngineTypes.endp = _endpos__4_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _;
              MenhirLib.EngineTypes.semv = _3;
              MenhirLib.EngineTypes.startp = _startpos__3_;
              MenhirLib.EngineTypes.endp = _endpos__3_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _;
                MenhirLib.EngineTypes.semv = _2;
                MenhirLib.EngineTypes.startp = _startpos__2_;
                MenhirLib.EngineTypes.endp = _endpos__2_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.state = _menhir_s;
                  MenhirLib.EngineTypes.semv = _1;
                  MenhirLib.EngineTypes.startp = _startpos__1_;
                  MenhirLib.EngineTypes.endp = _endpos__1_;
                  MenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let _4 : (
# 65 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.sort)
# 1802 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : unit = Obj.magic _3 in
        let _2 : (
# 37 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 1808 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_typed_arg = 
# 98 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                                                ( (_4, Var _2) )
# 1817 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
    |]
  
  and trace =
    None
  
end

module MenhirInterpreter = struct
  
  module ET = MenhirLib.TableInterpreter.MakeEngineTable (Tables)
  
  module TI = MenhirLib.Engine.Make (ET)
  
  include TI
  
end

let dynamics =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 70 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_chart)
# 1848 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
    ))

module Incremental = struct
  
  let dynamics =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 70 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_chart)
# 1858 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 1866 "/home/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
