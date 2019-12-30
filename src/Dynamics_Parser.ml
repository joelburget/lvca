
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20190924

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | STRING of (
# 23 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 16 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_BRACKET
    | LEFT_PAREN
    | LEFT_BRACKET
    | INT of (
# 22 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (Bigint.t)
# 26 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | ID of (
# 24 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 31 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | EOF
    | DOT
    | COMMA
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 3 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  
open Core

let ast_to_sort : Binding.Nominal.term -> Types.sort
  = failwith "TODO"

let ast_to_core_scope : Binding.Nominal.term -> core_scope
  = failwith "TODO"
(*
  | separated_llist(DOT, pattern) DOT core
  { Scope ($1, $3) }
  | core
  { Scope ([], $1) }
  *)

let ast_to_pattern : Binding.Nominal.term -> Pattern.t
  = failwith "TODO"

# 63 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | COMMA ->
          11
      | DOT ->
          10
      | EOF ->
          9
      | ID _ ->
          8
      | INT _ ->
          7
      | LEFT_BRACKET ->
          6
      | LEFT_PAREN ->
          5
      | RIGHT_BRACKET ->
          4
      | RIGHT_PAREN ->
          3
      | SEMICOLON ->
          2
      | STRING _ ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | COMMA ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | ID _v ->
          Obj.repr _v
      | INT _v ->
          Obj.repr _v
      | LEFT_BRACKET ->
          Obj.repr ()
      | LEFT_PAREN ->
          Obj.repr ()
      | RIGHT_BRACKET ->
          Obj.repr ()
      | RIGHT_PAREN ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
      | STRING _v ->
          Obj.repr _v
  
  and default_reduction =
    (8, "\000\027\000\026\000\000\000\000\020\018\004\000\000\031\025\000\002\000\000\005\000\000!\000\022\000\007\000\000#\011\016\n\000\024\000\t\000\000\029\000\012\001\000\014")
  
  and error =
    (12, "C\192\000K\144\000O\213\0244%\026\000\000\000\000\000\002A\128\000\000\001\000\000\000\002A\128\0000\004\026\000\003\002\000\001\000\000\003\000A\128\000\000\000\000\000\000\129\000\000\128\000\000\129C\128\000\000@\000\000\004<\000\000")
  
  and start =
    1
  
  and action =
    ((8, "\016\000\016\000\0032@\"\000\000\000\002\"\000\000\020\000\026\"\000\003\"\000&\000H\0008\"\000\000\000\000@\000H\000D\016\000@\000\000\016\000"), (8, "\029V}\029\022\029\029\029\029\006\02929B\n\014\0181\0069I\021\021J\014\026\006AQar\133\014\026\t\tY\030mj\146\167\t\138\000\154"))
  
  and lhs =
    (8, "\000\017\017\017\016\016\015\015\015\015\014\r\012\012\011\011\n\n\t\t\b\b\007\007\006\005\005\004\004\003\003\002\002\001\001")
  
  and goto =
    ((8, "\028\0008\000\000\"\000\005\000\000\000\000@\000\000\000\000\0002\000\000\014\000\000\000\000\000\000N\000\000\000\000\011\000\000\000\000\002\000\000\000\000H\000"), (8, "\t\n$\011\012(!\016\018\023\n\000\011\012\021\024&\018\025!\000\000\011\021\024\026)+\000,\011\028 !\031\014\000\011\012\"\030!\020&\011\000\000\000-\015\000,\000\028\000\000\031"))
  
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
# 24 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 177 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 185 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 189 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 61 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Operator (_1, _3) )
# 194 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 24 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 215 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 223 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 62 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       ( Var _1 )
# 227 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 37 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.primitive)
# 248 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 256 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 63 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
              ( Primitive _1 )
# 260 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 293 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let xs : 'tv_loption_separated_nonempty_list_DOT_pattern__ = Obj.magic xs in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 42 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 303 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _1 = 
# 232 "<standard.mly>"
    ( xs )
# 307 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 67 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Scope (_1, _3) )
# 312 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 333 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 42 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 341 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 69 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Scope ([], _1) )
# 345 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_loption_separated_nonempty_list_SEMICOLON_core_scope__ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 24 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 387 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 395 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 399 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 47 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Operator (_1, _3) )
# 404 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 24 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 425 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 433 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 49 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Var _1 )
# 437 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_loption_separated_nonempty_list_COMMA_core__ = Obj.magic xs in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 483 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _2 = 
# 232 "<standard.mly>"
    ( xs )
# 487 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 51 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Sequence _2 )
# 492 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 37 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.primitive)
# 513 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 521 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 52 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
              ( Primitive _1 )
# 525 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 546 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 36 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core_scope)
# 554 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 56 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                     ( ast_to_core_scope _1 )
# 558 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _1 : 'tv_list_core_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 39 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core list)
# 590 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 75 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                         ( _1 )
# 594 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _v : 'tv_list_core_ = 
# 211 "<standard.mly>"
    ( [] )
# 612 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_list_core_ = Obj.magic xs in
        let x : (
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 640 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_core_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 648 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _v : 'tv_loption_separated_nonempty_list_COMMA_core__ = 
# 142 "<standard.mly>"
    ( [] )
# 666 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let x : 'tv_separated_nonempty_list_COMMA_core_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_core__ = 
# 144 "<standard.mly>"
    ( x )
# 691 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 709 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 734 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 752 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 777 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_scope__ = 
# 142 "<standard.mly>"
    ( [] )
# 795 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let x : 'tv_separated_nonempty_list_SEMICOLON_core_scope_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_core_scope__ = 
# 144 "<standard.mly>"
    ( x )
# 820 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 838 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 863 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.term)
# 884 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 40 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Pattern.t)
# 892 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 57 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                     ( ast_to_pattern    _1 )
# 896 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 22 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (Bigint.t)
# 917 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 37 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.primitive)
# 925 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 72 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
           ( PrimInteger _1 )
# 929 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 23 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 950 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 37 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Types.primitive)
# 958 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 73 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
           ( PrimString  _1 )
# 962 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 983 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_core_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 991 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_COMMA_core_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core)
# 1026 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_core_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1034 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 40 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Pattern.t)
# 1055 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_pattern_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1063 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 40 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Pattern.t)
# 1098 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_pattern_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1106 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 42 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 1127 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_ast_like_scope_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1135 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 42 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Binding.Nominal.scope)
# 1170 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_ast_like_scope_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1178 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 36 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core_scope)
# 1199 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_scope_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1207 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_SEMICOLON_core_scope_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 36 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core_scope)
# 1242 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_core_scope_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1250 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 39 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core list)
# 1281 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
    ))

module Incremental = struct
  
  let dynamics =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 39 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.core list)
# 1291 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 1299 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
