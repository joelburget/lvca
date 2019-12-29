
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20190924

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | STRING of (
# 10 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 16 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_OXFORD
    | RIGHT_BRACKET
    | LEFT_PAREN
    | LEFT_OXFORD
    | LEFT_BRACKET
    | INT of (
# 9 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (Bigint.t)
# 28 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | ID of (
# 11 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 33 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
  )
    | EQ
    | EOF
    | DOT
    | COMMA
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 7 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
   open Core 
# 49 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | COMMA ->
          14
      | DOT ->
          13
      | EOF ->
          12
      | EQ ->
          11
      | ID _ ->
          10
      | INT _ ->
          9
      | LEFT_BRACKET ->
          8
      | LEFT_OXFORD ->
          7
      | LEFT_PAREN ->
          6
      | RIGHT_BRACKET ->
          5
      | RIGHT_OXFORD ->
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
      | EQ ->
          Obj.repr ()
      | ID _v ->
          Obj.repr _v
      | INT _v ->
          Obj.repr _v
      | LEFT_BRACKET ->
          Obj.repr ()
      | LEFT_OXFORD ->
          Obj.repr ()
      | LEFT_PAREN ->
          Obj.repr ()
      | RIGHT_BRACKET ->
          Obj.repr ()
      | RIGHT_OXFORD ->
          Obj.repr ()
      | RIGHT_PAREN ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
      | STRING _v ->
          Obj.repr _v
  
  and default_reduction =
    (8, "\000\000\000\000\000\000\004\018\000\000\000\005\000\002\000\000\028\006\000\000\000\022\000\000\n\000\021\t\016\b\000\020\000\007\000\000\026\012\000\011\000\014\001")
  
  and error =
    (15, "\001\b\000@\232\000\129# @\004\000\000\000\000\000\004\000@\200\016\000\001\000\000\000\012\000\000\018\000\000\000\000 \000\000\132\030\000\000\000\b\004\000\000\000\139\196\000\000\000\000\000\000\000\001\000\128\000\004\000\000\000\016\n\015\000\000\000\000\000\002\000\000\001\b\000\000\000\000")
  
  and start =
    1
  
  and action =
    ((8, "\022\028\003\024\003\028\000\000\026\"\016\0006\000\028\024\000\0006*\003\000.<\000\003\000\000\000\000\028\000<\000\030\003\000\0000\000\022\000\000"), (8, "V\t\t\t9\014Zfjn\t\tY9\014A>i\006Ia]\0181\n\022&*~\1426NR^b\134\159"))
  
  and lhs =
    (4, "\r\220\204\187\187\169\136wfUD3\"\017")
  
  and goto =
    ((8, "\020\025\000\003\000\021\000\000\000\t\000\000\000\000\000\006\000\000\000\000\006\000\000\000\000\012\000\000\000\000\"\000\000\000\000&\000\000\000\000 \000\000"), (8, "\b\019\t\007\017\r\t\030\029\012\030\015\018\031&\015\018#')+%!\030*)\000\000\000\000#"))
  
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
        let xs : 'tv_loption_separated_nonempty_list_SEMICOLON_denotation_pat_scope__ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 11 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 175 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 25 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t)
# 183 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 187 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 35 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Operator (_1, _3) )
# 192 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 11 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 213 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 25 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t)
# 221 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 37 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Var _1 )
# 225 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _3 : (
# 25 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t)
# 252 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__2_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 26 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.scope)
# 261 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _1 =
          let xs = 
# 10 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( [] )
# 266 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
           in
          
# 16 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( List.rev xs )
# 271 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
          
        in
        
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Scope (_1, _3) )
# 277 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 25 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t)
# 310 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let xs : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = Obj.magic xs in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 26 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.scope)
# 320 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _1 =
          let xs = 
# 12 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( xs )
# 325 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
           in
          
# 16 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( List.rev xs )
# 330 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
          
        in
        
# 41 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Scope (_1, _3) )
# 336 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 25 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t)
# 357 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 26 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.scope)
# 365 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 43 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Scope ([], _1) )
# 369 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_loption_separated_nonempty_list_COMMA_denotation_term__ = Obj.magic xs in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_term)
# 415 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = let _2 = 
# 232 "<standard.mly>"
    ( xs )
# 419 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
         in
        
# 61 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Sequence _2 )
# 424 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _1 : 'tv_prim = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_term)
# 449 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 63 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Primitive _1 )
# 453 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 11 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 474 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_term)
# 482 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 65 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Var _1 )
# 486 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 11 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 520 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_term)
# 529 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 67 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( Meaning _2 )
# 533 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _1 : 'tv_list_dynamics_rule_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 30 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.pre_denotation_chart)
# 565 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 85 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
                                  ( DenotationChart _1 )
# 569 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _5 : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_term)
# 614 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _5 in
        let _4 : unit = Obj.magic _4 in
        let _3 : unit = Obj.magic _3 in
        let _2 : (
# 25 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t)
# 621 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t * Core.denotation_term)
# 630 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = 
# 83 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
  ( (_2, _5) )
# 634 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _v : 'tv_list_dynamics_rule_ = 
# 211 "<standard.mly>"
    ( [] )
# 652 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_list_dynamics_rule_ = Obj.magic xs in
        let x : (
# 29 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.t * Core.denotation_term)
# 680 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_dynamics_rule_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 688 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _v : 'tv_loption_separated_nonempty_list_COMMA_denotation_term__ = 
# 142 "<standard.mly>"
    ( [] )
# 706 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let x : 'tv_separated_nonempty_list_COMMA_denotation_term_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_denotation_term__ = 
# 144 "<standard.mly>"
    ( x )
# 731 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_denotation_pat_scope__ = 
# 142 "<standard.mly>"
    ( [] )
# 749 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let x : 'tv_separated_nonempty_list_SEMICOLON_denotation_pat_scope_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_denotation_pat_scope__ = 
# 144 "<standard.mly>"
    ( x )
# 774 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 792 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 817 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 9 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (Bigint.t)
# 838 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prim = 
# 78 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
           ( PrimInteger _1 )
# 846 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 10 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 867 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prim = 
# 79 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
           ( PrimString  _1 )
# 875 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 11 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 896 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = 
# 5 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( [ x ] )
# 904 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
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
        let x : (
# 11 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
       (string)
# 937 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _2 : unit = Obj.magic _2 in
        let xs : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = Obj.magic xs in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = 
# 7 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( x :: xs )
# 947 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 27 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_term)
# 968 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_denotation_term_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 976 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_COMMA_denotation_term_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.denotation_term)
# 1011 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_denotation_term_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1019 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 26 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.scope)
# 1040 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_denotation_pat_scope_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1048 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_SEMICOLON_denotation_pat_scope_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 26 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (BindingAwarePattern.scope)
# 1083 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_denotation_pat_scope_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1091 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
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
# 30 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.pre_denotation_chart)
# 1122 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
    ))

module Incremental = struct
  
  let dynamics =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 30 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.mly"
      (Core.pre_denotation_chart)
# 1132 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 1140 "/Users/joel/code/lvca-bucklescript/src/Dynamics_Parser.ml"
