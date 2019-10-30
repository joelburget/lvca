
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20181113

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_D_ARR
    | LINE of (
# 12 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string option)
# 19 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
  )
    | LEFT_PAREN
    | LEFT_D_ARR
    | ID of (
# 1 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string)
# 26 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
  )
    | EOF
    | DOT
    | CTX_SEPARATOR
    | CTX
    | COMMA
    | COLON
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 15 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
  
open Statics

# 46 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | COLON ->
          13
      | COMMA ->
          12
      | CTX ->
          11
      | CTX_SEPARATOR ->
          10
      | DOT ->
          9
      | EOF ->
          8
      | ID _ ->
          7
      | LEFT_D_ARR ->
          6
      | LEFT_PAREN ->
          5
      | LINE _ ->
          4
      | RIGHT_D_ARR ->
          3
      | RIGHT_PAREN ->
          2
      | SEMICOLON ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | COLON ->
          Obj.repr ()
      | COMMA ->
          Obj.repr ()
      | CTX ->
          Obj.repr ()
      | CTX_SEPARATOR ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | ID _v ->
          Obj.repr _v
      | LEFT_D_ARR ->
          Obj.repr ()
      | LEFT_PAREN ->
          Obj.repr ()
      | LINE _v ->
          Obj.repr _v
      | RIGHT_D_ARR ->
          Obj.repr ()
      | RIGHT_PAREN ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
  
  and default_reduction =
    (8, "\000\000\000\000\000\000\000\000\000\018\020\r\000\000\024\000\000\000\019\000\025\028\000\000\022\005\001\000\011\000\000\016\000\000\006\000\000\007\000\003\029\030\000\t\000\017\000\002\000\027")
  
  and error =
    (14, "\b\144\000\160\016\000\001\001\001\250\226\020\025\016\001\000\000\000\000\000\000`\000\005\000\000\000\016\001\001\145\000\000\b\000\000\000\000\000\002\128@\000\000\000\000\000\002$\000\000 \000\001\000\000\000 \004\000\000\004\128\001\000\000\000\016\000\000\000\000\000\000\129\000\000\000\128\000\000\016\000\000\000\128\000\000")
  
  and start =
    2
  
  and action =
    ((8, "\016\022\003\002\022\003\026\003\022\000\000\000.\026\000(0\016\000<\000\000 \003\000\000\000\016\000:\016\0000\022\0000\022\000\022\000\000\000\016\0006\000\022\0008\000"), (8, "eeee\026e\014e5eee\029\026\018-%9\022\006\030\r\"\n6YQ\146^B\154FRz\134\183\199"))
  
  and lhs =
    (8, "\001\000\018\017\017\016\015\014\014\r\r\012\012\011\011\n\t\b\b\b\007\007\006\006\005\005\004\003\002\002")
  
  and goto =
    ((8, "\019\000\003\000\005\000\b\000\012\000\000\000\000\"\000\000\016\000\000\000\000\000\000.\000\000\000\b\000\000\"\000\000,\000\000\028\000B\000\000\000\030\000\000\000>\000\000\000"), (8, "\027\028\023\022-\030\026+!\011\012\n\r\019\028\016\020\029\030&+!\011\015#\r\023$\016,\025+! !01)(\000*"))
  
  and semantic_action =
    [|
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 159 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 165 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 25 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.checking_rule)
# 173 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 45 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                                      ( {tm = _1; ty = _3} )
# 177 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_context = 
# 55 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
  ( M.empty )
# 202 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let _3 : 'tv_separated_nonempty_list_COMMA_typed_term_ = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_context = 
# 57 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
  ( M.fromArray (Belt.List.toArray _3) )
# 241 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
          MenhirLib.EngineTypes.semv = clause;
          MenhirLib.EngineTypes.startp = _startpos_clause_;
          MenhirLib.EngineTypes.endp = _endpos_clause_;
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
        let clause : (
# 26 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.typing_clause)
# 274 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic clause in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_context = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_clause_ in
        let _v : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.hypothesis)
# 284 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 59 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                                                         ( (M.empty, clause) )
# 288 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 321 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 327 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 24 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.inference_rule)
# 335 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 44 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                                      ( {tm = _1; ty = _3} )
# 339 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let _v : 'tv_list_hypothesis_ = 
# 211 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 357 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let xs : 'tv_list_hypothesis_ = Obj.magic xs in
        let x : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.hypothesis)
# 385 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_hypothesis_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 393 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let _v : 'tv_list_rule_ = 
# 211 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 411 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let xs : 'tv_list_rule_ = Obj.magic xs in
        let x : (
# 28 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.rule)
# 439 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_rule_ = 
# 213 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 447 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 465 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let x : 'tv_separated_nonempty_list_SEMICOLON_scope_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 490 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 1 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string)
# 511 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = 
# 5 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( [ x ] )
# 519 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 1 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string)
# 552 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic x in
        let _2 : unit = Obj.magic _2 in
        let xs : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = Obj.magic xs in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = 
# 7 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( x :: xs )
# 562 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
          MenhirLib.EngineTypes.semv = conclusion;
          MenhirLib.EngineTypes.startp = _startpos_conclusion_;
          MenhirLib.EngineTypes.endp = _endpos_conclusion_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _;
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = hypotheses;
              MenhirLib.EngineTypes.startp = _startpos_hypotheses_;
              MenhirLib.EngineTypes.endp = _endpos_hypotheses_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let conclusion : (
# 27 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.hypothesis)
# 595 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic conclusion in
        let _2 : (
# 12 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string option)
# 600 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _2 in
        let hypotheses : 'tv_list_hypothesis_ = Obj.magic hypotheses in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_hypotheses_ in
        let _endpos = _endpos_conclusion_ in
        let _v : (
# 28 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.rule)
# 609 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 63 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
  ( { hypotheses; name = _2; conclusion } )
# 613 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
            MenhirLib.EngineTypes.semv = rules;
            MenhirLib.EngineTypes.startp = _startpos_rules_;
            MenhirLib.EngineTypes.endp = _endpos_rules_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let rules : 'tv_list_rule_ = Obj.magic rules in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_rules_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.rule list)
# 645 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 65 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                              ( rules )
# 649 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 676 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__2_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 23 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.scope)
# 685 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = let _1 =
          let xs = 
# 10 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( [] )
# 690 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
           in
          
# 16 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( List.rev xs )
# 695 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
          
        in
        
# 39 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                                      ( Scope (_1, _3) )
# 701 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 734 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let xs : 'tv_reverse_separated_nonempty_llist_DOT_ID_ = Obj.magic xs in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 23 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.scope)
# 744 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = let _1 =
          let xs = 
# 12 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( xs )
# 749 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
           in
          
# 16 "/Users/joel/code/lvca-bucklescript/src/ParserLib.mly"
    ( List.rev xs )
# 754 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
          
        in
        
# 39 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                                      ( Scope (_1, _3) )
# 760 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 781 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 23 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.scope)
# 789 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 40 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                                      ( Scope ([], _1) )
# 793 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let x : 'tv_typed_term = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_typed_term_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 818 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_COMMA_typed_term_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_typed_term = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_typed_term_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 857 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 23 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.scope)
# 878 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 886 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_SEMICOLON_scope_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 23 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.scope)
# 921 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 929 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let xs : 'tv_loption_separated_nonempty_list_SEMICOLON_scope__ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string)
# 971 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 979 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 983 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
         in
        
# 34 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
  ( Operator (_1, _3) )
# 988 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 1 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string)
# 1009 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 1017 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 36 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
  ( Free _1 )
# 1021 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
        let _1 : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 1049 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 22 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 1057 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 42 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                   ( _1 )
# 1061 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 1094 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
       (string)
# 1100 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_typed_term = 
# 51 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                          ( _1, _3 )
# 1108 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 24 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.inference_rule)
# 1129 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 26 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.typing_clause)
# 1137 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 48 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                   ( InferenceRule _1 )
# 1141 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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
# 25 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.checking_rule)
# 1162 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 26 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.typing_clause)
# 1170 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
        ) = 
# 49 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
                   ( CheckingRule  _1 )
# 1174 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
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

let term_top =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 46 lexer lexbuf) : (
# 22 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 1205 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
    ))

and rules =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 29 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.rule list)
# 1213 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
    ))

module Incremental = struct
  
  let term_top =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 46 initial_position) : (
# 22 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.term)
# 1223 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and rules =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 29 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.mly"
      (Statics.rule list)
# 1231 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 1239 "/Users/joel/code/lvca-bucklescript/src/Statics_Parser.ml"
