
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20181113

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | STAR
    | RIGHT_PAREN
    | QUESTION
    | PLUS
    | LEFT_PAREN
    | ESCAPED of (
# 4 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
  )
    | EOF
    | DOT
    | CHARS of (
# 1 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 28 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
  )
    | CHARACTER_SET of (
# 2 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 33 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
  )
    | CHARACTER_CLASS of (
# 3 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 38 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
  )
    | BAR
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | BAR ->
          12
      | CHARACTER_CLASS _ ->
          11
      | CHARACTER_SET _ ->
          10
      | CHARS _ ->
          9
      | DOT ->
          8
      | EOF ->
          7
      | ESCAPED _ ->
          6
      | LEFT_PAREN ->
          5
      | PLUS ->
          4
      | QUESTION ->
          3
      | RIGHT_PAREN ->
          2
      | STAR ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | BAR ->
          Obj.repr ()
      | CHARACTER_CLASS _v ->
          Obj.repr _v
      | CHARACTER_SET _v ->
          Obj.repr _v
      | CHARS _v ->
          Obj.repr _v
      | DOT ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | ESCAPED _v ->
          Obj.repr _v
      | LEFT_PAREN ->
          Obj.repr ()
      | PLUS ->
          Obj.repr ()
      | QUESTION ->
          Obj.repr ()
      | RIGHT_PAREN ->
          Obj.repr ()
      | STAR ->
          Obj.repr ()
  
  and default_reduction =
    (8, "\000\000\011\016\t\b\018\n\000\012\014\r\000\004\006\000\017\000\000\007\001\000\000\002\000\020\000")
  
  and error =
    (13, "\006\2407\128\000\000\000\000\000\000\000\000\000\127\248\000\000\000\000\002\127\128\000\000\004\001\000\0007\136B\000\000\000\000\004\027\192\000\001\000\000\000B")
  
  and start =
    2
  
  and action =
    ((8, "\020\020\000\000\000\000\000\000\003\000\000\000\020\000\000\024\000\020*\000\000\002\020\000&\000$"), (8, "&9*.99999999\tFB\006\n\t\014\018\022\026\t\017FIg\000\017\000F\000\000F"))
  
  and lhs =
    (4, "\016\153\136vffffT2")
  
  and goto =
    ((8, "\n\022\000\000\000\000\000\000\000\000\000\000.\000\000\000\000\"\000\000\000\000\005\000\000\000\000"), (8, "\024\025\b\t\r\015\027\020\021\b\t\r\015\022\020\b\t\r\015\016\020\b\t\r\015\019\020\b\t\r\000\000\014"))
  
  and semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_prec2_re = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_prec2_re_ = 
# 221 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 148 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let xs : 'tv_nonempty_list_prec2_re_ = Obj.magic xs in
        let x : 'tv_prec2_re = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_prec2_re_ = 
# 223 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 180 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 213 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 219 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 227 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 35 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                          ( ReChoice (_1, _3) )
# 231 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let _1 : 'tv_prec1_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 256 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 36 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
             ( _1 )
# 260 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let _1 : 'tv_nonempty_list_prec2_re_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec1_re = 
# 39 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                            ( match _1 with
    | [ re ] -> re
    | res -> ReConcat res
  )
# 288 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 2 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 309 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 45 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                  ( ReSet _1 )
# 317 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 1 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 338 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 46 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
          ( ReString _1 )
# 346 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 16 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.re_class)
# 367 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 47 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
             ( ReClass _1 )
# 375 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 4 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 396 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 49 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
            ( ReString (String.sub _1 1 1) )
# 404 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let _1 : 'tv_prec3_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_prec2_re = 
# 50 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                  ( ReStar _1 )
# 436 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let _1 : 'tv_prec3_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_prec2_re = 
# 51 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                  ( RePlus _1 )
# 468 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let _1 : 'tv_prec3_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_prec2_re = 
# 52 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                      ( ReOption _1 )
# 500 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let _1 : 'tv_prec3_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 53 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
             ( _1 )
# 525 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
        let _v : 'tv_prec2_re = 
# 54 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
        ( ReAny )
# 550 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 584 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_prec3_re = 
# 56 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                                          ( _2 )
# 593 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 3 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 614 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 16 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.re_class)
# 622 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 22 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                          ( match _1 with
  | {|\w|} -> PosClass Word
  | {|\s|} -> PosClass Whitespace
  | {|\d|} -> PosClass Digit
  | {|\b|} -> PosClass Boundary
  | {|\W|} -> NegClass Word
  | {|\S|} -> NegClass Whitespace
  | {|\D|} -> NegClass Digit
  | {|\B|} -> NegClass Boundary
  | _ -> failwith "unexpected character class"
  )
# 636 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 657 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 17 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 665 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 58 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                ( _1 )
# 669 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 17 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 697 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 705 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 60 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                       ( _1 )
# 709 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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

let regex__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 22 lexer lexbuf) : (
# 18 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 740 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
    ))

and regex =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 17 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 748 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
    ))

module Incremental = struct
  
  let regex__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 22 initial_position) : (
# 18 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 758 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and regex =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 17 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 766 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 774 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
