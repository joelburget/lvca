
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
    | DOLLAR
    | CHARS of (
# 1 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 29 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
  )
    | CHARACTER_SET of (
# 2 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 34 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
  )
    | CHARACTER_CLASS of (
# 3 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
       (string)
# 39 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
          13
      | CHARACTER_CLASS _ ->
          12
      | CHARACTER_SET _ ->
          11
      | CHARS _ ->
          10
      | DOLLAR ->
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
      | DOLLAR ->
          Obj.repr ()
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
    (8, "\000\000\012\017\n\t\019\011\000\r\015\014\000\005\007\000\018\000\000\b\000\000\002\000\000\003\000\021\000")
  
  and error =
    (14, "\006\184\026\224\000\000\000\000\000\000\000\000\000\000\127\188\000\000\000\000\000'\188\000\000\000\b\001\000\000\026\226\016@\000\000\004\026\224\000\000\001\006\184\000\000\016\000\000\001\004")
  
  and start =
    3
  
  and action =
    ((8, "\022\022\000\000\000\000\000\000\003\000\000\000\022\000\000\024\000\022\026\000\011\022\000\011\022\000*\000("), (8, "&=*.====F====\rB\021\006\n\r\014\021\018\022\026\rFFMo\000\000\000\000F"))
  
  and lhs =
    (4, "!\n\169\152wwwwvT0")
  
  and goto =
    ((8, "\020 \000\000\000\000\000\000\000\000\000\0008\000\000\000\000,\000\000\000\b\000\000\007\000\000\000\000"), (8, "\026\027\b\t\r\015\029\020\023\b\t\r\015\024\020\b\t\r\015\021\020\b\t\r\015\016\020\b\t\r\015\019\020\b\t\r\000\000\014"))
  
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
# 153 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 185 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 218 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 224 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 232 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 37 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                          ( ReChoice (_1, _3) )
# 236 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 261 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 38 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
             ( _1 )
# 265 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 41 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                            ( match _1 with
    | [ re ] -> re
    | res -> ReConcat res
  )
# 293 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 314 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 47 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                  ( ReSet _1 )
# 322 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 343 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 48 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
          ( ReString _1 )
# 351 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 18 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.re_class)
# 372 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 49 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
             ( ReClass _1 )
# 380 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 401 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 51 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
            ( ReString (String.sub _1 1 1) )
# 409 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
                  ( ReStar _1 )
# 441 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 53 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                  ( RePlus _1 )
# 473 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 54 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                      ( ReOption _1 )
# 505 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 55 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
             ( _1 )
# 530 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 56 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
        ( ReAny )
# 555 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 589 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_prec3_re = 
# 58 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                                          ( _2 )
# 598 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 619 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 18 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.re_class)
# 627 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 24 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
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
# 641 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 662 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 670 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 60 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                ( _1 )
# 674 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 702 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 20 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 710 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
        ) = 
# 62 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
                       ( _1 )
# 714 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
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
    (Obj.magic (MenhirInterpreter.entry 24 lexer lexbuf) : (
# 20 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 745 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
    ))

and regex =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 21 lexer lexbuf) : (
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 753 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
    ))

and prec0_re =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 761 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
    ))

module Incremental = struct
  
  let regex__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 24 initial_position) : (
# 20 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 771 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and regex =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 21 initial_position) : (
# 19 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 779 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and prec0_re =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 21 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.mly"
      (Regex.t)
# 787 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 795 "/Users/joel/code/lvca-bucklescript/src/Regex_Parser.ml"
