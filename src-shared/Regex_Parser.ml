
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20200211

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | STAR
    | RIGHT_PAREN
    | QUESTION
    | PLUS
    | LEFT_PAREN
    | ESCAPED of (
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 21 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
  )
    | EOF
    | DOT
    | CHARS of (
# 1 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 28 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
  )
    | CHARACTER_SET of (
# 2 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 33 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
  )
    | CHARACTER_CLASS of (
# 3 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 38 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
  )
    | BAR
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 14 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
  
let rec mk_choices : Regex.t list -> Regex.t
  = function
    | [] -> failwith "invariant violation: mk_choices called with empty list"
    | [ re ] -> re
    | re :: res -> ReChoice (re, mk_choices res)

# 57 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"

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
    (8, "\000\000\t\014\007\006\016\004\b\r\000\n\012\011\003\000\000\019\005\000\015\001\000\017")
  
  and error =
    (13, "\006\2407\128\000\000\000\000\000\000\000\000\000\000\000\000\031\254\000\000\000\000\000\000\004!\006\240\000\000\000@\000\000\000\000\004\000\000")
  
  and start =
    1
  
  and action =
    ((8, "\003\003\000\000\000\000\000\000\000\000\003\000\000\000\000\020\003\000\000\022\000\000\014\000"), (8, ".\00526\006\n\005\014\018\022\026\005ER_\000\000E\000\000\000\000B"))
  
  and lhs =
    (4, "\b\135eUUUUC!\016")
  
  and goto =
    ((8, "\003\014\000\000\000\000\000\000\000\000*\000\000\000\000\000\030\000\000\000\000\000\000\000"), (8, "\b\022\t\n\011\016\023\019\b\000\t\n\011\016\020\019\018\000\t\n\011\016\000\019\t\n\011\000\000\015"))
  
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
# 221 "<standard.mly>"
    ( [ x ] )
# 158 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 223 "<standard.mly>"
    ( x :: xs )
# 190 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
        let _1 : 'tv_separated_nonempty_list_BAR_prec1_re_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 25 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.t)
# 215 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = 
# 40 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                                                 ( mk_choices _1 )
# 219 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 43 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                            ( match _1 with
    | [ re ] -> re
    | res -> Regex.ReConcat res
  )
# 247 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 2 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 268 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 49 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                  ( ReSet _1 )
# 276 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 1 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 297 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 50 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
          ( ReString _1 )
# 305 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 23 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.re_class)
# 326 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 51 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
             ( ReClass _1 )
# 334 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 355 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_prec2_re = 
# 53 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
            ( ReString (String.sub _1 1 1) )
# 363 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
        let _1 : 'tv_prec2_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_prec2_re = 
# 54 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                  ( ReStar _1 )
# 395 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
        let _1 : 'tv_prec2_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_prec2_re = 
# 55 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                  ( RePlus _1 )
# 427 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
        let _1 : 'tv_prec2_re = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_prec2_re = 
# 56 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                      ( ReOption _1 )
# 459 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
             ( _1 )
# 484 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 58 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
        ( ReAny )
# 509 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 25 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.t)
# 543 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_prec3_re = 
# 60 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                                          ( _2 )
# 552 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 3 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
       (string)
# 573 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 23 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.re_class)
# 581 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = 
# 28 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
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
# 595 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
# 25 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.t)
# 623 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 24 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.t)
# 631 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
        ) = 
# 62 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
                    ( _1 )
# 635 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
        let x : 'tv_prec1_re = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_BAR_prec1_re_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 660 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_BAR_prec1_re_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_prec1_re = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_BAR_prec1_re_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 699 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
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

let regex =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 24 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.t)
# 730 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
    ))

module Incremental = struct
  
  let regex =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 24 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.mly"
      (Regex.t)
# 740 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 748 "/Users/joel/code/lvca-bucklescript/src-shared/Regex_Parser.ml"
