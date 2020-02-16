
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20200211

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_D_ARR
    | LINE of (
# 18 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
       (string option)
# 19 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
  )
    | LEFT_PAREN
    | LEFT_D_ARR
    | ID of (
# 7 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
       (string)
# 26 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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

# 21 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
  
open Tablecloth
open Statics

(* raises unnamed exception *)
let rec term_to_pattern : Statics.term -> Pattern.t
  = function
    | Operator (name, args)
    -> Operator (name, List.map args ~f:scope_to_pattern)
    | Free var -> Var var
    | _ -> failwith
      "bad parse -- can only match operators and variables in a pattern"

(* raises unnamed exception *)
and scope_to_pattern = function
  | Scope ([], body) -> term_to_pattern body
  | _ -> failwith "bad parse -- can't match binders in a pattern"

# 61 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"

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
    (8, "\000\000\000\000\000\000\000\000\000\020\r\016\000\000\022\000\023\026\000\000\018\005\001\000\011\000\000\014\000\000\006\000\000\007\000\003\027\028\000\t\000\015\000\002\000\025")
  
  and error =
    (14, "\b\144\000\160\016\000\001\001\001\251\226\016\024\016\001\000\000\000\000\000\000`\000\004\000\000\b\000\000\000\000\000\002\128@\000\000\000\000\000\002$\000\000 \000\001\000\000\000 \004\000\000\004\128\001\000\000\000\016\000\000\000\000\000\000\129\000\000\000\128\000\000\016\000\000\000\128\000\000")
  
  and start =
    2
  
  and action =
    ((8, "\016\n\003\028 \003 \024 \000\000\000. \0004\000\000\020\003\000\000\000\016\0004\016\000* \000. \000 \000\000\000\016\0000\000 \0002\000"), (8, "]]]]\026]\014]]]]]\029II\r%\n-\006A\"N\0226Q\130\018B\138jv\167\183"))
  
  and lhs =
    (8, "\001\000\018\017\017\016\015\014\014\r\r\012\012\011\n\t\b\b\007\007\006\006\005\005\004\003\002\002")
  
  and goto =
    ((8, "\021\0000\000\007\000\006\000.\000\000\000\000\026\000\000\000\000\0008\000\000\000\000\000\000,\000\000&\000\000\000\0000\000\000\000\018\000\000\000H\000\000\000"), (8, "\023\024\018)\026\"'\029\b\011\012\024\r\025\026\016'\029\b\015\012\031\r( '\029\019\b$\n\019\022\000%\000\021&\028\029,-"))
  
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
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 174 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 180 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 46 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.checking_rule)
# 188 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 69 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                                      ( {tm = _1; ty = _3} )
# 192 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 79 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
  ( StrDict.empty )
# 217 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 81 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
  ( StrDict.from_list _3 )
# 256 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 47 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.typing_clause)
# 289 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic clause in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_context = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_clause_ in
        let _v : (
# 48 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.hypothesis)
# 299 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 83 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                                                         ( (StrDict.empty, clause) )
# 303 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 336 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 342 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 45 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.inference_rule)
# 350 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 68 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                                      ( {tm = _1; ty = _3} )
# 354 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 211 "<standard.mly>"
    ( [] )
# 372 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 48 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.hypothesis)
# 400 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_hypothesis_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 408 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 211 "<standard.mly>"
    ( [] )
# 426 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 49 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.rule)
# 454 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_rule_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 462 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 142 "<standard.mly>"
    ( [] )
# 480 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 144 "<standard.mly>"
    ( x )
# 505 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 48 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.hypothesis)
# 538 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic conclusion in
        let _2 : (
# 18 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
       (string option)
# 543 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _2 in
        let hypotheses : 'tv_list_hypothesis_ = Obj.magic hypotheses in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_hypotheses_ in
        let _endpos = _endpos_conclusion_ in
        let _v : (
# 49 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.rule)
# 552 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 87 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
  ( { hypotheses; name = _2; conclusion } )
# 556 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 50 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.rule list)
# 588 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 89 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                              ( rules )
# 592 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
        let _1 : 'tv_separated_nonempty_list_DOT_term_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 44 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.scope)
# 617 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
  ( let binders_tm, body = Util.unsnoc _1 in
    let binders_pat = List.map binders_tm ~f:term_to_pattern in
    Scope (binders_pat, body)
  )
# 624 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 241 "<standard.mly>"
    ( [ x ] )
# 649 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 243 "<standard.mly>"
    ( x :: xs )
# 688 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 709 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_term_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 717 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_DOT_term_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 752 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_term_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 760 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 44 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.scope)
# 781 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 789 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 44 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.scope)
# 824 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_scope_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 832 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 7 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
       (string)
# 874 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 882 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 886 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
         in
        
# 55 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
  ( Operator (_1, _3) )
# 891 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 7 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
       (string)
# 912 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 920 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
  ( Free _1 )
# 924 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 952 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 43 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 960 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 66 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                   ( _1 )
# 964 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 42 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 997 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 7 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
       (string)
# 1003 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_typed_term = 
# 75 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                          ( _1, _3 )
# 1011 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 45 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.inference_rule)
# 1032 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 47 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.typing_clause)
# 1040 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 72 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                   ( InferenceRule _1 )
# 1044 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
# 46 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.checking_rule)
# 1065 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 47 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.typing_clause)
# 1073 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
        ) = 
# 73 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
                   ( CheckingRule  _1 )
# 1077 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
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
    (Obj.magic (MenhirInterpreter.entry 42 lexer lexbuf) : (
# 43 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 1108 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
    ))

and rules =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 50 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.rule list)
# 1116 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
    ))

module Incremental = struct
  
  let term_top =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 42 initial_position) : (
# 43 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.term)
# 1126 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and rules =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 50 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.mly"
      (Statics.rule list)
# 1134 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 1142 "/Users/joel/code/lvca-bucklescript/src-shared/Statics_Parser.ml"
