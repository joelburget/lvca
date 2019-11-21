
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20181113

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TERMINAL_ID of (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 16 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | STRING of (
# 5 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 21 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | STAR
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_FIXITY
    | RIGHT_BRACE
    | REGEX of (
# 2 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | QUESTION
    | PLUS
    | NONTERMINAL_ID of (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | NAT of (
# 1 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 43 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | LEFT_PAREN
    | LEFT_FIXITY
    | LEFT_BRACE
    | GREATER
    | EOF
    | DOT
    | DOLLAR
    | CHARACTER_SET of (
# 6 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 55 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | CHARACTER_CLASS of (
# 7 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | BAR
    | ASSIGN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 25 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
   open Types.ConcreteSyntaxDescription 
# 74 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | ASSIGN ->
          23
      | BAR ->
          22
      | CHARACTER_CLASS _ ->
          21
      | CHARACTER_SET _ ->
          20
      | DOLLAR ->
          19
      | DOT ->
          18
      | EOF ->
          17
      | GREATER ->
          16
      | LEFT_BRACE ->
          15
      | LEFT_FIXITY ->
          14
      | LEFT_PAREN ->
          13
      | NAT _ ->
          12
      | NONTERMINAL_ID _ ->
          11
      | PLUS ->
          10
      | QUESTION ->
          9
      | REGEX _ ->
          8
      | RIGHT_BRACE ->
          7
      | RIGHT_FIXITY ->
          6
      | RIGHT_PAREN ->
          5
      | SEMICOLON ->
          4
      | STAR ->
          3
      | STRING _ ->
          2
      | TERMINAL_ID _ ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | ASSIGN ->
          Obj.repr ()
      | BAR ->
          Obj.repr ()
      | CHARACTER_CLASS _v ->
          Obj.repr _v
      | CHARACTER_SET _v ->
          Obj.repr _v
      | DOLLAR ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | GREATER ->
          Obj.repr ()
      | LEFT_BRACE ->
          Obj.repr ()
      | LEFT_FIXITY ->
          Obj.repr ()
      | LEFT_PAREN ->
          Obj.repr ()
      | NAT _v ->
          Obj.repr _v
      | NONTERMINAL_ID _v ->
          Obj.repr _v
      | PLUS ->
          Obj.repr ()
      | QUESTION ->
          Obj.repr ()
      | REGEX _v ->
          Obj.repr _v
      | RIGHT_BRACE ->
          Obj.repr ()
      | RIGHT_FIXITY ->
          Obj.repr ()
      | RIGHT_PAREN ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
      | STAR ->
          Obj.repr ()
      | STRING _v ->
          Obj.repr _v
      | TERMINAL_ID _v ->
          Obj.repr _v
  
  and default_reduction =
    (8, "\000\000\007\001\000\000\000('\000\018\000\000\000\029\000\019\020$\000\000\025\000\014\000\000\000\000\000\007\000\000#&\000\000!\012\000\027\000\000\t\b\021\031\026\000\024\000\016\000\n\002\000\019\020\003\000\004\000\022\000\005\000%\000\006\000)")
  
  and error =
    (24, "\000\000\016\000\b\000\000\000\000\000\000\000@\000\000\000\000\001 \128\000\000\000\000\000\000\000@\016\000\000\000\000\000\016\000\000\000\001@\016\002\000\000\000@\016\000\000\000\000\000\000\000\000\000\000\000\016\194@\016\000\000\000\000@\017\000\000\000\000\000\001\000\000\016\016\000\004\000\004\000\016\000\b\000\000\000\000\012\000\000\000\000\016\000\000\000\000\000\000\012\000 \000\000\016\000\000\000\000\000\000\004\000\000\000\000\000\001\000\000\002\018\194\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\016\000\000\000\000\000\016@\000\000\000\000\000@\000\000\000\000\000\000@\016\000\000\000\000\000\000\000\000\000\000@\016\000\000\000\000\000\000@\000\000\000\000\016\000\000\000\000\000\000@\000\000\000@\000\000\000\000\000\000\000@\000\000\000")
  
  and start =
    6
  
  and action =
    ((8, "#\022\000\000 \016\014\000\000 \000\"\018\004\000\006\000\000\000\021\006\000\006\000(\0200\"4\000*\020\000\000\030\020\000\000D\000D\r\000\000\000\000\000\006\000\"\0002\000\000:\000\000\000\006\0004\000\"\0006\000 \0008\000"), (16, "\000\170\000Y\000\006\000m\000B\000u\000R\000Y\000\174\000\030\000u\000u\000\190\000m\000F\000\"\000u\000\022\0001\000}\000}\000j\000)\000\011\000:\000~\000\133\000A\0002\000r\000\223\000\026\0006\000\142\0009\000f\000r\000n\000v\000\158\000\227\000\166\000\211\000\247\001\007\001\023"))
  
  and lhs =
    (8, "\005\004\003\002\001\000\026\025\025\024\023\023\022\022\021\021\020\020\019\019\018\017\016\016\016\015\015\014\014\r\r\012\012\011\011\n\t\b\007\007\006")
  
  and goto =
    ((8, "1\000\000\000\003\000\000\000\000\r\000 \000\011\000\023\000\000\000\000\r\000\028\000\000\024\000\017\000\000\000\018\000\000\000(\000\000\000\000\0004\000\000\000\000\000\012\000>\000\000\000\000*\000\000\000\024\000\000\000B\000\000\000L\000\000\000"), (8, "\031\n\004&\"\019\n\020\023\016\022\025\020\023\011'\025\031#\012!\"16\020\0232)\025<=\023%\023\025#\0244/-:2@ADE#\000\000\000\000.3"))
  
  and semantic_action =
    [|
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
        let _2 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 221 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.capture_number)
# 230 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 58 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                           ( _2 )
# 234 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _v : 'tv_fixity = 
# 84 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixl )
# 259 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _v : 'tv_fixity = 
# 85 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixr )
# 284 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _2 : 'tv_nonempty_list_sort_rule_ = Obj.magic _2 in
        let _1 : 'tv_nonempty_list_terminal_rule_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 43 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.t)
# 323 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 48 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( Types.ConcreteSyntaxDescription.make _1 _2 )
# 327 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_term_scope_pattern__ = 
# 142 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 345 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let x : 'tv_separated_nonempty_list_SEMICOLON_term_scope_pattern_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_term_scope_pattern__ = 
# 144 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 370 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.nonterminal_token)
# 391 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 221 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 399 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let xs : 'tv_nonempty_list_nonterminal_token_ = Obj.magic xs in
        let x : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.nonterminal_token)
# 427 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 223 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 435 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.sort_rule)
# 456 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_sort_rule_ = 
# 221 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 464 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let xs : 'tv_nonempty_list_sort_rule_ = Obj.magic xs in
        let x : (
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.sort_rule)
# 492 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_sort_rule_ = 
# 223 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 500 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 521 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 221 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 529 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let xs : 'tv_nonempty_list_terminal_rule_ = Obj.magic xs in
        let x : (
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 557 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 223 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 565 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 586 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.nonterminal_token)
# 594 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 111 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                   ( TerminalName    _1 )
# 598 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 619 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.nonterminal_token)
# 627 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 112 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                   ( NonterminalName _1 )
# 631 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _5 : 'tv_option_fixity_ = Obj.magic _5 in
        let _4 : unit = Obj.magic _4 in
        let _3 : (
# 42 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match_pattern)
# 678 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_nonempty_list_nonterminal_token_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 688 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 90 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let fixity = (match _5 with
    | None   -> Nofix
    | Some f -> f
    )
    in OperatorMatch { tokens = _1; operator_match_pattern = _3; fixity } )
# 696 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 724 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 732 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 108 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                         ( _1 )
# 736 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 757 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match list list)
# 765 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 74 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [[ _1 ]] )
# 769 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match list list)
# 802 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 808 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match list list)
# 816 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 76 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( match _3 with
      | []      -> [[ _1 ]]
      | x :: xs -> (_1 :: x) :: xs
    )
# 823 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match list list)
# 856 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 862 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match list list)
# 870 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 81 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [ _1 ] :: _3 )
# 874 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.capture_number)
# 895 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 42 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match_pattern)
# 903 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 99 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( ParenthesizingPattern _1 )
# 907 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let xs : 'tv_loption_separated_nonempty_list_SEMICOLON_term_scope_pattern__ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 949 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 42 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match_pattern)
# 957 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = let _3 = 
# 232 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 961 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
        
# 102 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( OperatorPattern (_1, _3) )
# 966 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 114 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( None )
# 984 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 116 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( Some x )
# 1009 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _v : 'tv_option_fixity_ = 
# 114 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( None )
# 1027 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let x : 'tv_fixity = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_fixity_ = 
# 116 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( Some x )
# 1052 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.capture_number)
# 1073 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_capture_number_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1081 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_DOT_capture_number_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.capture_number)
# 1116 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_capture_number_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1124 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let x : 'tv_term_scope_pattern = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_term_scope_pattern_ = 
# 241 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1149 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let xs : 'tv_separated_nonempty_list_SEMICOLON_term_scope_pattern_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_term_scope_pattern = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_term_scope_pattern_ = 
# 243 "/Users/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1188 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match list list)
# 1227 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : 'tv_option_BAR_ = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1234 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.sort_rule)
# 1242 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 64 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let (operator_rules, variable) = partition_nonterminal_matches(_4) in
    SortRule { sort_name = _1; operator_rules; variable }
  )
# 1248 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.sort_rule)
# 1276 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 41 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.sort_rule)
# 1284 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                               ( _1 )
# 1288 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _1 : 'tv_separated_nonempty_list_DOT_capture_number_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_term_scope_pattern = 
# 106 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let (binds, body) = Util.unsnoc _1 in NumberedScopePattern (binds, body) )
# 1313 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 2 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1346 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1352 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 1360 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 52 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Left _3) )
# 1364 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 5 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1397 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1403 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 1411 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 54 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Right _3) )
# 1415 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 1443 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 1451 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                       ( _1 )
# 1455 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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

let terminal_rule__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 66 lexer lexbuf) : (
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 1486 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and sort_rule__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 62 lexer lexbuf) : (
# 41 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.sort_rule)
# 1494 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and operator_match__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 58 lexer lexbuf) : (
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 1502 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_token =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 54 lexer lexbuf) : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.nonterminal_token)
# 1510 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and language =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 4 lexer lexbuf) : (
# 43 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.t)
# 1518 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and capture_number =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.capture_number)
# 1526 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

module Incremental = struct
  
  let terminal_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 66 initial_position) : (
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.pre_terminal_rule)
# 1536 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and sort_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 62 initial_position) : (
# 41 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.sort_rule)
# 1544 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and operator_match__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 58 initial_position) : (
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.operator_match)
# 1552 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_token =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 54 initial_position) : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.nonterminal_token)
# 1560 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and language =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 4 initial_position) : (
# 43 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.t)
# 1568 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and capture_number =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Types.ConcreteSyntaxDescription.capture_number)
# 1576 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "/Users/joel/.opam/default/lib/menhir/standard.mly"
  

# 1584 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
