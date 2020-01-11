
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20190924

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNDERSCORE
    | TERMINAL_ID of (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 17 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | STRING of (
# 5 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 22 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_FIXITY
    | RIGHT_BRACE
    | REGEX of (
# 2 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | NONTERMINAL_ID of (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | NAT of (
# 1 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 41 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | LEFT_PAREN
    | LEFT_FIXITY
    | LEFT_BRACE
    | GREATER
    | EOF
    | DOT
    | DOLLAR
    | BAR
    | ASSIGN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 21 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
   open ConcreteSyntaxDescription 
# 62 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | ASSIGN ->
          19
      | BAR ->
          18
      | DOLLAR ->
          17
      | DOT ->
          16
      | EOF ->
          15
      | GREATER ->
          14
      | LEFT_BRACE ->
          13
      | LEFT_FIXITY ->
          12
      | LEFT_PAREN ->
          11
      | NAT _ ->
          10
      | NONTERMINAL_ID _ ->
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
      | STRING _ ->
          3
      | TERMINAL_ID _ ->
          2
      | UNDERSCORE ->
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
      | STRING _v ->
          Obj.repr _v
      | TERMINAL_ID _v ->
          Obj.repr _v
      | UNDERSCORE ->
          Obj.repr ()
  
  and default_reduction =
    (8, "\000\000\007\001\000\000\000/.\000\020\000\000\000!\000\000#\023\021\022*\000\000\029\000\016\000\000\000\000\000\007\000\000)\012-\000\000,\030\000\000'\000\014\000\031\000\000\t\b\025%\000\028\000\018\000\n\002\000\003\000\024\000\004\000\026\000\005\000+\000\006\0000")
  
  and error =
    (20, "\000\000@\002\000\000\000\000\000\000 \000\000\000\001\016\128\000\000\000\000\000\002\004\000\000\000\000\004\000\000\000\022\004\002\000\000\006\004\000`e\000\000\000\000\000\000\000\000\000\000\000\000\000\000C&\004\000\000\000\006\004@\000\000\000\000@\000@@\001\000\004@\192\002\000\000\000\000\192\000\000@\192\000\000\000\000\000\000\000\000\000\128\004\004\000\000\000\000\000\012\000\128\000\004\000\000\000\000\b\000\000\000@\000\000\000\000\016\000\002K \000\000\000\000\000\000\000\000\000\006\004\000\000\000\000\004\016\000\000\000\000\016\000\000\000\000\000`@\000\000\000\000\001\000\000\000`@\000\000\000\000\001\000\000\000\000@\000\000\000\000\001\000\000\000 \000\000\000\000\000\001\000\000\000")
  
  and start =
    6
  
  and action =
    ((8, "\031\016\000\000F,6\000\000F\000>.\004\000.\003\000\000\000\000\000\016.\000.\000<\024H,L\000\002\024\000\000\000B\024\000\000:\024\000:\000Z\000X\002\000\000\000\000.\000>\000J\000\000.\000L\000.\000N\000>\000P\000F\000R\000"), (16, "\000\133\000\133\000\006\000}\000}\000\138\000\157\000\206\000\133\000F\000\141\000}\000\133\000\210\000\133\000\141\000\141\000i\000\011\000\141\000:\000v\000^\000i\000B\000N\000\222\0001\000)\000~\000\030\000v\000R\000u\000u\000\"\0009\000\022\000)\000~\0002\000\026\0006\000r\000I\000\174\000A\000z\000\130\000\158\000\194\000\202\000\243\001\007\001\023\001'\0017"))
  
  and lhs =
    (8, "\005\004\003\002\001\000\029\028\028\027\026\026\025\025\024\024\023\023\022\022\021\021\021\020\019\018\017\017\017\016\016\015\015\014\014\r\r\012\012\011\011\n\t\b\b\007\007\006")
  
  and goto =
    ((8, "!\000\000\000$\000\000\000\000\r\000*\000\b\000\021P\000\000\000\000\000\000\n\000\019\000\000*\000\017\000\000\000\r\000\000\000\000,\000\000\000:\000\000\000\000\000\000\028\000\000\000\000\022\000B\000\000\000\0004\000\000\000\030\000\000\000N\000\000\000\\\000\000\000"), (8, "\"\n\"/%$%\022&\023&\026\026\004\028\027\0110'\016'+\025+\023\n\02669\028\023:\026DE\028\0262)\028\012-7:<>@AHI**LM\019\000;\000."))
  
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
# 193 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 202 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 53 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                           ( _2 )
# 206 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 78 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixl )
# 231 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 79 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixr )
# 256 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list)
# 295 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 43 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                        ( (_1, _2) )
# 299 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _v : 'tv_loption_separated_nonempty_list_DOT_capture_number__ = 
# 142 "<standard.mly>"
    ( [] )
# 317 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let x : 'tv_separated_nonempty_list_DOT_capture_number_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_DOT_capture_number__ = 
# 144 "<standard.mly>"
    ( x )
# 342 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 142 "<standard.mly>"
    ( [] )
# 360 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 144 "<standard.mly>"
    ( x )
# 385 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 32 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 406 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 414 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 32 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 442 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 450 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 471 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_sort_rule_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 479 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 507 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_sort_rule_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 515 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 536 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 544 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 572 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 580 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 601 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 32 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 609 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 106 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( TerminalName    _1 )
# 613 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 634 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 32 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 642 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 107 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( NonterminalName _1 )
# 646 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _2 : 'tv_option_NAT_ = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 32 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 678 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 108 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( Underscore (Belt.Option.getWithDefault _2 1) )
# 682 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 32 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 710 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 718 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 110 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                               ( _1 )
# 722 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 769 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_nonempty_list_nonterminal_token_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 779 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 84 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let fixity = match _5 with
    | None   -> Nofix
    | Some f -> f
    in OperatorMatch { tokens = _1; operator_match_pattern = _3; fixity } )
# 786 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 814 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 822 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 103 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                         ( _1 )
# 826 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 847 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 855 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 68 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [[ _1 ]] )
# 859 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 892 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 898 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 906 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 70 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( match _3 with
      | []      -> [[ _1 ]]
      | x :: xs -> (_1 :: x) :: xs
    )
# 913 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 946 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 34 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 952 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 960 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 75 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [ _1 ] :: _3 )
# 964 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 985 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 993 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 92 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( SingleCapturePattern _1 )
# 997 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 1039 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1047 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 1051 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
        
# 95 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( OperatorPattern (_1, _3) )
# 1056 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 1074 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 1099 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
        let _v : 'tv_option_NAT_ = 
# 114 "<standard.mly>"
    ( None )
# 1117 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 1 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 1138 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_NAT_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1146 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 114 "<standard.mly>"
    ( None )
# 1164 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 116 "<standard.mly>"
    ( Some x )
# 1189 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1210 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_capture_number_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1218 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1253 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_capture_number_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1261 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 241 "<standard.mly>"
    ( [ x ] )
# 1286 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 243 "<standard.mly>"
    ( x :: xs )
# 1325 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 36 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1364 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : 'tv_option_BAR_ = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1371 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 37 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1379 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 59 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( SortRule { sort_name = _1; operator_rules = _4 }
  )
# 1384 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
      (ConcreteSyntaxDescription.sort_rule)
# 1412 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1420 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 55 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                               ( _1 )
# 1424 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1457 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let xs : 'tv_loption_separated_nonempty_list_DOT_capture_number__ = Obj.magic xs in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_term_scope_pattern = let _1 = 
# 232 "<standard.mly>"
    ( xs )
# 1467 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
        
# 99 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( NumberedScopePattern (_1, _3) )
# 1472 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 39 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1493 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_term_scope_pattern = 
# 101 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( NumberedScopePattern ([], _1) )
# 1501 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 1534 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1540 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1548 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 47 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Left _3) )
# 1552 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 1585 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1591 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 29 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1599 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 49 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Right _3) )
# 1603 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
# 29 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1631 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 30 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1639 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 51 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                       ( _1 )
# 1643 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
    (Obj.magic (MenhirInterpreter.entry 74 lexer lexbuf) : (
# 30 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1674 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and sort_rule__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 70 lexer lexbuf) : (
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1682 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and operator_match__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 66 lexer lexbuf) : (
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1690 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_token__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 62 lexer lexbuf) : (
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1698 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and language =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 4 lexer lexbuf) : (
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list)
# 1706 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and capture_number =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1714 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

module Incremental = struct
  
  let terminal_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 74 initial_position) : (
# 30 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1724 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and sort_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 70 initial_position) : (
# 38 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1732 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and operator_match__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 66 initial_position) : (
# 35 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1740 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_token__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 62 initial_position) : (
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1748 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and language =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 4 initial_position) : (
# 40 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list)
# 1756 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and capture_number =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 31 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1764 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 1772 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
