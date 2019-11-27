
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20181113

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNDERSCORE
    | TERMINAL_ID of (
# 3 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 17 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | STRING of (
# 5 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 22 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_FIXITY
    | RIGHT_BRACE
    | REGEX of (
# 2 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | NONTERMINAL_ID of (
# 4 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 36 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
    | NAT of (
# 1 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 41 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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

# 21 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
   open ConcreteSyntaxDescription 
# 62 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"

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
    (8, "\000\000\007\001\000\000\000+*\000\018\000\000\000\030\000\000 \021\019\020'\000\000\026\000\014\000\000\000\000\000\007\000\000&)\000\000$\012\000\028\000\000\t\b\022\"\027\000\025\000\016\000\n\002\000\000 \021\019\020\003\000\004\000\023\000\005\000(\000\006\000,")
  
  and error =
    (20, "\000\000@\002\000\000\000\000\000\000 \000\000\000\001\016\128\000\000\000\000\000\002\004\000\000\000\000\004\000\000\000\022\004\002\000\000\006\004\000`d\000\000\000\000\000\000\000\000\000\000\000\000\000\000C&\004\000\000\000\006\004@\000\000\000\000@\000@@\001\000\004\000@\002\000\000\000\000\192\000\000\000@\000\000\000\000\000\192\b\000\000@\000\000\000\000\000@\000\000\000\000\016\000\002K \000\000\000\000\000\000\000\000\000\000\000\000`@\000\000\000\000A\000\000\000\000\001\000\000\000\000\000\006\004\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000`@\000\000\000\000\001\000\000\000\000@\000\000\000\000\001\000\000\000 \000\000\000\000\000\001\000\000\000")
  
  and start =
    6
  
  and action =
    ((8, "!6\000\000\030(\003\000\000\030\000:*\024\000\028\018\000\000\000\000\000\022\028\000\028\000:&D<H\000\000&\000\0006&\000\000V\000T\r\000\000\000\000\000\028\000:\000F\000\000@R\000\000\000\000\000\028\000J\000:\000L\000\030\000N\000"), (16, "\000\182\000\006\000\030\000\129\000\138\000\145\000\186\000\"\000\129\000\129\000y\000y\000\129\000q\000q\000B\000N\000\022\000y\000F\000]\000q\000y\000R\000A\000^\000]\0001\000v\000\202\000:\000\137\000\137\000\234\000\247\000)\000~\000\011\0002\000\026\0006\000\251\000r\000\154\0009\000z\000\130\000~\000\170\000\178\000\223\000\239\001\015\001\031\001/"))
  
  and lhs =
    (8, "\005\004\003\002\001\000\027\026\026\025\024\024\023\023\022\022\021\021\020\020\020\019\018\017\017\017\016\016\015\015\014\014\r\r\012\012\011\011\n\t\b\007\007\006")
  
  and goto =
    ((8, "3\000\000\000\020\000\000\000\000\"\000\019\000:\000\025@\000\000\000\000\000\000\011\000\020\000\000$\000\017\000\000\000\005\000\000\000\018\000\000\000\000\0000\000\000\000\000\000\006\000:\000\000\000\0006D\000\000\000\000\000\018\000\000\000>\000\000\000H\000\000\000"), (8, "\"5\004)%\022\"\023\026$%\028\0257\023\026*\n\028&4(\023\026\n&\028BC\026\026\012\028\027,9&0\0115FGJK\0162\019@=\00016"))
  
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
# 1 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 193 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 202 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 52 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                           ( _2 )
# 206 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 78 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixl )
# 231 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 79 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixr )
# 256 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 39 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list)
# 295 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 42 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                        ( (_1, _2) )
# 299 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 142 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 317 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 144 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x )
# 342 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 363 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 221 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 371 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 399 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 223 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 407 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 36 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 428 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_sort_rule_ = 
# 221 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 436 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 36 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 464 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_sort_rule_ = 
# 223 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 472 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 29 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 493 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 221 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 501 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 29 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 529 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 223 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 537 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 3 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 558 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 566 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 105 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( TerminalName    _1 )
# 570 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 4 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 591 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 599 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 106 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( NonterminalName _1 )
# 603 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 635 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 107 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    (
    let n = match _2 with
      | Some n -> n
      | None -> 1
    in
    Underscore n
  )
# 645 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 38 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 692 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_nonempty_list_nonterminal_token_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 33 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 702 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 84 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let fixity = (match _5 with
    | None   -> Nofix
    | Some f -> f
    )
    in OperatorMatch { tokens = _1; operator_match_pattern = _3; fixity } )
# 710 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 33 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 738 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 34 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 746 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 102 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                         ( _1 )
# 750 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 33 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 771 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 779 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 68 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [[ _1 ]] )
# 783 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 816 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 33 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 822 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 830 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 70 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( match _3 with
      | []      -> [[ _1 ]]
      | x :: xs -> (_1 :: x) :: xs
    )
# 837 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 870 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 33 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 876 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 884 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 75 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [ _1 ] :: _3 )
# 888 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 909 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 38 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 917 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 93 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( ParenthesizingPattern _1 )
# 921 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 4 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 963 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 38 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 971 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = let _3 = 
# 232 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 975 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
        
# 96 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( OperatorPattern (_1, _3) )
# 980 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 114 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( None )
# 998 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 116 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( Some x )
# 1023 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 114 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( None )
# 1041 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 1062 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_NAT_ = 
# 116 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( Some x )
# 1070 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 114 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( None )
# 1088 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 116 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( Some x )
# 1113 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1134 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_capture_number_ = 
# 241 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1142 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1177 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_capture_number_ = 
# 243 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1185 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 241 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1210 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 243 "/home/joel/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 1249 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 35 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1288 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : 'tv_option_BAR_ = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1295 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 36 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1303 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 58 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let (operator_rules, variable) = partition_nonterminal_matches(_4) in
    SortRule { sort_name = _1; operator_rules; variable }
  )
# 1309 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 36 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1337 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 37 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1345 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 54 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                               ( _1 )
# 1349 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 100 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let (binds, body) = Util.unsnoc _1 in NumberedScopePattern (binds, body) )
# 1374 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 2 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1407 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1413 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 29 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1421 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 46 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Left _3) )
# 1425 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 5 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1458 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1464 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 29 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1472 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 48 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Right _3) )
# 1476 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 29 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1504 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 30 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1512 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 50 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                       ( _1 )
# 1516 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
    (Obj.magic (MenhirInterpreter.entry 72 lexer lexbuf) : (
# 30 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1547 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and sort_rule__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 68 lexer lexbuf) : (
# 37 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1555 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and operator_match__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 64 lexer lexbuf) : (
# 34 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1563 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_token =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 57 lexer lexbuf) : (
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1571 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and language =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 4 lexer lexbuf) : (
# 39 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list)
# 1579 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and capture_number =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1587 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

module Incremental = struct
  
  let terminal_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 72 initial_position) : (
# 30 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 1597 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and sort_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 68 initial_position) : (
# 37 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.sort_rule)
# 1605 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and operator_match__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 64 initial_position) : (
# 34 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1613 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_token =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 57 initial_position) : (
# 32 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1621 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and language =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 4 initial_position) : (
# 39 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list)
# 1629 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and capture_number =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 31 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1637 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "/home/joel/.opam/default/lib/menhir/standard.mly"
  

# 1645 "/home/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
