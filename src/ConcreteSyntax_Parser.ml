
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
    | RIGHT_BRACKET
    | RIGHT_BRACE
    | RIGHT_ANGLE
    | REGEX of (
# 2 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 33 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
  )
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
    | LEFT_BRACKET
    | LEFT_BRACE
    | LEFT_ANGLE
    | FORALL
    | EOF
    | DOT
    | DOLLAR
    | COMMA
    | COLON
    | BAR
    | ASSIGN
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 28 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
   open ConcreteSyntaxDescription

let concretize_vars : Belt.Set.String.t -> Types.sort -> Types.sort
  = fun var_set ->
    let open Types in
    let rec go = function
      | SortAp (name, args)
      -> SortAp (name, Belt.Array.map args go)
      | SortVar name
      -> if Belt.Set.String.has var_set name
         then SortVar name
         else SortAp (name, [||])
    in go

# 82 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | ARROW ->
          26
      | ASSIGN ->
          25
      | BAR ->
          24
      | COLON ->
          23
      | COMMA ->
          22
      | DOLLAR ->
          21
      | DOT ->
          20
      | EOF ->
          19
      | FORALL ->
          18
      | LEFT_ANGLE ->
          17
      | LEFT_BRACE ->
          16
      | LEFT_BRACKET ->
          15
      | LEFT_FIXITY ->
          14
      | LEFT_PAREN ->
          13
      | NAT _ ->
          12
      | NONTERMINAL_ID _ ->
          11
      | REGEX _ ->
          10
      | RIGHT_ANGLE ->
          9
      | RIGHT_BRACE ->
          8
      | RIGHT_BRACKET ->
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
      | ARROW ->
          Obj.repr ()
      | ASSIGN ->
          Obj.repr ()
      | BAR ->
          Obj.repr ()
      | COLON ->
          Obj.repr ()
      | COMMA ->
          Obj.repr ()
      | DOLLAR ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | FORALL ->
          Obj.repr ()
      | LEFT_ANGLE ->
          Obj.repr ()
      | LEFT_BRACE ->
          Obj.repr ()
      | LEFT_BRACKET ->
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
      | RIGHT_ANGLE ->
          Obj.repr ()
      | RIGHT_BRACE ->
          Obj.repr ()
      | RIGHT_BRACKET ->
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
    (8, "\000\000\012\001\000\000\000HG\000\029\000\000\000\000\000\021\000:9\000\000\n\000\000\tED\000\023\000\000='\000\0001\000\0003%#\"$\000\000\000\000\000?\017\000\011!5\031\000\000-\000\027\000\000\000\000\000\012\000\000CF\000\000A.\019\000/\000\000\014\r)7\000,\000\000\030\000\025\000\015\002\000\003\000 \000\004\000&\000\005\000(\000\006\000*\000\007\000;\000\b\000I")
  
  and error =
    (27, "\000\000\004\000\001\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\132\b\000\000\000\000\000\000\000\000\004\002\000\000\000\000\000\000\b\000\000\000\000\020\000(@\000\004\000\000\000\128@\000\000\000\000\000\001\000\000\000\000\000\000\000\000\001@\000\b( \192\000\000\000\000\160\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000AA\006\000\000\000\000\000\004\024\000\160\000\000\000\000\000\000\000\000\000\000\0010\136\128@\000\000\000\194\"\000\024Fd\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\017\029\000\000 \000\000\018\000\000\002\000\016\000\b\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000P\016\140\" \000\000\000\0000\136\192\000\000\000\000\000\001\000\000\004\001\000\000 \000\004\016\004\000\001\000\000\000\000\000\006\000\000\000\001\000@\000\000\000\000\000\000\000`\000@\000\016\004\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\000\000 \000\000\018\144\132\000\000\000\000\000\000\000\000\000\000\000\000\000\006\017\016\000\000\000\000\024D@#\b\136\000\000\000\000\000\002\002\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\016\000\000\000\001\132D\000\000\000\000\000\000\001\000\000\000\000\000\005\b\000\000\000\000\000\000\016\000\000\000\001\132D\000\000\000\000\000\000\001\000\000\000\000\000\000\b\000\000\000\000\000\000\016\000\000\000\000\128\000\000\000\000\000\000\000\001\000\000\000\000\000")
  
  and start =
    8
  
  and action =
    ((8, "%.\000\000J0\004\000\000\128\000\134\012\019vv\000|\000\000~\030\000~\156\000\000\000&\000\\~\000\000vF\000\\\003\000\000\000\000\000\012\148\014\136\014\000\000\156\000\000\000\000Z\\\000\\\000\144,\152,\158\000\152,\000\000^,\000\000\000\174\000\170b\000\000\000\000\\\000F\\\000\134\000\150\000\000\134\000\152\000\\\000\154\000\019\000\156\000\\\000\158\000\019\000\160\000J\000\162\000"), (16, "\000\197\000\197\000\221\000\006\000\221\000\030\000\197\000\205\000\205\000:\000\197\000\158\000\"\000\205\000\197\000\197\000=\000\205\000\197\000\190\000%\000\205\000\205\000\182\000U\000\205\000Z\000E\000^\0006\000Z\001Z\000^\000\254\000%\000\011\000\189\000\189\000U\000\022\000%\000%\000\189\001\006\000U\000U\000\189\000\154\000\166\000\026\000\189\000\253\000\253\000\170\000\230\001B\000\169\000\174\000\213\000\146\000\213\000\178\000e\001F\000\169\000\237\000\022\001\"\000\213\001R\000>\000\237\000~\000\213\000V\000m\000^\000\245\0002\000M\001\018\001\005\000J\000f\000\142\000\186\000]\000\210\000\250\001\002\000\194\001\n\0016\001>\001s\001\135\001\151\001\167\001\183\001\199\001\215"))
  
  and lhs =
    (8, "\007\006\005\004\003\002\001\000,,+*))(''&&%%$$##\"\"!!  \031\030\030\030\030\030\029\028\027\026\025\024\024\024\023\023\022\022\021\021\020\020\019\019\018\018\017\016\015\015\014\014\r\r\012\012\011\011\n\t\t\b")
  
  and goto =
    ((8, "=\000\000\000\014\000\000\000\000N\000$\0004.D\000\000\000\000$\030\000R\000\000\000\000&\000\000<\000\000\000f\000!r\000\000\000\000\000H\000V\000\140\000\000\000\000\000\000\000\000\031\000;\000\000F\000\021\000\000\000\r\000\000\0004\000\000\000\000\000\000T\000\000\000\000\t\000\128\007\000@\000\000\000\000D\000\000\000P\000\000\000\014\000\000\000\024\000\000\000v\000\000\000\144\000\000\000"), (8, "D<LGD=FG8;99\004H<<\nH>>VY99\020\021<<M\031>>K\"hiKlmJ\012\031<\020\021!>^\nHZ\028\025\\#\03063O\029\018S\027\029Z`a[Kde\017\011&\027pqK)7tu4T2\027X"))
  
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
        let _3 : unit = Obj.magic _3 in
        let _2 : 'tv_sort = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_atomic_sort = 
# 102 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( _2 )
# 253 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 274 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_atomic_sort = 
# 105 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( Types.SortVar _1 )
# 282 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let xs : 'tv_loption_separated_nonempty_list_COMMA_NAT__ = Obj.magic xs in
        let _2 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 323 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_box_formatting_options = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 332 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
        
# 195 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let box_type = match _2 with
      | "h" -> HBox
      | "v" -> VBox
      | "hov" -> HovBox
      | "b" -> BBox
      | "hv" -> HvBox
      | _ -> failwith "TODO: error"
    in
    box_type, _3
  )
# 346 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _2 : (
# 1 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 373 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 55 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 382 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 79 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                           ( _2 )
# 386 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 154 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixl )
# 411 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 155 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                 ( Infixr )
# 436 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _2 : 'tv_nonempty_list_nonterminal_rule_ = Obj.magic _2 in
        let _1 : 'tv_nonempty_list_terminal_rule_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 66 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list)
# 475 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 69 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                               ( (_1, _2) )
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
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_NAT__ = 
# 142 "<standard.mly>"
    ( [] )
# 497 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let x : 'tv_separated_nonempty_list_COMMA_NAT_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_NAT__ = 
# 144 "<standard.mly>"
    ( x )
# 522 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 540 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 586 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_NONTERMINAL_ID_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 594 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let xs : 'tv_nonempty_list_NONTERMINAL_ID_ = Obj.magic xs in
        let x : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 622 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_NONTERMINAL_ID_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 630 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let x : 'tv_atomic_sort = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_atomic_sort_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 655 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let xs : 'tv_nonempty_list_atomic_sort_ = Obj.magic xs in
        let x : 'tv_atomic_sort = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_atomic_sort_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 687 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 61 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 708 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_nonterminal_rule_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 716 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let xs : 'tv_nonempty_list_nonterminal_rule_ = Obj.magic xs in
        let x : (
# 61 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 744 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_nonterminal_rule_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 752 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 773 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 781 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 809 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 817 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 53 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 838 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 846 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 53 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 874 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 882 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 921 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : 'tv_option_BAR_ = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 928 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 61 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 936 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 124 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( NonterminalRule
    { nonterminal_name = _1
    ; nonterminal_type = NonterminalType ([], SortAp (_1, [||]))
    ; operator_rules = _4
    }
  )
# 945 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
          MenhirLib.EngineTypes.semv = _6;
          MenhirLib.EngineTypes.startp = _startpos__6_;
          MenhirLib.EngineTypes.endp = _endpos__6_;
          MenhirLib.EngineTypes.next = {
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
          };
        } = _menhir_stack in
        let _6 : (
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 996 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _6 in
        let _5 : 'tv_option_BAR_ = Obj.magic _5 in
        let _4 : unit = Obj.magic _4 in
        let _3 : (
# 63 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1003 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1009 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : (
# 61 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 1017 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 131 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( NonterminalRule
    { nonterminal_name = _1
    ; nonterminal_type = _3
    ; operator_rules = _6
    }
  )
# 1026 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 61 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 1054 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 62 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 1062 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 81 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                             ( _1 )
# 1066 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _2 : 'tv_option_box_formatting_options_ = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1098 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 208 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( OpenBox _2 )
# 1102 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _v : (
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1127 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 209 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( CloseBox           )
# 1131 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1152 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1160 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 210 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( TerminalName    _1 )
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
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 1185 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1193 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 211 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( NonterminalName _1 )
# 1197 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1229 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 213 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                    ( Underscore (Belt.Option.getWithDefault _2 1) )
# 1233 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 56 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1261 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 57 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1269 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 215 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                               ( _1 )
# 1273 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _2 : 'tv_separated_nonempty_list_ARROW_sort_ = Obj.magic _2 in
        let _1 : 'tv_option_quantifiers_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 63 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1305 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 111 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let var_set = match _1 with
      | None -> Belt.Set.String.empty
      | Some var_set -> var_set
    in
    let arg_sorts, result_sort = _2
      |. Belt.List.map (concretize_vars var_set)
      |. Util.unsnoc
    in
    NonterminalType (arg_sorts, result_sort)
  )
# 1318 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 63 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1346 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 64 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1354 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 107 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                             ( _1 )
# 1358 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 65 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1405 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_nonempty_list_nonterminal_token_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 58 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1415 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 160 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let fixity = match _5 with
    | None   -> Nofix
    | Some f -> f
    in OperatorMatch { tokens = _1; operator_match_pattern = _3; fixity } )
# 1422 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 58 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1450 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 59 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1458 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 190 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                         ( _1 )
# 1462 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 58 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1483 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1491 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 144 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [[ _1 ]] )
# 1495 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1528 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 58 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1534 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1542 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 146 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( match _3 with
      | []      -> [[ _1 ]]
      | x :: xs -> (_1 :: x) :: xs
    )
# 1549 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1582 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 58 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1588 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 60 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1596 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 151 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
    ( [ _1 ] :: _3 )
# 1600 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 55 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1621 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 65 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1629 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 168 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( SingleCapturePattern _1 )
# 1633 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1675 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 65 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1683 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 1687 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
        
# 171 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( OperatorPattern (_1, _3) )
# 1692 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1710 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1735 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1753 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1774 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_NAT_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1782 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _v : 'tv_option_box_formatting_options_ = 
# 114 "<standard.mly>"
    ( None )
# 1800 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let x : 'tv_box_formatting_options = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_box_formatting_options_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1825 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1843 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 1868 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _v : 'tv_option_quantifiers_ = 
# 114 "<standard.mly>"
    ( None )
# 1886 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 51 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Belt.Set.String.t)
# 1907 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_quantifiers_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1915 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _2 : 'tv_nonempty_list_NONTERMINAL_ID_ = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 51 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Belt.Set.String.t)
# 1954 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 85 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( _2
    |. Belt.List.toArray
    |. Belt.Set.String.fromArray
  )
# 1961 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 51 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Belt.Set.String.t)
# 1989 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 52 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Belt.Set.String.t)
# 1997 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 90 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                   ( _1 )
# 2001 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let x : 'tv_sort = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_ARROW_sort_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2026 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let xs : 'tv_separated_nonempty_list_ARROW_sort_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_sort = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_ARROW_sort_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2065 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 2086 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_NAT_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2094 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let xs : 'tv_separated_nonempty_list_COMMA_NAT_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 1 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (int)
# 2129 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_NAT_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2137 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 65 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 2158 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_operator_match_pattern_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2166 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let xs : 'tv_separated_nonempty_list_DOT_operator_match_pattern_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 65 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 2201 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_operator_match_pattern_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2209 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 2234 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 2273 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _2 : 'tv_nonempty_list_atomic_sort_ = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 2301 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_sort = 
# 96 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( Types.SortAp (_1, Belt.List.toArray _2) )
# 2309 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _1 : 'tv_atomic_sort = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_sort = 
# 98 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( _1 )
# 2334 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
        let _1 : 'tv_separated_nonempty_list_DOT_operator_match_pattern_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_term_scope_pattern = 
# 175 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( let capture_nums, body = Util.unsnoc _1 in

    let capture_nums' = capture_nums
      |. Belt.List.map (function
        | SingleCapturePattern n -> PatternCapture n
        | OperatorPattern
          ("var", [NumberedScopePattern ([], SingleCapturePattern n)])
        -> VarCapture n
        | OperatorPattern _ -> failwith "TODO: message 1"
      )
    in

    NumberedScopePattern (capture_nums', body)
  )
# 2372 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 2405 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 2411 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 53 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2419 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 73 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Left _3) )
# 2423 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 2456 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
       (string)
# 2462 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 53 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2470 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 75 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Right _3) )
# 2474 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
         in
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
# 53 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2502 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 54 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2510 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
        ) = 
# 77 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
                                       ( _1 )
# 2514 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
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
    (Obj.magic (MenhirInterpreter.entry 114 lexer lexbuf) : (
# 54 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2545 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and quantifiers__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 110 lexer lexbuf) : (
# 52 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Belt.Set.String.t)
# 2553 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and operator_match__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 106 lexer lexbuf) : (
# 59 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 2561 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_type__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 102 lexer lexbuf) : (
# 64 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 2569 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_token__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 98 lexer lexbuf) : (
# 57 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 2577 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_rule__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 94 lexer lexbuf) : (
# 62 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 2585 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and language =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 4 lexer lexbuf) : (
# 66 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list)
# 2593 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

and capture_number =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 55 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 2601 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
    ))

module Incremental = struct
  
  let terminal_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 114 initial_position) : (
# 54 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2611 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and quantifiers__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 110 initial_position) : (
# 52 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (Belt.Set.String.t)
# 2619 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and operator_match__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 106 initial_position) : (
# 59 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 2627 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_type__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 102 initial_position) : (
# 64 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 2635 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_token__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 98 initial_position) : (
# 57 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 2643 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 94 initial_position) : (
# 62 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 2651 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and language =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 4 initial_position) : (
# 66 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list)
# 2659 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and capture_number =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 55 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 2667 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 2675 "/Users/joel/code/lvca-bucklescript/src/ConcreteSyntax_Parser.ml"
