
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20200211

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNDERSCORE
    | TERMINAL_ID of (
# 3 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 17 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
  )
    | STRING of (
# 5 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 22 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
  )
    | SEMICOLON
    | RIGHT_PAREN
    | RIGHT_FIXITY
    | RIGHT_BRACKET
    | RIGHT_BRACE
    | RIGHT_ANGLE
    | REGEX of (
# 2 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 33 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
  )
    | NONTERMINAL_ID of (
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 38 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
  )
    | NAT of (
# 1 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (int)
# 43 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
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

# 28 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
   open ConcreteSyntaxDescription
open Tablecloth

let concretize_vars : StrSet.t -> Types.sort -> Types.sort
  = fun var_set ->
    let open Types in
    let rec go = function
      | SortAp (name, args)
      -> SortAp (name, Array.map args ~f:go)
      | SortVar name
      -> if StrSet.member var_set ~value:name
         then SortVar name
         else SortAp (name, [||])
    in go

# 83 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"

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
# 100 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( _2 )
# 254 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 275 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_atomic_sort = 
# 103 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( Types.SortVar _1 )
# 283 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 324 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_box_formatting_options = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 333 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        
# 193 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
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
# 347 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 1 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (int)
# 374 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 56 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 383 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 80 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                           ( _2 )
# 387 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 152 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                 ( Infixl )
# 412 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 153 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                 ( Infixr )
# 437 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 67 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list)
# 476 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 70 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                                               ( (_1, _2) )
# 480 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
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
# 498 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 523 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
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
# 541 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 566 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 587 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_NONTERMINAL_ID_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 595 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 623 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_NONTERMINAL_ID_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 631 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 656 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 688 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 62 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 709 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_nonterminal_rule_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 717 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 62 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 745 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_nonterminal_rule_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 753 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 774 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 782 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 810 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_nonterminal_token_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 818 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 54 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 839 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 847 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 54 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 875 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_terminal_rule_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 883 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 922 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _4 in
        let _3 : 'tv_option_BAR_ = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 929 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 62 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 937 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 122 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( NonterminalRule
    { nonterminal_name = _1
    ; nonterminal_type = NonterminalType ([], SortAp (_1, [||]))
    ; operator_rules = _4
    }
  )
# 946 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 997 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _6 in
        let _5 : 'tv_option_BAR_ = Obj.magic _5 in
        let _4 : unit = Obj.magic _4 in
        let _3 : (
# 64 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1004 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 1010 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : (
# 62 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 1018 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 129 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( NonterminalRule
    { nonterminal_name = _1
    ; nonterminal_type = _3
    ; operator_rules = _6
    }
  )
# 1027 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 62 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 1055 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 63 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 1063 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 82 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                                             ( _1 )
# 1067 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1099 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 206 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( OpenBox _2 )
# 1103 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1128 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 207 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                    ( CloseBox           )
# 1132 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 3 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 1153 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1161 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 208 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                    ( TerminalName    _1 )
# 1165 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 1186 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1194 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 209 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                    ( NonterminalName _1 )
# 1198 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1230 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 211 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                    ( Underscore (Option.with_default _2 ~default:1) )
# 1234 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 57 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1262 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 58 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 1270 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 213 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                                               ( _1 )
# 1274 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 64 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1306 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 109 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( let var_set = match _1 with
      | None -> StrSet.empty
      | Some var_set -> var_set
    in
    let arg_sorts, result_sort = _2
      |. List.map ~f:(concretize_vars var_set)
      |. Util.unsnoc
    in
    NonterminalType (arg_sorts, result_sort)
  )
# 1319 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 64 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1347 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 65 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 1355 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 105 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                                             ( _1 )
# 1359 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 66 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1406 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : 'tv_nonempty_list_nonterminal_token_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 59 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1416 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 158 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( let fixity = match _5 with
    | None   -> Nofix
    | Some f -> f
    in OperatorMatch { tokens = _1; operator_match_pattern = _3; fixity } )
# 1423 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 59 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1451 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 60 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1459 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 188 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                                         ( _1 )
# 1463 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 59 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1484 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1492 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 142 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
    ( [[ _1 ]] )
# 1496 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1529 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 59 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1535 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1543 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 144 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
    ( match _3 with
      | []      -> [[ _1 ]]
      | x :: xs -> (_1 :: x) :: xs
    )
# 1550 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1583 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 59 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 1589 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 61 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match list list)
# 1597 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 149 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
    ( [ _1 ] :: _3 )
# 1601 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 56 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 1622 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 66 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1630 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 166 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( SingleCapturePattern _1 )
# 1634 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 1676 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 66 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 1684 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = let _3 = 
# 232 "<standard.mly>"
    ( xs )
# 1688 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        
# 169 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( OperatorPattern (_1, _3) )
# 1693 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
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
# 1711 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 1736 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
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
# 1754 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 1 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (int)
# 1775 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_NAT_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1783 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
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
# 1801 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 1826 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
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
# 1844 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 1869 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
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
# 1887 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 52 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (Tablecloth.StrSet.t)
# 1908 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_quantifiers_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1916 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 52 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (Tablecloth.StrSet.t)
# 1955 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 86 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( StrSet.from_list _2 )
# 1959 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 52 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (Tablecloth.StrSet.t)
# 1987 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 53 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (Tablecloth.StrSet.t)
# 1995 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 88 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                                   ( _1 )
# 1999 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 2024 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 2063 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 1 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (int)
# 2084 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_NAT_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2092 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 1 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (int)
# 2127 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_NAT_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2135 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 66 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 2156 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_DOT_operator_match_pattern_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2164 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 66 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match_pattern)
# 2199 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_DOT_operator_match_pattern_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2207 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 2232 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 2271 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 4 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 2299 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_sort = 
# 94 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( Types.SortAp (_1, Array.from_list _2) )
# 2307 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 96 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( _1 )
# 2332 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 173 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( let capture_nums, body = Util.unsnoc _1 in

    let capture_nums' = capture_nums
      |> List.map ~f:(function
        | SingleCapturePattern n -> PatternCapture n
        | OperatorPattern
          ("var", [NumberedScopePattern ([], SingleCapturePattern n)])
        -> VarCapture n
        | OperatorPattern _ -> failwith "TODO: message 1"
      )
    in

    NumberedScopePattern (capture_nums', body)
  )
# 2370 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 2 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 2403 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 2409 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 54 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2417 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 74 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Left _3) )
# 2421 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 5 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 2454 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 3 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
       (string)
# 2460 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 54 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2468 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 76 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
  ( PreTerminalRule (_1, Right _3) )
# 2472 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
         in
        {
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
# 54 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2500 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 55 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2508 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
        ) = 
# 78 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
                                       ( _1 )
# 2512 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
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
# 55 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2543 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

and quantifiers__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 110 lexer lexbuf) : (
# 53 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (Tablecloth.StrSet.t)
# 2551 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

and operator_match__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 106 lexer lexbuf) : (
# 60 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 2559 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_type__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 102 lexer lexbuf) : (
# 65 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 2567 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_token__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 98 lexer lexbuf) : (
# 58 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 2575 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

and nonterminal_rule__test =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 94 lexer lexbuf) : (
# 63 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 2583 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

and language =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 4 lexer lexbuf) : (
# 67 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list)
# 2591 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

and capture_number =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 56 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 2599 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
    ))

module Incremental = struct
  
  let terminal_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 114 initial_position) : (
# 55 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule)
# 2609 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and quantifiers__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 110 initial_position) : (
# 53 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (Tablecloth.StrSet.t)
# 2617 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and operator_match__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 106 initial_position) : (
# 60 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.operator_match)
# 2625 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_type__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 102 initial_position) : (
# 65 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_type)
# 2633 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_token__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 98 initial_position) : (
# 58 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_token)
# 2641 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and nonterminal_rule__test =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 94 initial_position) : (
# 63 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.nonterminal_rule)
# 2649 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and language =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 4 initial_position) : (
# 67 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list)
# 2657 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and capture_number =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 56 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.mly"
      (ConcreteSyntaxDescription.capture_number)
# 2665 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 269 "<standard.mly>"
  

# 2673 "/Users/joel/code/lvca-bucklescript/src-shared/ConcreteSyntax_Parser.ml"
