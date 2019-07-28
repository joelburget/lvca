open Jest
open Expect
let (to_ast, to_string, of_ast, mk_tree, parse, equivalent,
  regex_piece_to_string) =
  ConcreteSyntax.(to_ast, to_string, of_ast, mk_tree, parse, equivalent,
    regex_piece_to_string)
type tree = ConcreteSyntax.tree
open Belt.Result

(*
let toBeEquivalent : ('a -> 'a -> bool) -> 'a -> [< 'a partial] -> assertion
  = fun equiv t1 -> function
    | `Just t2 -> toBe (equiv t1 t2) (`Just true)
    | `Not  t2 -> toBe (equiv t1 t2) (`Not  true)
in
*)

let _ = describe "ConcreteSyntax" (fun () ->
  let description = {|
  ADD  := "+"
  SUB  := "-"
  NAME := [a-z][a-zA-Z0-9]*

  arith :=
    | arith ADD arith { add($1; $3) }
    | arith SUB arith { sub($1; $3) }
    | NAME            { var($1)     }
  |}
  in
  (*
  LPAREN := "("
  RPAREN := ")"
    | LPAREN; arith; RPAREN { $1 }
    *)

  let language = Types.(Language (Belt.Map.String.fromArray [|
    "arith", SortDef ([], [
      OperatorDef ("add", Arity ([], [
        FixedValence ([], SortAp ("arith", [||]));
        FixedValence ([], SortAp ("arith", [||]));
        ]))
    ])
  |]))
  in

  testAll "regex_piece_to_string"
    [ expect (regex_piece_to_string (ReString "+")) |> toBe "\\+";
      expect (regex_piece_to_string (ReString "*")) |> toBe "\\*";
      expect (regex_piece_to_string (ReString "?")) |> toBe "\\?";
      expect (regex_piece_to_string (ReString "-")) |> toBe "\\-";
      expect (regex_piece_to_string (ReSet  "a-z")) |> toBe "[a-z]";
    ]
    Util.id;

  test "language parses" (fun () ->
    let lex = Lexing.from_string description in
    match ConcreteSyntaxParser.language ConcreteSyntaxLexer.read lex with
      _concrete -> pass
  );

  let lex = Lexing.from_string description in
  match ConcreteSyntaxParser.language ConcreteSyntaxLexer.read lex with
    concrete ->
      let arith = Types.SortAp ("arith", [||]) in
      let tree = mk_tree arith (Operator "add")
            [| Right (mk_tree arith Var [| Left "x" |]);
               Left "+";
               Right (mk_tree arith Var [| Left "y" |]);
            |]
      in
      let tree' = mk_tree arith (Operator "sub")
            [| Right (mk_tree arith (Operator "add")
                 [| Right (mk_tree arith Var [| Left "x" |]);
                    Left "+";
                    Right (mk_tree arith Var [| Left "y" |]);
                 |]
               );
               Left "-";
               Right (mk_tree arith Var [| Left "z" |]);
            |]
      in
      let ast =
        Binding.Nominal.(Operator ("add",
          [ Scope ([], Var "x");
            Scope ([], Var "y");
          ]))
      in

      test "of_ast" (fun () ->
        expect (of_ast language concrete arith ast) |> toEqual tree;
      );

      test "to_ast" (fun () ->
        expect (to_ast language tree) |> toEqual (Ok ast);
      );

      test "to_string" (fun () ->
        expect (to_string tree) |> toEqual "x+y";
      );

      testAll "parse"
        [ expect (parse concrete "x+y") |> toEqual (Ok tree);
          expect (
            parse concrete "x + y"
              |. Belt.Result.map (equivalent tree)
          ) |> toEqual (Ok true);
          expect (parse concrete "x+y-z") |> toEqual (Ok tree');
          expect (
            parse concrete "x + y-z"
              |. Belt.Result.map (equivalent tree')
          ) |> toEqual (Ok true);
        ]
        Util.id;
)
