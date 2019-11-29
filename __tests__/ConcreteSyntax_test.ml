open Jest
open Expect
let (to_ast, to_string, of_ast, mk_tree, parse, equivalent, remove_spaces) =
  ConcreteSyntax.(to_ast, to_string, of_ast, mk_tree, parse, equivalent,
    remove_spaces)
type tree = ConcreteSyntax.tree
open Belt.Result
module Parse_concrete = Parsing.Incremental(Parsing.Parseable_concrete_syntax)
open TestUtil

let nt_capture capture = ConcreteSyntax.NonterminalCapture capture

let mk_terminal_capture content trailing_trivia =
  ConcreteSyntax.TerminalCapture
    { ConcreteSyntax.leading_trivia = ""; content; trailing_trivia }

let _ = describe "ConcreteSyntax" (fun () ->
  let description = {|
  ADD    := "+"
  SUB    := "-"
  MUL    := "*"
  DIV    := "/"
  LPAREN := "("
  RPAREN := ")"
  NAME   := /[a-z][a-zA-Z0-9]*/
  // SPACE  := / +/

  arith :=
    | arith _ ADD _ arith { add($1; $5) }
    | arith _ SUB _ arith { sub($1; $5) }
    | arith _ MUL _ arith { mul($1; $5) }
    | arith _ DIV _ arith { div($1; $5) }
    | LPAREN arith RPAREN { $2          }
    | NAME                { var($1)     }
  |}
  in

  let arith = Types.SortAp ("arith", [||]) in
  let arith' = Types.FixedValence ([], arith) in
  let language = Types.(Language (Belt.Map.String.fromArray [|
    "arith", SortDef ([], [
      OperatorDef ("mul", Arity ([], [ arith'; arith' ]));
      OperatorDef ("div", Arity ([], [ arith'; arith' ]));
      OperatorDef ("add", Arity ([], [ arith'; arith' ]));
      OperatorDef ("sub", Arity ([], [ arith'; arith' ]));
    ])
  |]))
  in

  test "language parses" (fun () ->
    match Parse_concrete.parse description with
      | Ok _concrete -> pass
      | Error msg    -> fail msg
  );

  match Parse_concrete.parse description with
    | Error msg -> failwith msg
    | Ok (pre_terminal_rules, sort_rules) ->
      let concrete =
        ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
      in
      let mk_tree' = mk_tree "arith" in
      let tree = mk_tree' (Operator "add")
            [| nt_capture (mk_tree' Var [| mk_terminal_capture "x" " " |]);
               mk_terminal_capture "+" " ";
               nt_capture (mk_tree' Var [| mk_terminal_capture "y" "" |]);
            |]
      in

      let tree' = mk_tree' (Operator "sub")
            [| nt_capture (mk_tree' Parenthesizing
                [|
                  nt_capture (mk_tree' (Operator "add")
                    [| nt_capture (mk_tree' Var [| mk_terminal_capture "x" " " |]);
                       mk_terminal_capture "+" " ";
                       nt_capture (mk_tree' Var [| mk_terminal_capture "y" "" |]);
                    |]
                  );
                |]
                );
                mk_terminal_capture "-" " ";
                nt_capture (mk_tree' Var [| mk_terminal_capture "z" "" |]);
            |]
      in

      let tree'' = mk_tree' (Operator "add")
            [| nt_capture (mk_tree' Var [| mk_terminal_capture "x" " " |]);
               mk_terminal_capture "+" " ";
               nt_capture (mk_tree' Parenthesizing
                 [|
                   mk_terminal_capture "(" "";
                   nt_capture (mk_tree' (Operator "mul")
                     [| nt_capture (mk_tree' Var [| mk_terminal_capture "y" " " |]);
                        mk_terminal_capture "*" " ";
                        nt_capture (mk_tree' Var [| mk_terminal_capture "z" "" |]);
                     |]
                   );
                   mk_terminal_capture ")" "";
                 |]
               );
            |]
      in

      let ast =
        Binding.Nominal.(Operator ("add",
          [ Scope ([], Var "x");
            Scope ([], Var "y");
          ]))
      in

      test "of_ast" (fun () ->
        expect (of_ast language concrete arith ast)
          (* TODO: should have spaces *)
          |> toEqual (remove_spaces tree);
      );

      test "to_ast" (fun () ->
        expect (to_ast language tree) |> toEqual (Ok ast);
      );

      test "to_string" (fun () ->
        expect (to_string tree) |> toEqual "x + y";
      );

      testAll "parse"
        [
          expect (parse concrete "arith" "x + y")
            |> toEqual (Ok tree);
          expect (parse concrete "arith" "x+y")
            |> toEqual (Ok (remove_spaces tree));
          (*
          expect (parse concrete "x+y-z") |> toEqual (Ok tree');
          expect (
            parse concrete "x + y-z"
              |. Belt.Result.map (equivalent tree')
          ) |> toEqual (Ok true);
          expect (parse concrete "x + y * z") |> toEqual (Ok tree'');
          *)
          expect (parse concrete "arith" "x + (y * z)")
            |> toEqual (Ok tree'');
          expect (parse concrete "arith" "x+(y*z)")
            |> toEqual (Ok (remove_spaces tree''));
        ]
        Util.id;

      let expect_round_trip_tree tree = expect (tree
          |> to_ast language
          |. Belt.Result.map (of_ast language concrete arith)
          |. Belt.Result.getExn
        ) |> toBeEquivalent equivalent tree
      in

      let expect_round_trip_ast tm = expect (tm
          |> of_ast language concrete arith
          |> to_ast language
        ) |> toEqual (Ok tm)
      in

      testAll "round trip ast->tree->ast"
        [ expect_round_trip_tree tree;
          (* expect_round_trip_tree tree'; *)
          (* expect_round_trip_tree tree''; *)
        ] Util.id;

      testAll "round trip tree->ast->tree"
        [ expect_round_trip_ast ast;
        ] Util.id;
)
