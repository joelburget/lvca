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

let nt_capture capture = ConcreteSyntax.NonterminalCapture capture

let mk_terminal_capture content = ConcreteSyntax.TerminalCapture
  { ConcreteSyntax.leading_trivia = ""; content; trailing_trivia = "" }

let _ = describe "ConcreteSyntax" (fun () ->
  let description = {|
  ADD    := "+"
  SUB    := "-"
  MUL    := "*"
  DIV    := "/"
  LPAREN := "("
  RPAREN := ")"
  NAME   := [a-z][a-zA-Z0-9]*
  SPACE  := [ ]+

  arith :=
    | LPAREN arith RPAREN { $2          }
    > arith _ MUL _ arith { mul($1; $5) } %left
    | arith _ DIV _ arith { div($1; $5) } %left
    > arith _ ADD _ arith { add($1; $5) } %left
    | arith _ SUB _ arith { sub($1; $5) } %left
    > NAME                { var($1)     }
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

  testAll "regex_piece_to_string"
    [ expect (regex_piece_to_string (ReString "+")) |> toBe "\\+";
      expect (regex_piece_to_string (ReString "*")) |> toBe "\\*";
      expect (regex_piece_to_string (ReString "?")) |> toBe "\\?";
      expect (regex_piece_to_string (ReString "-")) |> toBe "\\-";
      expect (regex_piece_to_string (ReSet  "a-z")) |> toBe "[a-z]";
    ]
    Util.id;

  let module Parse_concrete = Parsing.Incremental(Parsing.Parseable_concrete_syntax) in

  test "language parses" (fun () ->
    match Parse_concrete.parse description with
      | Ok _concrete -> pass
      | Error msg    -> fail msg
  );

  match Parse_concrete.parse description with
    | Error msg -> failwith msg
    | Ok concrete ->
      let arith = Types.SortAp ("arith", [||]) in
      let tree = mk_tree arith (Operator "add")
            [| nt_capture (mk_tree arith Var [| mk_terminal_capture "x" |]);
               mk_terminal_capture "+";
               nt_capture (mk_tree arith Var [| mk_terminal_capture "y" |]);
            |]
      in
      Js.log tree;
      let tree' = mk_tree arith (Operator "sub")
            [| nt_capture (mk_tree arith (Operator "add")
                 [| nt_capture (mk_tree arith Var [| mk_terminal_capture "x" |]);
                    mk_terminal_capture "+";
                    nt_capture (mk_tree arith Var [| mk_terminal_capture "y" |]);
                 |]
               );
               mk_terminal_capture "-";
               nt_capture (mk_tree arith Var [| mk_terminal_capture "z" |]);
            |]
      in
      let tree'' = mk_tree arith (Operator "add")
            [| nt_capture (mk_tree arith Var [| mk_terminal_capture "x" |]);
               mk_terminal_capture "+";
               nt_capture (mk_tree arith (Operator "mul")
                 [| nt_capture (mk_tree arith Var [| mk_terminal_capture "y" |]);
                    mk_terminal_capture "*";
                    nt_capture (mk_tree arith Var [| mk_terminal_capture "z" |]);
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
          (*
          expect (
            parse concrete "x + y"
              |. Belt.Result.map (equivalent tree)
          ) |> toEqual (Ok true);
          expect (parse concrete "x+y-z") |> toEqual (Ok tree');
          expect (
            parse concrete "x + y-z"
              |. Belt.Result.map (equivalent tree')
          ) |> toEqual (Ok true);
          expect (parse concrete "x + y * z") |> toEqual (Ok tree'');
          expect (parse concrete "x + (y * z)") |> toEqual (Ok tree'');
          *)
        ]
        Util.id;
)
