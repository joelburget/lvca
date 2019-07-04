open Jest
open Expect
open Types.ConcreteSyntax

let _ = describe "ConcreteSyntaxParser" (fun () ->
  let expectParse parser str tm = test ("'" ^ str ^ "'") (fun () ->
    (match parser ConcreteSyntaxLexer.read (Lexing.from_string str) with
    | tm' -> expect tm' |> toEqual tm
    (* | exception ConcreteSyntaxParser.Error -> fail ("'" ^ str ^ "' triggered an exception") *)
  )) in

  expectParse ConcreteSyntaxParser.regex "\"foo\"" "foo";

  expectParse ConcreteSyntaxParser.terminal_rule
    "TERMINAL := \"foo\""
    (TerminalRule ("TERMINAL", "foo"));

  expectParse ConcreteSyntaxParser.capture_number "$2" 2;
  expectParse ConcreteSyntaxParser.nonterminal_token "foo" (NonterminalName "foo");
  expectParse ConcreteSyntaxParser.nonterminal_token "BAR" (TerminalName "BAR");

  expectParse ConcreteSyntaxParser.operator_match__test
    "foo; BAR; baz { foo($1; $2) }"
    (OperatorMatch
      { tokens =
          [ NonterminalName "foo";
            TerminalName    "BAR";
            NonterminalName "baz"
          ];
        term_pattern = ("foo", [TermScope ([], 1); TermScope ([], 2)])
      }
    );

  expectParse ConcreteSyntaxParser.sort_rule__test
    {|
       arith :=
         | arith; ADD; arith { add($1; $3) }
         | arith; SUB; arith { sub($1; $3) }
         | NAME              { var($1)     }
    |}
    (SortRule
      { sort_name = "arith";
        operator_rules =
          [
            OperatorMatch
              { tokens =
                  [ NonterminalName "arith";
                    TerminalName    "ADD";
                    NonterminalName "arith"
                  ];
                term_pattern = ("add", [TermScope ([], 1); TermScope ([], 3)])
              };
            OperatorMatch
              { tokens =
                  [ NonterminalName "arith";
                    TerminalName    "SUB";
                    NonterminalName "arith"
                  ];
                term_pattern = ("sub", [TermScope ([], 1); TermScope ([], 3)])
              };
          ];
        variable = Some { tokens = [TerminalName "NAME"] };
      });
)
