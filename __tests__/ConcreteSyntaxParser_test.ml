open Jest
open Expect
open ConcreteSyntaxDescription

let _ = describe "ConcreteSyntax_Parser" (fun () ->
  let expectParse parser str tm = test ("parse '" ^ str ^ "'") (fun () ->
    match parser ConcreteSyntax_Lexer.read (Lexing.from_string str) with
    | tm' -> expect tm' |> toEqual tm
    (* | exception ConcreteSyntax_Parser.Error -> fail ("'" ^ str ^ "' triggered an exception") *)
  ) in

  expectParse ConcreteSyntax.Parser.terminal_rule__test
    {|TERMINAL := /foo/|}
    (PreTerminalRule ("TERMINAL", Left "foo"));

(*   expectParse ConcreteSyntax.Parser.terminal_rule__test *)
(*     {|TERMINAL := "\\"|} *)
(*     (PreTerminalRule ("TERMINAL", [ReString "\\"])); *)

  expectParse ConcreteSyntax.Parser.terminal_rule__test
    {|TERMINAL := "->"|}
    (PreTerminalRule ("TERMINAL", Right "->"));

  expectParse ConcreteSyntax.Parser.terminal_rule__test
    {|ID := /[a-zA-Z][a-zA-Z0-9_]*/|}
    (PreTerminalRule ("ID", Left "[a-zA-Z][a-zA-Z0-9_]*"));
  expectParse ConcreteSyntax.Parser.terminal_rule__test
    {|SPACE := / +/|}
    (PreTerminalRule ("SPACE", Left " +"));

  expectParse ConcreteSyntax.Parser.capture_number "$2" 2;
  expectParse ConcreteSyntax.Parser.nonterminal_token__test
    "foo" (NonterminalName "foo");
  expectParse ConcreteSyntax.Parser.nonterminal_token__test
    "BAR" (TerminalName "BAR");
  expectParse ConcreteSyntax.Parser.nonterminal_token__test
    "_" (Underscore 1);
  expectParse ConcreteSyntax.Parser.nonterminal_token__test
    "_0" (Underscore 0);

  expectParse ConcreteSyntax.Parser.operator_match__test
    "foo BAR baz { foo($1; $2) }"
    (OperatorMatch
      { tokens =
          [ NonterminalName "foo";
            TerminalName    "BAR";
            NonterminalName "baz";
          ];
        operator_match_pattern = OperatorPattern ("foo",
          [ NumberedScopePattern ([], SingleCapturePattern 1);
            NumberedScopePattern ([], SingleCapturePattern 2);
          ]);
        fixity = Nofix;
      }
    );

  expectParse ConcreteSyntax.Parser.nonterminal_rule__test
    {|
       arith :=
         | arith ADD arith { add($1; $3) } %left
         | arith SUB arith { sub($1; $3) } %left
         > NAME            { $1          }
    |}
    (NonterminalRule
      { nonterminal_name = "arith"
      ; nonterminal_type = NonterminalType ([], SortAp ("arith", [||]))
      ; operator_rules =
          [[

            OperatorMatch
              { tokens =
                  [ NonterminalName "arith";
                    TerminalName    "ADD";
                    NonterminalName "arith";
                  ];
                operator_match_pattern = OperatorPattern ("add",
                  [ NumberedScopePattern ([], SingleCapturePattern 1);
                    NumberedScopePattern ([], SingleCapturePattern 3);
                  ]);
                fixity = Infixl;
              };

            OperatorMatch
              { tokens =
                  [ NonterminalName "arith";
                    TerminalName    "SUB";
                    NonterminalName "arith"
                  ];
                operator_match_pattern = OperatorPattern ("sub",
                  [ NumberedScopePattern ([], SingleCapturePattern 1);
                    NumberedScopePattern ([], SingleCapturePattern 3);
                  ]);
                fixity = Infixl;
              };
          ];

          [ OperatorMatch
              { tokens = [TerminalName "NAME"];
                operator_match_pattern = SingleCapturePattern 1;
                fixity = Nofix;
              };
          ]];
      });
)
