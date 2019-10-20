open Jest
open Expect
open Types.ConcreteSyntaxDescription

let _ = describe "ConcreteSyntaxParser" (fun () ->
  let expectParse parser str tm = test ("parse '" ^ str ^ "'") (fun () ->
    match parser ConcreteSyntaxLexer.read (Lexing.from_string str) with
    | tm' -> expect tm' |> toEqual tm
    (* | exception ConcreteSyntaxParser.Error -> fail ("'" ^ str ^ "' triggered an exception") *)
  ) in

  (* Expect (show_regex . parse) = id *)
  let expectRoundTrip parser str = test ("round trip '" ^ str ^ "'") (fun () ->
    match parser ConcreteSyntaxLexer.read (Lexing.from_string str) with
    | re -> expect (show_regex re) |> toEqual str
  ) in

  expectParse ConcreteSyntaxParser.regex__test {|"foo"|}    [ReString "foo"];
  expectParse ConcreteSyntaxParser.regex__test "[a-z]"      [ReSet    "a-z"];
  expectParse ConcreteSyntaxParser.regex__test "[a-zA-Z]"   [ReSet    "a-zA-Z"];
  expectParse ConcreteSyntaxParser.regex__test "[a-z][A-Z]" [ReSet    "a-z"; ReSet "A-Z"];
  expectParse ConcreteSyntaxParser.regex__test {|"foo"*|}   [ReStar   (ReString "foo")];
  expectParse ConcreteSyntaxParser.regex__test {|"foo"+|}   [RePlus   (ReString "foo")];
  expectParse ConcreteSyntaxParser.regex__test {|"foo"?|}   [ReOption (ReString "foo")];

  expectParse ConcreteSyntaxParser.regex__test {|"\\"|}     [ReString {|\|}];
  expectParse ConcreteSyntaxParser.regex__test {|"\b"|}     [ReClass  {|\b|}];

  expectRoundTrip ConcreteSyntaxParser.regex__test {|"foo"|};
  expectRoundTrip ConcreteSyntaxParser.regex__test "[a-z]";
  expectRoundTrip ConcreteSyntaxParser.regex__test "[a-zA-Z]";
  expectRoundTrip ConcreteSyntaxParser.regex__test "[a-z][A-Z]";
  expectRoundTrip ConcreteSyntaxParser.regex__test {|"foo"*|};
  expectRoundTrip ConcreteSyntaxParser.regex__test {|"foo"+|};
  expectRoundTrip ConcreteSyntaxParser.regex__test {|"foo"?|};

  expectParse ConcreteSyntaxParser.terminal_rule__test
    {|TERMINAL := "foo"|}
    (TerminalRule ("TERMINAL", [ReString "foo"]));

(*   expectParse ConcreteSyntaxParser.terminal_rule__test *)
(*     {|TERMINAL := "\\"|} *)
(*     (TerminalRule ("TERMINAL", [ReString "\\"])); *)

  expectParse ConcreteSyntaxParser.terminal_rule__test
    {|TERMINAL := "->"|}
    (TerminalRule ("TERMINAL", [ReString "->"]));

  expectParse ConcreteSyntaxParser.terminal_rule__test
    {|ID := [a-zA-Z][a-zA-Z0-9_]*|}
    (TerminalRule ("ID", [ReSet "a-zA-Z"; ReStar (ReSet "a-zA-Z0-9_")]));
  expectParse ConcreteSyntaxParser.terminal_rule__test
    {|SPACE := [ ]+|}
    (TerminalRule ("SPACE", [RePlus (ReSet " ")]));

  expectParse ConcreteSyntaxParser.capture_number "$2" 2;
  expectParse ConcreteSyntaxParser.nonterminal_token "foo" (NonterminalName "foo");
  expectParse ConcreteSyntaxParser.nonterminal_token "BAR" (TerminalName "BAR");

  expectParse ConcreteSyntaxParser.operator_match__test
    "foo BAR baz { foo($1; $2) }"
    (OperatorMatch
      { tokens =
          [ NonterminalName "foo";
            TerminalName    "BAR";
            NonterminalName "baz"
          ];
        term_pattern = TermPattern ("foo", [NumberedScopePattern ([], 1); NumberedScopePattern ([], 2)]);
        fixity = Nofix;
      }
    );

  expectParse ConcreteSyntaxParser.sort_rule__test
    {|
       arith :=
         | arith ADD arith { add($1; $3) } %left
         | arith SUB arith { sub($1; $3) } %left
         > NAME            { var($1)     }
    |}
    (SortRule
      { sort_name = "arith";
        operator_rules =
          [[
            OperatorMatch
              { tokens =
                  [ NonterminalName "arith";
                    TerminalName    "ADD";
                    NonterminalName "arith"
                  ];
                term_pattern = TermPattern ("add", [NumberedScopePattern ([], 1); NumberedScopePattern ([], 3)]);
                fixity = Infixl;
              };
            OperatorMatch
              { tokens =
                  [ NonterminalName "arith";
                    TerminalName    "SUB";
                    NonterminalName "arith"
                  ];
                term_pattern = TermPattern ("sub", [NumberedScopePattern ([], 1); NumberedScopePattern ([], 3)]);
                fixity = Infixl;
              };
          ]];
        variable = Some { tokens = [TerminalName "NAME"]; var_capture = 1 };
      });
)
