open ConcreteSyntaxDescription
module P = ConcreteSyntax.Parser

let expect_parse parser str tm =
  assert (parser ConcreteSyntax_Lexer.read (Lexing.from_string str) = tm)

let%test_module "ConcreteSyntax_Parser" = (module struct

  let%test_unit "" = expect_parse P.terminal_rule__test
    {|TERMINAL := /foo/|}
    (PreTerminalRule ("TERMINAL", First "foo"))

(*   expect_parse P.terminal_rule__test *)
(*     {|TERMINAL := "\\"|} *)
(*     (PreTerminalRule ("TERMINAL", [ReString "\\"])) *)

  let%test_unit "" = expect_parse P.terminal_rule__test
    {|TERMINAL := "->"|}
    (PreTerminalRule ("TERMINAL", Second "->"))

  let%test_unit "" = expect_parse P.terminal_rule__test
    {|ID := /[a-zA-Z][a-zA-Z0-9_]*/|}
    (PreTerminalRule ("ID", First "[a-zA-Z][a-zA-Z0-9_]*"))
  let%test_unit "" = expect_parse P.terminal_rule__test
    {|SPACE := / +/|}
    (PreTerminalRule ("SPACE", First " +"))

  let%test_unit "" = expect_parse P.capture_number "$2" 2
  let%test_unit "" = expect_parse P.nonterminal_token__test
    "foo" (NonterminalName "foo")
  let%test_unit "" = expect_parse P.nonterminal_token__test
    "BAR" (TerminalName "BAR")
  let%test_unit "" = expect_parse P.nonterminal_token__test
    "_" (Underscore 1)
  let%test_unit "" = expect_parse P.nonterminal_token__test
    "_0" (Underscore 0)
  let%test_unit "" = expect_parse P.nonterminal_token__test
    "[<hov>" (OpenBox (Some (HovBox, [])))
  let%test_unit "" = expect_parse P.nonterminal_token__test
    "[<hv 0,1,2>" (OpenBox (Some (HvBox, [0; 1; 2])))

  let%test_unit "" = expect_parse P.operator_match__test
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
    )

  let%test_unit "" = expect_parse P.nonterminal_rule__test
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
      })

  let%test_unit "" = expect_parse P.nonterminal_type__test
    "list int"
    (NonterminalType ([], SortAp ("list", [| SortAp ("int", [||]) |])))

  let%test_unit "" =
    let list_int = Types.SortAp ("list", [| SortAp ("int", [||]) |]) in
    expect_parse P.nonterminal_type__test
      "list int -> list int"
      (NonterminalType ([list_int], list_int))

      (* TODO: test
  let%test_unit "" = assert
    (P.quantifiers__test ConcreteSyntax_Lexer.read
      (Lexing.from_string "forall a b.") =
    (Core_kernel.String.Set.of_list [ "a"; "b" ]))
*)

  let%test_unit "" = expect_parse P.nonterminal_type__test
    "forall a. list a"
    (NonterminalType ([], SortAp ("list", [| SortVar "a" |])))

  let%test_unit "" = expect_parse P.nonterminal_rule__test
    {|
       list
         : forall a. a -> list a
         := L_BRACKET [ inner_list ] R_BRACKET { $2 }
    |}

    (NonterminalRule
      { nonterminal_name = "list"
      ; nonterminal_type = NonterminalType
        ([SortVar "a"], SortAp ("list", [| SortVar "a"|]))
      ; operator_rules =
        [[
          OperatorMatch
            { tokens =
              [ TerminalName "L_BRACKET";
                OpenBox None;
                NonterminalName "inner_list";
                CloseBox;
                TerminalName "R_BRACKET";
              ]
            ; operator_match_pattern = SingleCapturePattern 2
            ; fixity = Nofix;
            };
        ]]
      }
    )

end)
