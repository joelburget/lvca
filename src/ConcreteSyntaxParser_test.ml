open ConcreteSyntaxDescription
module P = ConcreteSyntax.Parser

let expect_parse parser str tm =
  assert (parser ConcreteSyntax_Lexer.read (Lexing.from_string str) = tm)
;;

let%test_module "ConcreteSyntax_Parser" =
  (module struct
    let%test_unit _ =
      expect_parse
        P.terminal_rule__test
        {|TERMINAL := /foo/|}
        (PreTerminalRule ("TERMINAL", First "foo"))
    ;;

    let%test_unit _ =
      expect_parse
        P.terminal_rule__test
        {|TERMINAL := "\\"|}
        (PreTerminalRule ("TERMINAL", Second "\\"))
    ;;

    let%test_unit _ =
      expect_parse
        P.terminal_rule__test
        {|TERMINAL := "->"|}
        (PreTerminalRule ("TERMINAL", Second "->"))
    ;;

    let%test_unit _ =
      expect_parse
        P.terminal_rule__test
        {|ID := /[a-zA-Z][a-zA-Z0-9_]*/|}
        (PreTerminalRule ("ID", First "[a-zA-Z][a-zA-Z0-9_]*"))
    ;;

    let%test_unit _ =
      expect_parse
        P.terminal_rule__test
        {|SPACE := / +/|}
        (PreTerminalRule ("SPACE", First " +"))
    ;;

    let%test_unit _ =
      expect_parse P.nonterminal_token__test "foo = bar"
      (nonterminal_name (Some "foo") "bar")
    ;;

    let%test_unit _ =
      expect_parse P.nonterminal_token__test "foo = BAR"
      (terminal_name (Some "foo") "BAR")
    let%test_unit _ = expect_parse P.nonterminal_token__test "_" (Underscore 1)
    let%test_unit _ = expect_parse P.nonterminal_token__test "_0" (Underscore 0)

    let%test_unit _ =
      expect_parse P.nonterminal_token__test "[<hov>" (OpenBox (Some (HovBox, [])))
    ;;

    let%test_unit _ =
      expect_parse
        P.nonterminal_token__test
        "[<hv 0,1,2>"
        (OpenBox (Some (HvBox, [ 0; 1; 2 ])))
    ;;

    let%test_unit _ =
      expect_parse
        P.operator_match__test
        "foo = foo bar = BAR baz = baz { quux(foo; bar) }"
        (OperatorMatch
           { tokens =
             [ nonterminal_name (Some "foo") "foo"
             ; terminal_name (Some "bar") "BAR"
             ; nonterminal_name (Some "baz") "baz"
             ]
           ; operator_match_pattern =
               OperatorPattern
                 ( "quux"
                 , [ NamedScopePattern ([], SingleCapturePattern "foo")
                   ; NamedScopePattern ([], SingleCapturePattern "bar")
                   ] )
           })
    ;;

    let%test_unit _ =
      expect_parse
        P.nonterminal_rule__test
        {|
       arith :=
         | a = arith ADD b = arith { add(a; b) }
         | a = arith SUB b = arith { sub(a; b) }
         | name = NAME             { var(name) }
    |}
        (NonterminalRule
           { nonterminal_name = "arith"
           ; result_sort = None
           ; operator_rules =
               [ OperatorMatch
                   { tokens =
                       [ nonterminal_name (Some "a") "arith"
                       ; terminal_name None "ADD"
                       ; nonterminal_name (Some "b") "arith"
                       ]
                   ; operator_match_pattern =
                       OperatorPattern
                         ( "add"
                         , [ NamedScopePattern ([], SingleCapturePattern "a")
                           ; NamedScopePattern ([], SingleCapturePattern "b")
                           ] )
                   }
               ; OperatorMatch
                   { tokens =
                       [ nonterminal_name (Some "a") "arith"
                       ; terminal_name None "SUB"
                       ; nonterminal_name (Some "b") "arith"
                       ]
                   ; operator_match_pattern =
                       OperatorPattern
                         ( "sub"
                         , [ NamedScopePattern ([], SingleCapturePattern "a")
                           ; NamedScopePattern ([], SingleCapturePattern "b")
                           ] )
                   }
               ; OperatorMatch
                   { tokens = [ terminal_name (Some "name") "NAME" ]
                   ; operator_match_pattern = OperatorPattern
                     ( "var"
                     , [ NamedScopePattern ([], SingleCapturePattern "name") ]
                     )
                   }
               ]
           })
    ;;

    let%test_unit _ =
      expect_parse
        P.nonterminal_rule__test
        {|
          list : list(int())
            := L_BRACKET [ inner_list = inner_list ] R_BRACKET { inner_list }
        |}
        (NonterminalRule
           { nonterminal_name = "list"
           ; result_sort = Some (SortAp ("list", [| SortAp ("int", [||]) |]))
           ; operator_rules =
               [ OperatorMatch
                   { tokens =
                       [ terminal_name None "L_BRACKET"
                       ; OpenBox None
                       ; nonterminal_name (Some "inner_list") "inner_list"
                       ; CloseBox
                       ; terminal_name None "R_BRACKET"
                       ]
                   ; operator_match_pattern = SingleCapturePattern "inner_list"
                   }
               ]
           })
    ;;

  end)
;;
