open Binding
open Nominal

let expect_parse str tm =
  Term.Parser.top_term Term.Lexer.read (Lexing.from_string str) = tm
;;

let%test_module "TermParser" =
  (module struct
    let%test _ = expect_parse "x" (Var "x")
    let%test _ = expect_parse "123" (Primitive (PrimInteger (Bigint.of_int 123)))
    let%test _ = expect_parse "\"abc\"" (Primitive (PrimString "abc"))

    let%test _ =
      expect_parse "lam(x. x)" (Operator ("lam", [ Scope ([ Var "x" ], Var "x") ]))

    let%test _ =
      let match_line a b = Operator ("match_line", [ Scope ([a], b) ]) in
      let match_lines subtms = Operator
        ( "match_lines"
        , Core_kernel.List.map subtms ~f:(fun tm -> Scope ([], tm))
        )
      in

      expect_parse {|
        match(x; match_lines(
          match_line(foo(). true());
          match_line(bar(_; _x; y). y)
        ))
      |}
      (Operator ("match", [
        Scope ([], Var "x");
        Scope ([], match_lines [
          match_line (Pattern.Operator ("foo", [])) (Operator ("true", []));
          match_line
            (Pattern.Operator ("bar", [Ignored ""; Ignored "x"; Var "y"]))
            (Var "y");
        ]);
      ]))

  end)
;;
