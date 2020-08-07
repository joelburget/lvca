open AbstractSyntax

let%test_module "AbstractSyntax.Parser" =
  (module struct

    module AbstractSyntaxParse = AbstractSyntax.Parse(Lvca_util.Angstrom.CComment)

    let parse str =
      match
        Angstrom.parse_string ~consume:All
          Angstrom.(Lvca_util.Angstrom.whitespace *> AbstractSyntaxParse.t)
          str
      with
        | Ok t -> t
        | Error msg -> failwith msg

    let tm_sort = SortAp ("tm", [])
    let tm_valence = Valence ([], (tm_sort, Unstarred))
    let ty_sort = SortAp ("ty", [])
    let ty_valence = Valence ([], (ty_sort, Unstarred))
    let x_sort = SortVar "x"

    let%test _ = parse "bool := true() | false()" =
      { imports = []
      ; sort_defs = SortDefs (Lvca_util.String.Map.of_alist_exn [
        "bool", SortDef ([],
        [ OperatorDef ("true", [])
        ; OperatorDef ("false", [])
        ])
      ])
      }

    let%test _ =
      parse {|
      ty :=
        | bool()
        | arr(ty(); ty())

      tm :=
        | app(tm(); tm())
        | lam(tm(). tm())

      foo(x) :=
        | foo(x*. x; x. x) // fixed arity, (variable valence, fixed valence)
        | bar(x*)          // variable arity
      |}
      =
      { imports = []
      ; sort_defs = SortDefs (Lvca_util.String.Map.of_alist_exn [
        "ty", SortDef ([],
          [ OperatorDef ("bool", [])
          ; OperatorDef ("arr", [ ty_valence; ty_valence ])
          ]);

        "tm", SortDef ([],
          [ OperatorDef ("app", [ tm_valence; tm_valence ])
          ; OperatorDef ("lam", [ Valence ([tm_sort, Unstarred], (tm_sort, Unstarred)) ])
          ]);

        "foo", SortDef (["x"],
          [ OperatorDef ("foo",
            [ Valence ([x_sort, Starred], (x_sort, Unstarred))
            ; Valence ([x_sort, Unstarred], (x_sort, Unstarred))
            ])
          ; OperatorDef ("bar", [Valence ([], (x_sort, Starred))])
          ]);
        ]);
      }

  end)
;;