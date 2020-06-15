open AbstractSyntax

let%test_module "AbstractSyntax.Parser" =
  (module struct

    module AbstractSyntaxParse = AbstractSyntax.Parse(struct
      open Angstrom
      let comment =
        string "//" >>= fun _ ->
        many (satisfy (fun x -> x <> '\n')) >>| fun _ ->
        ()
      let reserved = Util.String.Set.empty
    end)

    let parse str =
      match
        Angstrom.parse_string ~consume:All
          Angstrom.(Util.Angstrom.whitespace *> AbstractSyntaxParse.t)
          str
      with
        | Ok t -> t
        | Error msg -> failwith msg

    let tm_sort = SortAp ("tm", [])
    let tm_valence = FixedValence ([], tm_sort)
    let ty_sort = SortAp ("ty", [])
    let ty_valence = FixedValence ([], ty_sort)
    let x_sort = SortVar "x"

    let%test _ = parse "bool := true() | false()" =
      { imports = []
      ; sort_defs = SortDefs (Util.String.Map.of_alist_exn [
        "bool", SortDef ([],
        [ OperatorDef ("true", FixedArity [])
        ; OperatorDef ("false", FixedArity [])
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
      ; sort_defs = SortDefs (Util.String.Map.of_alist_exn [
        "ty", SortDef ([],
          [ OperatorDef ("bool", FixedArity [])
          ; OperatorDef ("arr", FixedArity [ ty_valence; ty_valence ])
          ]);

        "tm", SortDef ([],
          [ OperatorDef ("app", FixedArity [ tm_valence; tm_valence ])
          ; OperatorDef ("lam", FixedArity [ FixedValence ([tm_sort], tm_sort) ])
          ]);

        "foo", SortDef (["x"],
          [ OperatorDef ("foo", FixedArity
            [ VariableValence (x_sort, x_sort)
            ; FixedValence ([x_sort], x_sort)
            ])
          ; OperatorDef ("bar", VariableArity x_sort)
          ]);
        ]);
      }

  end)
;;
