open AbstractSyntax

let%test_module "AbstractSyntax.Parser" =
  (module struct
    module AbstractSyntaxParse = AbstractSyntax.Parse (ParseUtil.CComment)

    let parse str =
      ParseUtil.parse_string AbstractSyntaxParse.whitespace_t str
      |> Base.Result.ok_or_failwith
    ;;

    let tm_sort = Sort.Name ((), "tm")
    let tm_valence = Valence ([], tm_sort)
    let ty_sort = Sort.Name ((), "ty")
    let ty_valence = Valence ([], ty_sort)
    let foo_sort = Sort.Name ((), "foo")
    let x_sort = Sort.Name ((), "x")

    let%test _ =
      parse "bool := true() | false()"
      = { externals = []
        ; sort_defs =
            [ "bool", SortDef ([], [ OperatorDef ("true", []); OperatorDef ("false", []) ])
            ]
        }
    ;;

    let%test _ =
      parse
        {|
      integer : *
      list : * -> *

      ty :=
        | bool()
        | arr(ty; ty)

      tm :=
        | app(tm; tm)
        | lam(tm. tm)

      foo x :=
        | foo(foo[x]. x; x. x) // fixed arity, (variable valence, fixed valence)
        | bar(x)          // variable arity
      |}
      |> erase_info
      =
      let externals = [ "integer", Kind 1; "list", Kind 2 ] in
      let sort_defs =
        [ ( "ty"
          , SortDef
              ( []
              , [ OperatorDef ("bool", [])
                ; OperatorDef ("arr", [ ty_valence; ty_valence ])
                ] ) )
        ; ( "tm"
          , SortDef
              ( []
              , [ OperatorDef ("app", [ tm_valence; tm_valence ])
                ; OperatorDef ("lam", [ Valence ([ SortBinding tm_sort ], tm_sort) ])
                ] ) )
        ; ( "foo"
          , SortDef
              ( [ "x" ]
              , [ OperatorDef
                    ( "foo"
                    , [ Valence
                          ( [ SortPattern { pattern_sort = foo_sort; var_sort = x_sort } ]
                          , x_sort )
                      ; Valence ([ SortBinding x_sort ], x_sort)
                      ] )
                ; OperatorDef ("bar", [ Valence ([], x_sort) ])
                ] ) )
        ]
      in
      { externals; sort_defs }
    ;;
  end)
;;
