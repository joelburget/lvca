open Lvca_provenance
open AbstractSyntax

let%test_module "AbstractSyntax.Parser" =
  (module struct
    let parse str =
      Lvca_parsing.parse_string AbstractSyntax.Parse.whitespace_t str
      |> Base.Result.ok_or_failwith
    ;;

    let tm_sort = Sort.Name ((), "tm")
    let tm_valence = Valence.Valence ([], tm_sort)
    let ty_sort = Sort.Name ((), "ty")
    let ty_valence = Valence.Valence ([], ty_sort)
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

      foo (x : *) :=
        | foo(foo[x]. x; x. x)
        | bar(x)
      |}
      |> erase_info
      =
      let externals = [ "integer", Kind.Kind ((), 1); "list", Kind ((), 2) ] in
      let sort_defs =
        [ ( "ty"
          , SortDef.SortDef
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
              ( [ "x", Some (Kind ((), 1)) ]
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

    let%test _ =
      let lang =
        parse {|
      integer : *
      list : * -> *

      foo := Foo()
      |}
      in
      lang.externals
      = [ "integer", Kind.Kind (OptRange.mk 17 18, 1)
        ; "list", Kind (OptRange.mk 32 38, 2)
        ]
    ;;
  end)
;;
