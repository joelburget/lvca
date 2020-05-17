open AbstractSyntax
module String = Util.String

let parse str = Parser.language_def Lexer.read (Lexing.from_string str)
;;

let tm_sort = SortAp ("tm", [])
let tm_valence = FixedValence ([], tm_sort)
let ty_sort = SortAp ("ty", [])
let ty_valence = FixedValence ([], ty_sort)

let%test_module "AbstractSyntax.Parser" =
  (module struct
    let%test _ = AbstractSyntax.eq (parse "bool := true() | false()")
      { imports = []
      ; sort_defs = SortDefs (String.Map.of_alist_exn [
        "bool", SortDef ([],
        [ OperatorDef ("true", FixedArity [])
        ; OperatorDef ("false", FixedArity [])
        ])
      ])
      }

    let%test _ = AbstractSyntax.eq
      (parse {|
      ty :=
        | bool()
        | arr(ty(); ty())

      tm :=
        | app(tm(); tm())
        | lam(tm(). tm())
      |})
      { imports = []
      ; sort_defs = SortDefs (String.Map.of_alist_exn [
        "ty", SortDef ([],
          [ OperatorDef ("bool", FixedArity [])
          ; OperatorDef ("arr", FixedArity [ ty_valence; ty_valence ])
          ]);

        "tm", SortDef ([],
          [ OperatorDef ("app", FixedArity [ tm_valence; tm_valence ])
          ; OperatorDef ("lam", FixedArity [ FixedValence ([tm_sort], tm_sort) ])
          ]);
        ]);
      }

  end)
;;
