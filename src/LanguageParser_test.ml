open AbstractSyntax
open Core_kernel

let parse str = Parser.language_def Lexer.read (Lexing.from_string str)
;;

let tm_sort = SortAp ("tm", [||])
let tm_valence = FixedValence ([], tm_sort)
let ty_sort = SortAp ("ty", [||])
let ty_valence = FixedValence ([], ty_sort)

let%test_module "AbstractSyntax.Parser" =
  (module struct
    let%test _ = AbstractSyntax.eq (parse "bool := true() | false()")
      { imports = []
      ; sort_defs = SortDefs (String.Map.of_alist_exn [
        "bool", SortDef ([],
        [ OperatorDef ("true", Arity ([], []))
        ; OperatorDef ("false", Arity ([], []))
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
          [ OperatorDef ("bool", Arity ([], []))
          ; OperatorDef ("arr", Arity ([], [ ty_valence; ty_valence ]))
          ]);

        "tm", SortDef ([],
          [ OperatorDef ("app", Arity ([], [ tm_valence; tm_valence ]))
          ; OperatorDef ("lam", Arity ([], [ FixedValence ([tm_sort], tm_sort) ]))
          ]);
        ]);
      }

  end)
;;
