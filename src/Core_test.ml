open Binding
open Dynamics.Core
open AbstractSyntax

let one = Bigint.of_int 1
let sort = SortAp ("bool", [||])
let scope body = Scope ([], body)

let dynamics_str =
  {|
meaning = \(tm : ty()) -> match tm with {
  | true() -> true()
  | false() -> false()
  | ite(t1; t2; t3) -> match meaning t1 with {
    | true()  -> meaning t2
    | false() -> meaning t3
  }
  | ap(f; arg) -> (meaning f) (meaning arg)
  | fun(scope) -> lambda([]; scope) // TODO: add type
}
|}
;;

let meaning x = CoreApp (Var "meaning", [ x ])

let dynamics =
  DenotationChart
    [ ( "meaning"
      , Lambda
          ( [ SortAp ("ty", [||]) ]
          , Scope
              ( [ Var "tm" ]
              , Case
                  ( Var "tm"
                  , [ CaseScope (Operator ("true", []), Operator ("true", []))
                    ; CaseScope (Operator ("false", []), Operator ("false", []))
                    ; CaseScope
                        ( Operator ("ite" , [ Var "t1" ; Var "t2" ; Var "t3" ])
                        , Case
                            ( CoreApp (Var "meaning", [ Var "t1" ])
                            , [ CaseScope (Operator ("true", []), meaning (Var "t2"))
                              ; CaseScope (Operator ("false", []), meaning (Var "t3"))
                              ] ) )
                    ; CaseScope
                        ( Operator ("ap", [ Var "f"; Var "arg" ])
                        , CoreApp (meaning @@ Var "f", [ meaning @@ Var "arg" ]) )
                    ; CaseScope
                        ( Operator ("fun", [ Var "scope" ])
                        , Operator
                            ("lambda", [ scope @@ Sequence []; scope @@ Var "scope" ]) )
                    ] ) ) ) )
    ]
;;

let%test_module "Dynamics.Core" =
  (module struct
    let eval_str = fun str -> print_string (match Parsing.Core.parse str with
      | Error err -> ParseError.to_string err
      | Ok core -> (match eval core with
        | Error (msg, tm) -> msg ^ ": " ^ pp_core_str tm
        | Ok result -> pp_core_str result))

    let%test "dynamics as expected" = Parsing.Dynamics.parse dynamics_str = Ok dynamics

    let%test "to_ast 1" =
      to_ast (Primitive (PrimInteger one)) = Nominal.Primitive (PrimInteger one)
    ;;

    let%test "to_ast 2" =
      to_ast (Operator ("foo", [ scope @@ Primitive (PrimInteger one) ]))
      = Nominal.Operator ("foo", [ Scope ([], Primitive (PrimInteger one)) ])
    ;;

    let%expect_test _ = eval_str "1"; [%expect{| 1 |}]
    let%expect_test _ = eval_str "foo(1)"; [%expect{| foo(1) |}]
    let%expect_test _ = eval_str "true()"; [%expect{| true() |}]
    let%expect_test _ = eval_str "false()"; [%expect{| false() |}]
    let%expect_test _ = eval_str
      {| match true() with {
           | true() -> false()
           | false() -> true()
         }
      |};
      [%expect{| false() |}]
    let%expect_test _ =
      eval_str {|(\(x: bool()) -> x) true()|};
      [%expect{| true() |}]
    let%expect_test _ = eval_str "#add(1; 2)"; [%expect{| 3 |}]
    let%expect_test _ = eval_str "#sub(1; 2)"; [%expect{| -1 |}]
    (* let%expect_test _ = eval_str "#sub 1 2"; [%expect{| -1 |}] *)
  end)
;;
