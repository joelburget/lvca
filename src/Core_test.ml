open Binding
open Dynamics.Core
open AbstractSyntax

let one = Bigint.of_int 1
let sort = SortAp ("bool", [||])
let pat_scope body : BindingAwarePattern.scope = Scope ([], body)
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
                  , [ CaseScope ([ Operator ("true", []) ], Operator ("true", []))
                    ; CaseScope ([ Operator ("false", []) ], Operator ("false", []))
                    ; CaseScope
                        ( [ Operator
                              ( "ite"
                              , [ pat_scope @@ Var "t1"
                                ; pat_scope @@ Var "t2"
                                ; pat_scope @@ Var "t3"
                                ] )
                          ]
                        , Case
                            ( CoreApp (Var "meaning", [ Var "t1" ])
                            , [ CaseScope ([ Operator ("true", []) ], meaning (Var "t2"))
                              ; CaseScope ([ Operator ("false", []) ], meaning (Var "t3"))
                              ] ) )
                    ; CaseScope
                        ( [ Operator
                              ("ap", [ pat_scope @@ Var "f"; pat_scope @@ Var "arg" ])
                          ]
                        , CoreApp (meaning @@ Var "f", [ meaning @@ Var "arg" ]) )
                    ; CaseScope
                        ( [ Operator ("fun", [ pat_scope @@ Var "scope" ]) ]
                        , Operator
                            ("lambda", [ scope @@ Sequence []; scope @@ Var "scope" ]) )
                    ] ) ) ) )
    ]
;;

let dynamics' = Parsing.Dynamics.parse dynamics_str

let true_val = Operator ("true", [])
let false_val = Operator ("false", [])

(* let ite_tm = DeBruijn.Operator ("ite", [ scope true_tm; scope false_tm; scope true_tm;
   ]) *)

let ite_val =
  Case
    ( true_val
    , [ CaseScope ([ Operator ("true", []) ], false_val)
      ; CaseScope ([ Operator ("false", []) ], true_val)
      ] )
;;

(* let fun_tm = DeBruijn.Operator ("ap", [ scope @@ Operator ("fun", [ Scope ([Var "x"],
   Var (0, 0)) ]); scope true_tm; ]) *)

let fun_val = CoreApp (Lambda ([ sort ], Scope ([ Var "x" ], Var "x")), [ true_val ])

let binary_int_op op a b =
  Operator
    ( op
    , [ Scope ([], Primitive (PrimInteger (Bigint.of_int a)))
      ; Scope ([], Primitive (PrimInteger (Bigint.of_int b)))
      ] )
;;

let%test_module "Dynamics.Core" =
  (module struct
    let%test "dynamics as expected" = dynamics' = Ok dynamics

    let%test "to_ast 1" =
      to_ast (Primitive (PrimInteger one)) = Nominal.Primitive (PrimInteger one)
    ;;

    let%test "to_ast 2" =
      to_ast (Operator ("foo", [ scope @@ Primitive (PrimInteger one) ]))
      = Nominal.Operator ("foo", [ Scope ([], Primitive (PrimInteger one)) ])
    ;;

    let%test "eval 1" = eval true_val = Ok true_val
    let%test "eval 2" = eval false_val = Ok false_val
    let%test "eval 3" = eval ite_val = Ok false_val
    let%test "eval 4" = eval fun_val = Ok true_val

    let%test "eval 5" =
      eval (binary_int_op "#add" 1 2) = Ok (Primitive (PrimInteger (Bigint.of_int 3)))
    ;;

    let%test "eval 6" =
      eval (binary_int_op "#sub" 1 2) = Ok (Primitive (PrimInteger (Bigint.of_int (-1))))
    ;;
  end)
;;
