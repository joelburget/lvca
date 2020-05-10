open Dynamics.Core

let expect_parse str tm = match Parsing.Dynamics.parse str with
  | Error err -> print_string (ParseError.to_string err); false
  | dyn -> dyn = Ok tm

let dynamics x = CoreApp (Var "dynamics", [ x ])
let scope : Binding.Nominal.term -> Binding.Nominal.scope
  = fun x -> Scope ([], x)

let dyn1 =
  {|
dynamics = \(tm : ty()) -> match tm with {
| true()          -> true()
| false()         -> false()
| val(v)          -> v
| annot(tm; ty)   -> dynamics tm
| app(fun; arg)   -> (dynamics fun) (dynamics arg)
| lam(scope) -> lambda([]; scope) // TODO: add type
| ite(t1; t2; t3) -> match dynamics t1 with {
  | true()  -> dynamics t2
  | false() -> dynamics t3
}
};
|}
;;

let expected =
  DenotationChart
    [ ( "dynamics"
      , Lambda
          ( [ SortAp ("ty", [||]) ]
          , Scope
              ( [ "tm" ]
              , Case
                  ( Var "tm"
                  , [ CaseScope (Operator ("true", []), Term (Operator ("true", [])))
                    ; CaseScope (Operator ("false", []), Term (Operator ("false", [])))
                    ; CaseScope (Operator ("val", [ Var "v" ]), Var "v")
                    ; CaseScope
                        ( Operator ("annot", [ Var "tm"; Var "ty" ])
                        , dynamics @@ Var "tm" )
                    ; CaseScope
                        ( Operator ("app", [ Var "fun"; Var "arg" ])
                        , CoreApp (dynamics @@ Var "fun", [ dynamics @@ Var "arg" ]) )
                    ; CaseScope
                        ( Operator ("lam", [ Var "scope" ])
                        , (* XXX should we have binding aware patterns? *)
                          Term (Operator
                            ("lambda", [ scope @@ Sequence []; scope @@ Var "scope" ]) ))
                    ; CaseScope
                        ( Operator ("ite" , [ Var "t1" ; Var "t2" ; Var "t3" ])
                        , Case
                            ( dynamics @@ Var "t1"
                            , [ CaseScope (Operator ("true", []), dynamics @@ Var "t2")
                              ; CaseScope
                                  (Operator ("false", []), dynamics @@ Var "t3")
                              ] ) )
                    ] ) ) ) )
    ]
;;

let%test "TermParser" = expect_parse dyn1 expected
