open Dynamics.Core

let expect_parse str tm = match Parsing.Dynamics.parse str with
  | Error err -> print_string (ParseError.to_string err); false
  | Ok dyn ->
    if dyn = tm
    then true
    else (
      pp_module Format.std_formatter dyn;
      Format.print_newline ();
      pp_module Format.std_formatter tm;
      Format.print_newline ();
      false)

let var name = Term (Var name)
let dynamics x = CoreApp (var "dynamics", x)
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
  CoreModule
    [ ( "dynamics"
      , Lambda
          ( SortAp ("ty", [||])
          , Scope
              ( "tm"
              , Case
                  ( var "tm"
                  , [ CaseScope (Operator ("true", []), Term (Operator ("true", [])))
                    ; CaseScope (Operator ("false", []), Term (Operator ("false", [])))
                    ; CaseScope (Operator ("val", [ Var "v" ]), var "v")
                    ; CaseScope
                        ( Operator ("annot", [ Var "tm"; Var "ty" ])
                        , dynamics @@ var "tm" )
                    ; CaseScope
                        ( Operator ("app", [ Var "fun"; Var "arg" ])
                        , CoreApp (dynamics @@ var "fun", dynamics @@ var "arg") )
                    ; CaseScope
                        ( Operator ("lam", [ Var "scope" ])
                        , (* XXX should we have binding aware patterns? *)
                          Term (Operator
                            ("lambda", [ scope @@ Sequence []; scope @@ Var "scope" ]) ))
                    ; CaseScope
                        ( Operator ("ite" , [ Var "t1" ; Var "t2" ; Var "t3" ])
                        , Case
                            ( dynamics @@ var "t1"
                            , [ CaseScope (Operator ("true", []), dynamics @@ var "t2")
                              ; CaseScope
                                  (Operator ("false", []), dynamics @@ var "t3")
                              ] ) )
                    ] ) ) ) )
    ]
;;

let%test "TermParser" = expect_parse dyn1 expected
