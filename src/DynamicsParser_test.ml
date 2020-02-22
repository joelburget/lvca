open Core
module P_dyn = Parsing.Incremental(Parsing.Parseable_dynamics)

let expect_parse str tm = P_dyn.parse str = Ok tm
let pat_scope body : BindingAwarePattern.scope = Scope ([], body)
let dynamics x = CoreApp (Var "dynamics", [x])
let scope x = Scope ([], x)

let dyn1 = {|
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
}
|}

let expected = DenotationChart
  [ "dynamics"
  , Lambda
      ( [SortAp ("ty", [||])]
      , Scope
        ( [Var "tm"]
        , Case (Var "tm",
           [ CaseScope ([Operator ("true", [])], Operator ("true", []));
             CaseScope ([Operator ("false", [])], Operator ("false", []));
             CaseScope ([Operator ("val", [pat_scope @@ Var "v"])], Var "v");
             CaseScope (
               [ Operator ("annot",
                 [ pat_scope @@ Var "tm";
                   pat_scope @@ Var "ty";
                 ])
               ],
               dynamics @@ Var "tm"
             );
             CaseScope (
               [ Operator ("app",
                 [ pat_scope @@ Var "fun";
                   pat_scope @@ Var "arg";
                 ])
               ],
               CoreApp (dynamics @@ Var "fun", [ dynamics @@ Var "arg" ])
             );
             CaseScope (
               [ Operator ("lam", [ pat_scope @@ Var "scope" ]) ], (* XXX should we have binding aware patterns? *)
               Operator ("lambda", [scope @@ Sequence []; scope @@ Var "scope"])
             );
             CaseScope (
               [ Operator ("ite",
                   [ pat_scope @@ Var "t1";
                     pat_scope @@ Var "t2";
                     pat_scope @@ Var "t3";
                   ])
               ],
               Case
                 ( dynamics @@ Var "t1"
                 , [ CaseScope ([Operator ("true", [])], dynamics @@ Var "t2");
                     CaseScope ([Operator ("false", [])], dynamics @@ Var "t3");
                   ]
                 )
             );
           ])
        )
      )
  ]

let%test "TermParser" = expect_parse dyn1 expected
