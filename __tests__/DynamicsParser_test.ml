open Jest
open Expect
open Core
open Binding

let _ = describe "TermParser" (fun () ->
  let module P_dyn = Parsing.Incremental(Parsing.Parseable_dynamics) in

  let expectParse str tm = test ("'" ^ str ^ "'") (fun () ->
    let parsed = P_dyn.parse str in
    expect parsed |> toEqual (Result.Ok tm)
  ) in
  let pat_scope body : BindingAwarePattern.scope = Scope ([], body) in
  let dynamics x = CoreApp (Var "dynamics", [x]) in

  let dyn1 = {|
dynamics = \(tm : ty()) -> match tm with {
  | true()          -> true()
  | false()         -> false()
  | val(v)          -> v
  | annot(tm; ty)   -> dynamics tm
  | app(fun; arg)   -> (dynamics fun) (dynamics arg)
  | ite(t1; t2; t3) -> match dynamics t1 with {
    | true()  -> dynamics t2
    | false() -> dynamics t3
  }
}
  |}
  (* | lam(x. body)    -> \(x : bool()) -> dynamics body // XXX need to open body *)
  in

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
                 [ Operator ("lam", [ Scope (["x"], Var "body") ]) ],
                 Lambda ([SortAp ("bool", [||])], Scope ([Var "x"], Var "body"))
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
  in
  expectParse dyn1 expected;
)
