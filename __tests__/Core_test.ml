open Jest
open Expect
open Binding
open Core
open Types

let _ = describe "Core" (fun () ->
  let module P_dyn = Parsing.Incremental(Parsing.Parseable_dynamics) in
  let one = Bigint.of_int 1 in
  let sort = SortAp ("bool", [||]) in
  let pat_scope body : BindingAwarePattern.scope = Scope ([], body) in
  let core_scope body = Scope ([], body) in
  let scope body = Core.Scope ([], body) in

  let dynamics_str = {|
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
  in

  let meaning x = CoreApp (Var "meaning", [x]) in

  let dynamics = DenotationChart
    [ "meaning"
    , Lambda
        ( [SortAp ("ty", [||])]
        , Scope
          ( [Var "tm"]
          , Case
            ( Var "tm"
            , [
                CaseScope ([Operator ("true",  [])], Operator ("true",  []));
                CaseScope ([Operator ("false", [])], Operator ("false", []));
                CaseScope (
                  [ Operator ("ite",
                    [ pat_scope @@ Var "t1";
                      pat_scope @@ Var "t2";
                      pat_scope @@ Var "t3";
                    ])
                  ],
                  Case
                    ( CoreApp (Var "meaning", [Var "t1"])
                    , [ CaseScope
                          ( [Operator ("true", [])], meaning (Var "t2"));
                        CaseScope
                          ( [Operator ("false", [])], meaning (Var "t3"));
                      ]
                    )
                );
                CaseScope (
                  [ Operator ("ap",
                    [ pat_scope @@ Var "f";
                      pat_scope @@ Var "arg";
                    ])
                  ],
                  CoreApp (meaning @@ Var "f", [meaning @@ Var "arg"])
                );
                CaseScope (
                  [ Operator ("fun", [ pat_scope @@ Var "scope" ]) ],
                  Operator ("lambda", [scope @@ Sequence []; scope @@ Var "scope"])
                );
              ]
            )
          )
        )
    ]

  in
  let dynamics' = P_dyn.parse dynamics_str in

  test "dynamics as expected" (fun () ->
    expect dynamics' |> toEqual (Result.Ok dynamics);
  );

  (*
  let lit_dynamics_str = {|
meaning = \(tm : ty()) -> match tm with {
  | lit(b) -> b
  | ite(t; l; r) -> case(meaning t; [
    branch(true();  meaning l),
    branch(false(); meaning r),
  ])
}
  |}
  in

  let lit_dynamics = P_dyn.parse lit_dynamics_str in
  *)

  (*
  let true_tm   = DeBruijn.Operator ("true", []) in
  let false_tm  = DeBruijn.Operator ("false", []) in
  *)

  let true_val  = Operator ("true", []) in
  let false_val = Operator ("false", []) in

  (*
  let ite_tm = DeBruijn.Operator ("ite",
    [ scope true_tm;
      scope false_tm;
      scope true_tm;
    ])
  in
  *)

  let ite_val = Case
    ( true_val
    , [ CaseScope ([Operator ("true", [])], false_val);
        CaseScope ([Operator ("false", [])], true_val);
      ]
    )
  in

  (*
  let fun_tm = DeBruijn.Operator ("ap",
    [ scope @@ Operator ("fun", [ Scope ([Var "x"], Var (0, 0)) ]);
      scope true_tm;
    ])
  in
  *)

  let fun_val = CoreApp
    ( Lambda ([sort], Scope ([Var "x"], Var "x"))
    , [ true_val ]
    )
  in

  let binary_int_op op a b = Operator (op,
    [ Scope ([], Primitive (PrimInteger (Bigint.of_int a)));
      Scope ([], Primitive (PrimInteger (Bigint.of_int b)));
    ])
  in

  testAll "to_ast"
    [ expect (to_ast (Primitive (PrimInteger one)))
      |> toEqual (Nominal.Primitive (PrimInteger one));

      (*
      expect (to_ast (Lambda ([sort; sort], Scope (["x"; "y"], Var "x"))))
      |> toEqual (Nominal.Operator
        ( "lam"
        , [Nominal.Scope ([Var "x"; Var "y"], Var "x")]
        ));
        *)

      expect (to_ast (Operator
        ( "foo"
        , [core_scope @@ Primitive (PrimInteger one)]
        )))
      |> toEqual (Nominal.Operator
        ( "foo"
        , [Scope ([], Primitive (PrimInteger one))]
        ))
    ]
    Util.id;

  let open Result in

  testAll "eval"
    [ expect (eval true_val)
      |> toEqual (Ok true_val);
      expect (eval false_val)
      |> toEqual (Ok false_val);
      expect (eval ite_val)
      |> toEqual (Ok false_val);
      expect (eval fun_val)
      |> toEqual (Ok true_val);

      expect (eval (binary_int_op "#add" 1 2))
      |> toEqual (Ok (Primitive (PrimInteger (Bigint.of_int 3))));
      expect (eval (binary_int_op "#sub" 1 2))
      |> toEqual (Ok (Primitive (PrimInteger (Bigint.of_int (-1)))));
    ]
    Util.id;
)
