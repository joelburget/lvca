open Jest
open Expect
open Core
open Binding

let _ = describe "TermParser" (fun () ->
  let module P_dyn = Parsing.Incremental(Parsing.Parseable_dynamics) in

  let expectParse str tm = test ("'" ^ str ^ "'") (fun () ->
    let parsed = P_dyn.parse str |. Result.map produce_denotation_chart in
    expect parsed |> toEqual (Result.Ok tm)
  ) in
  let pat_scope body : denotation_pat_scope = Scope ([], body) in
  let core_scope body = CoreScope ([], body) in

  let dyn1 = {|
[[ true()          ]] = true()
[[ false()         ]] = false()
[[ val(v)          ]] = v
[[ annot(tm; ty)   ]] = [[ tm ]]
[[ app(fun; arg)   ]] = app([[ fun ]]; [[ arg ]])
[[ lam(x. body)    ]] = fun([bool()]; x. [[ body ]])
[[ ite(t1; t2; t3) ]] = case([[ t1 ]]; [
  true(). [[ t2 ]],
  false(). [[ t3 ]],
])
  |}
  in

  let expected = DenotationChart
    [ Operator ("true",  []), Operator ("true",  []);
      Operator ("false", []), Operator ("false", []);
      Operator ("val", [pat_scope @@ Var "v"]), Metavar "v";
      Operator ("annot",
        [ pat_scope @@ Var "tm";
          pat_scope @@ Var "ty";
        ]),
        Meaning "tm";
      Operator ("app",
        [ pat_scope @@ Var "fun";
          pat_scope @@ Var "arg";
        ]),
        CoreApp (Meaning "fun", [ Meaning "arg" ]);
      Operator ("lam", [ Scope (["x"], Var "body") ]),
        Lambda ([SortAp ("bool", [||])], CoreScope ([Var "x"], Meaning "body"));
      Operator ("ite",
        [ pat_scope @@ Var "t1";
          pat_scope @@ Var "t2";
          pat_scope @@ Var "t3";
        ]),
        Case
          ( Meaning "t1"
          , [ CoreScope ([Operator ("true", [])], Meaning "t2");
              CoreScope ([Operator ("false", [])], Meaning "t3");
            ]
          );
    ]
  in
  expectParse dyn1 expected;

  let metavar_test = produce_denotation_chart @@
    Result.getExn @@
    P_dyn.parse "[[ lit(v) ]] = v"
  in
  let metavar_test_expected = DenotationChart
    [ Operator ("lit",  [pat_scope @@ Var "v"]),
      Metavar "v";
    ]
  in

  test "metavar fixing-up" (fun () ->
    expect metavar_test |> toEqual metavar_test_expected
  );
)
