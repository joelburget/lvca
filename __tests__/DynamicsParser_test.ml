open Jest
open Expect
open Core
open Binding

let _ = describe "TermParser" (fun () ->
  let module P_dyn = Parsing.Incremental(Parsing.Parseable_dynamics) in

  let expectParse str tm = test ("'" ^ str ^ "'") (fun () ->
    expect (P_dyn.parse str) |> toEqual (Result.Ok tm)
  ) in
  let pat_scope body = DenotationScopePat ([], body) in
  let core_scope body = CoreScope ([], body) in

  let dyn1 = {|
[[ true()          ]] = true()
[[ false()         ]] = false()
[[ val(v)          ]] = v
[[ annot(tm; ty)   ]] = [[ tm ]]
[[ app(fun; arg)   ]] = app([[ fun ]]; [[ arg ]])
[[ lam(x. body)    ]] = \(x : bool) -> [[ body ]]
[[ ite(t1; t2; t3) ]] = case [[ t1 ]] of {
  | true()  -> [[ t2 ]]
  | false() -> [[ t3 ]]
}
  |}
  in

  let expected = DenotationChart
    [ DPatternTm ("true",  []), Operator ("true",  []);
      DPatternTm ("false", []), Operator ("false", []);
      DPatternTm ("val", [pat_scope @@ DVar "v"]), Metavar "v";
      DPatternTm ("annot",
        [ pat_scope @@ DVar "tm";
          pat_scope @@ DVar "ty";
        ]),
        Meaning "tm";
      DPatternTm ("app",
        [ pat_scope @@ DVar "fun";
          pat_scope @@ DVar "arg";
        ]),
        CoreApp (Meaning "fun", [ Meaning "arg" ]);
      DPatternTm ("lam", [ DenotationScopePat (["x"], DVar "body") ]),
        Lambda ([SortAp ("bool", [||])], CoreScope (["x"], Meaning "body"));
      DPatternTm ("ite",
        [ pat_scope @@ DVar "t1";
          pat_scope @@ DVar "t2";
          pat_scope @@ DVar "t3";
        ]),
        Case
          ( Meaning "t1"
          , [ PatternTerm ("true", []),  core_scope @@ Meaning "t2";
              PatternTerm ("false", []), core_scope @@ Meaning "t3";
            ]
          );
    ]
  in
  expectParse dyn1 expected;

  let metavar_test = Result.getExn (P_dyn.parse "[[ lit(v) ]] = v") in
  let metavar_test_expected = DenotationChart
    [ DPatternTm ("lit",  [pat_scope @@ DVar "v"]),
      Metavar "v";
    ]
  in

  test "metavar fixing-up" (fun () ->
    expect metavar_test |> toEqual metavar_test_expected
  );
)
