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
[[ ite(t1; t2; t3) ]] = case(
  [[ t1 ]]: bool;
  true() -> [[ t2 ]];
  false() -> [[ t3 ]]
)
  |}
  in
  let expected = DenotationChart
    [ DPatternTm ("true",  []), Operator ("true",  []);
      DPatternTm ("false", []), Operator ("false", []);
      DPatternTm ("val", [pat_scope @@ DVar (Some "v")]), Metavar "v";
      DPatternTm ("annot",
        [ pat_scope @@ DVar (Some "tm");
          pat_scope @@ DVar (Some "ty");
        ]),
        Meaning "tm";
      DPatternTm ("app",
        [ pat_scope @@ DVar (Some "fun");
          pat_scope @@ DVar (Some "arg");
        ]),
        CoreApp (Meaning "fun", [ Meaning "arg" ]);
      DPatternTm ("ite",
        [ pat_scope @@ DVar (Some "t1");
          pat_scope @@ DVar (Some "t2");
          pat_scope @@ DVar (Some "t3");
        ]),
        Case
          ( Meaning "t1"
          , SortAp ("bool", [||])
          , [ PatternTerm ("true", []),  core_scope @@ Meaning "t2";
              PatternTerm ("false", []), core_scope @@ Meaning "t3";
            ]
          );
    ]
  in
  expectParse dyn1 expected;
)
