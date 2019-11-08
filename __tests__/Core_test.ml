open Jest
open Expect
open Binding
open Core
open Types

let _ = describe "Core" (fun () ->
  let module P_dyn = Parsing.Incremental(Parsing.Parseable_dynamics) in
  let one = Bigint.of_int 1 in
  let sort = SortAp ("bool", [||]) in
  let pat_scope body = DenotationScopePat ([], body) in
  let core_scope body = CoreScope ([], body) in
  let scope body = DeBruijn.Scope ([], body) in

  let dynamics_str = {|
  [[ true() ]] = true()
  [[ false() ]] = false()
  [[ ite(t1; t2; t3) ]] = case [[ t1 ]] of {
    | true()  -> [[ t2 ]]
    | false() -> [[ t3 ]]
  }
  [[ ap(f; arg) ]] = app([[ f ]]; [[ arg ]])
  [[ fun(v. body) ]] = \(v : bool) -> [[ body ]]
  |}
  in

  let dynamics = DenotationChart
    [ DPatternTm ("true",  []), Operator ("true",  []);
      DPatternTm ("false", []), Operator ("false", []);

      DPatternTm ("ite",
      [ pat_scope @@ DVar "t1";
        pat_scope @@ DVar "t2";
        pat_scope @@ DVar "t3";
      ]),
      Case
        ( Meaning "t1"
        , [ PatternTerm ("true" , []), core_scope @@ Meaning "t2";
            PatternTerm ("false", []), core_scope @@ Meaning "t3";
          ]
        );

      DPatternTm ("ap",
        [ pat_scope @@ DVar "f";
          pat_scope @@ DVar "arg";
        ]),
      CoreApp (Meaning "f", [Meaning "arg"]);

      DPatternTm("fun",
        [ DenotationScopePat(["v"], DVar "body")
        ]),
      Lambda ([sort], CoreScope (["v"], Meaning "body"));
    ]
  in
  let dynamics' = P_dyn.parse dynamics_str in

  test "dynamics as expected" (fun () ->
    expect dynamics' |> toEqual (Result.Ok dynamics);
  );

  let lit_dynamics_str = {|
  [[ lit(b) ]] = b
  [[ ite(t; l; r) ]] = case [[ t ]] of {
    | true()  -> [[ l ]]
    | false() -> [[ r ]]
  }
  |}
  in

  let lit_dynamics = Result.getExn (P_dyn.parse lit_dynamics_str) in

  let true_tm   = DeBruijn.Operator ("true", []) in
  let false_tm  = DeBruijn.Operator ("false", []) in
  let true_val  = Operator ("true", []) in
  let false_val = Operator ("false", []) in
  let ite_tm = DeBruijn.Operator ("ite",
    [ scope true_tm;
      scope false_tm;
      scope true_tm;
    ])
  in
  let ite_val = Case
    ( true_val
    , [ PatternTerm ("true" , []), core_scope false_val;
        PatternTerm ("false", []), core_scope true_val;
      ]
    )
  in

  let fun_tm = DeBruijn.Operator ("ap",
    [ scope @@ Operator ("fun", [ Scope (["x"], Var 0) ]);
      scope true_tm;
    ])
  in

  let fun_val = CoreApp
    ( Lambda ([sort], CoreScope (["x"], Var "x"))
    , [ true_val ]
    )
  in

  let binary_int_op op a b = Operator (op,
    [ CoreScope ([], Primitive (PrimInteger (Bigint.of_int a)));
      CoreScope ([], Primitive (PrimInteger (Bigint.of_int b)));
    ])
  in

  testAll "to_ast"
    [ expect (to_ast (Primitive (PrimInteger one)))
      |> toEqual (Nominal.Primitive (PrimInteger one));

      (*
      expect (to_ast (Lambda ([sort; sort], CoreScope (["x"; "y"], Var "x"))))
      |> toEqual (Nominal.Operator
        ( "lam"
        , [Nominal.Scope (["x"; "y"], Var "x")]
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

  testAll "term_denotation"
    [ expect (term_denotation dynamics [] true_tm)
      |> toEqual (Ok true_val);
      expect (term_denotation dynamics [] false_tm)
      |> toEqual (Ok false_val);
      expect (term_denotation dynamics [] ite_tm)
      |> toEqual (Ok ite_val);
      expect (term_denotation dynamics [] fun_tm)
      |> toEqual (Ok fun_val);
      expect (term_denotation lit_dynamics []
        (DeBruijn.Operator ("lit", [ scope @@ true_tm ])))
      |> toEqual (Ok true_val);
    ]
    Util.id;

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
