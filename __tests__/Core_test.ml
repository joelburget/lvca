open Jest
open Expect
open Binding
open Core
open Types

let _ = describe "Core" (fun () ->
  let one = Bigint.of_int 1 in
  let sort = SortAp ("bool", [||]) in
  let pat_scope body = DenotationScopePat ([], body) in
  let core_scope body = CoreScope ([], body) in
  let scope body = DeBruijn.Scope ([], body) in

  testAll "to_ast"
    [ expect (to_ast (Primitive (PrimInteger one)))
      |> toEqual (Nominal.Primitive (PrimInteger one));

      (*
      expect (to_ast (Lambda (["x"; "y"], Var "x")))
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

  let dynamics = DenotationChart
    [ DPatternTm ("true",  []), Operator ("true",  []);
      DPatternTm ("false", []), Operator ("false", []);

      DPatternTm ("ite",
      [ pat_scope @@ DVar (Some "t1");
        pat_scope @@ DVar (Some "t2");
        pat_scope @@ DVar (Some "t3");
      ]),
      Case
        ( Meaning "t1"
        , sort
        , [ PatternTerm ("true" , []), core_scope @@ Meaning "t2";
            PatternTerm ("false", []), core_scope @@ Meaning "t3";
          ]
        );

      DPatternTm ("ap",
        [ pat_scope @@ DVar (Some "f");
          pat_scope @@ DVar (Some "arg");
        ]),
      CoreApp (Meaning "f", [Meaning "arg"]);

      DPatternTm("fun",
        [ DenotationScopePat(["v"], DVar (Some "body"))
        ]),
      Lambda (CoreScope (["v"], Meaning "body"));
    ]
  in

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
    , sort
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
    ( Lambda (CoreScope (["x"], Var "x"))
    , [ true_val ]
    )
  in

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
    ]
    Util.id;
)
