open Jest
open Expect
open Binding
open Core
open Types

let _ = describe "Core" (fun () ->
  let one = Bigint.of_int 1 in
  testAll "val_to_ast"
    [ expect (val_to_ast (PrimVal (PrimInteger one)))
      |> toEqual (Nominal.Primitive (PrimInteger one));

      expect (val_to_ast (LamVal (["x"; "y"], CoreVar "x")))
      |> toEqual (Nominal.Operator
        ( "lam"
        , [Nominal.Scope (["x"; "y"], Var "x")]
        ));

      expect (val_to_ast (OperatorVal
        ( "foo"
        , [PrimVal (PrimInteger one)]
        )))
      |> toEqual (Nominal.Operator
        ( "foo"
        , [Scope ([], Primitive (PrimInteger one))]
        ))
    ]
    Util.id;

  let sort = SortAp ("bool", [||]) in

  let dynamics = DenotationChart
    [ DPatternTm ("true",  []), CoreVal (OperatorVal ("true",  []));
      DPatternTm ("false", []), CoreVal (OperatorVal ("false", []));

      DPatternTm ("ite",
      [ DenotationScopePat ([], DVar (Some "t1"));
        DenotationScopePat ([], DVar (Some "t2"));
        DenotationScopePat ([], DVar (Some "t3"));
      ]),
      Case
        ( Meaning "t1"
        , sort
        , [ PatternTerm ("true" , []), Meaning "t2";
            PatternTerm ("false", []), Meaning "t3";
          ]
        )
    ]
  in

  let true_tm   = DeBruijn.Operator ("true", []) in
  let false_tm  = DeBruijn.Operator ("false", []) in
  let true_val  = CoreVal (OperatorVal ("true", [])) in
  let false_val = CoreVal (OperatorVal ("false", [])) in
  let ite_tm = DeBruijn.Operator ("ite",
    [ Scope ([], true_tm);
      Scope ([], false_tm);
      Scope ([], true_tm);
    ])
  in
  let ite_val = Case
    ( true_val
    , sort
    , [ PatternTerm ("true" , []), false_val;
        PatternTerm ("false", []), true_val;
      ]
    )
  in

  let open Result in

  testAll "term_to_core"
    [ expect (term_to_core dynamics true_tm)
      |> toEqual (Ok true_val);
      expect (term_to_core dynamics false_tm)
      |> toEqual (Ok false_val);
      expect (term_to_core dynamics ite_tm)
      |> toEqual (Ok ite_val);
    ]
    Util.id;

  testAll "eval"
    [ expect (eval true_val)
      |> toEqual (Ok (OperatorVal ("true", [])));
      expect (eval false_val)
      |> toEqual (Ok (OperatorVal ("false", [])));
      expect (eval ite_val)
      |> toEqual (Ok (OperatorVal ("false", [])));
    ]
    Util.id;
)
