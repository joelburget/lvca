open Binding
open Core.Types
open AbstractSyntax

let%test_module "Core.Types parsing" = (module struct
  let one = Bigint.of_int 1
  let scope : Nominal.term -> Nominal.scope
    = fun body -> Scope ([], body)

  let to_ast : term -> Nominal.term = function
    | Term tm -> tm
    | (Lambda _ | Let _ | CoreApp _ | Case _) -> failwith "to_ast: not a term!"

  let dynamics_str =
    {|
  meaning : arrow(ty(); val()) = \(tm : ty()) -> match tm with {
    | true() -> true()
    | false() -> false()
    | ite(t1; t2; t3) -> match meaning t1 with {
      | true()  -> meaning t2
      | false() -> meaning t3
    }
    | ap(f; arg) -> (meaning f) (meaning arg)
    | fun(scope) -> lambda([]; scope) // TODO: add type
  };
  |}
  ;;

  let var name = Term (Var name)
  let meaning x = CoreApp (var "meaning", x)

  let ty = SortAp ("ty", [||])

  let dynamics =
    CoreModule
      [ { name = "meaning"
        ; ty = SortAp ("arrow", [| ty; SortAp ("val", [||]) |])
        ; defn = Lambda
            ( ty
            , Scope
                ( "tm"
                , Case
                    ( var "tm"
                    , [ CaseScope (Operator ("true", []),
                        Term (Operator ("true", [])))
                      ; CaseScope (Operator ("false", []),
                        Term (Operator ("false", [])))
                      ; CaseScope
                          ( Operator ("ite" , [ Var "t1" ; Var "t2" ; Var "t3" ])
                          , Case
                              ( CoreApp (var "meaning", var "t1")
                              , [ CaseScope (Operator ("true", []), meaning (var "t2"))
                                ; CaseScope (Operator ("false", []), meaning (var "t3"))
                                ] ) )
                      ; CaseScope
                          ( Operator ("ap", [ Var "f"; Var "arg" ])
                          , CoreApp (meaning @@ var "f", meaning @@ var "arg") )
                      ; CaseScope
                          ( Operator ("fun", [ Var "scope" ])
                          , Term (Operator
                              ("lambda", [ scope @@ Sequence []; scope @@ Var "scope" ])) )
                      ] ) ) )
        }
      ]

  let%test "dynamics as expected" = match Parsing.CoreModule.parse dynamics_str with
    | Error err -> print_string (ParseError.to_string err); false
    | Ok dyn -> dyn = dynamics

  let%test "to_ast 1" =
    to_ast (Term (Primitive (PrimInteger one))) = Nominal.Primitive (PrimInteger one)
  ;;

  let%test "to_ast 2" =
    to_ast (Term (Operator ("foo", [ scope @@ Primitive (PrimInteger one) ])))
    = Nominal.Operator ("foo", [ Scope ([], Primitive (PrimInteger one)) ])
  ;;
end)
;;

let%test_module "Core.Types eval" =
  (module struct
    let eval_str = fun str -> print_string (match Parsing.CoreTerm.parse str with
      | Error err -> ParseError.to_string err
      | Ok core -> (match eval core with
        | Error (msg, tm) -> msg ^ ": " ^ pp_core_str tm
        | Ok result -> Nominal.pp_term' result))

    let%expect_test _ = eval_str "1"; [%expect{| 1 |}]
    let%expect_test _ = eval_str "foo(1)"; [%expect{| foo(1) |}]
    let%expect_test _ = eval_str "true()"; [%expect{| true() |}]
    let%expect_test _ = eval_str "false()"; [%expect{| false() |}]
    let%expect_test _ = eval_str
      {| match true() with {
           | true() -> false()
           | false() -> true()
         }
      |};
      [%expect{| false() |}]
    let%expect_test _ =
      eval_str {|(\(x: bool()) -> x) true()|};
      [%expect{| true() |}]
    let%expect_test _ = eval_str "#add(1; 2)"; [%expect{| 3 |}]
    let%expect_test _ = eval_str "#sub(1; 2)"; [%expect{| -1 |}]
    (* let%expect_test _ = eval_str "#sub 1 2"; [%expect{| -1 |}] *)
  end)
;;

let%test_module "Core.Types pretty" =
  (module struct
    let pretty width str = print_string (match Parsing.CoreTerm.parse str with
      | Error err -> ParseError.to_string err
      | Ok core ->
          let fmt = Format.str_formatter in
          Format.pp_set_geometry fmt ~max_indent:width ~margin:(width + 1);
          pp_core fmt core;
          Format.flush_str_formatter ())

    let%expect_test _ =
      pretty 20 "match true() with { true() -> false() | false() -> true() }";
      [%expect{|
        match true() with {
          | true()
            -> false()
          | false()
            -> true()
        } |}]

    let%expect_test _ =
      pretty 21 "match true() with { true() -> false() | false() -> true() }";
      [%expect{|
        match true() with {
          | true() -> false()
          | false() -> true()
        } |}]

    let%expect_test _ =
      pretty 23 "match x with { _ -> 1 }";
      [%expect{| match x with { _ -> 1 } |}]

    let%expect_test _ =
      pretty 22 "match x with { _ -> 1 }";
      [%expect{|
        match x with {
          | _ -> 1
        } |}]

    let%expect_test _ =
      pretty 20 "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]";
      [%expect{|
        [1, 2, 3, 4, 5, 6,
         7, 8, 9, 10] |}]

    let%expect_test _ =
      pretty 20 "foo a b c d e f g h i j k l";
      [%expect{|
        foo a b c d e f g h
            i j k l |}]

    let%expect_test _ =
      pretty 20 "f a b c d e f g h i j k l";
      [%expect{|
        f a b c d e f g h i
          j k l |}]

    let%expect_test _ =
      pretty 20 "let x = true() in not x";
      [%expect{|
        let x = true() in
        not x |}]
  end)
;;

let%test_module "Core.Types eval in dynamics" =
  (module struct
    let eval_in = fun dynamics_str str ->
      print_string (match Parsing.CoreModule.parse dynamics_str with
      | Error err -> ParseError.to_string err
      | Ok dynamics -> (match dynamics with
        | CoreModule [ { name = _; ty = _; defn } ] ->
          (match Parsing.CoreTerm.parse str with
            | Error err -> ParseError.to_string err
            | Ok core -> (match eval (CoreApp (defn, core)) with
              | Error (msg, tm) -> msg ^ ": " ^ pp_core_str tm
              | Ok result -> Nominal.pp_term' result))
        | _ -> "dynamics must consist of a single definition"))

    let dynamics_str =
      {|
meaning : arrow(ty(); val()) = \(tm : ty()) -> match tm with {
  | true() -> true()
  | false() -> false()
  | ite(t1; t2; t3) -> match meaning t1 with {
    | true()  -> meaning t2
    | false() -> meaning t3
  }
  | ap(f; arg) -> (meaning f) (meaning arg)
  | fun(scope) -> lambda([]; scope) // TODO: add type
};
      |}

    let%expect_test _ =
      eval_in dynamics_str "true()";
      [%expect{| true() |}]

    let id_dynamics = {|meaning : arrow(ty(); val()) = \(tm : ty()) -> tm;|}

    let%expect_test _ =
      eval_in id_dynamics "true()";
      [%expect{| true() |}]

    let%expect_test _ =
      eval_in id_dynamics "lambda(tm. tm; [ty()])";
      [%expect{| lambda(tm. tm; [ty()]) |}]
  end)
;;
