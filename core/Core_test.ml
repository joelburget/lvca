open Lvca_syntax
open Binding
open Core

module ParseCore = Core.Parse(Lvca_util.Angstrom.CComment)

let parse_defn str =
  match
    Angstrom.parse_string ~consume:All ParseCore.defn str
  with
    | Error err -> failwith err
    | Ok dyn -> dyn

let parse_term str =
  match
    Angstrom.parse_string ~consume:All ParseCore.term str
  with
    | Error err -> failwith err
    | Ok dyn -> dyn

let%test_module "Core parsing" = (module struct
  open AbstractSyntax

  let scope : unit Nominal.term -> unit Nominal.scope
    = fun body -> Scope ([], [body])

  let dynamics_str =
    {|\(tm : ty()) -> match tm with {
    | true() -> {true()}
    | false() -> {false()}
    | ite(t1; t2; t3) -> match meaning t1 with {
      | true()  -> meaning t2
      | false() -> meaning t3
    }
    | ap(f; arg) -> (meaning f) (meaning arg)
    | fun(scope) -> {lambda(list(); scope)} // TODO: add type
  }
  |}
  ;;

  let p_var name = Pattern.Var ((), name)
  let t_var name = Term (Var ((), name))
  let p_operator tag children = Pattern.Operator ((), tag, children)
  let t_operator tag children = Nominal.Operator ((), tag, children)
  let meaning x = CoreApp (t_var "meaning", x)

  let ty = SortAp ("ty", [])

  let dynamics =
    Defn
      ( []
      , Lambda
        ( ty
        , Scope
            ( "tm"
            , Case
                ( t_var "tm"
                , [ CaseScope (p_operator "true" [],
                    Term (t_operator "true" []))
                  ; CaseScope (p_operator "false" [],
                    Term (t_operator "false" []))
                  ; CaseScope
                      ( p_operator "ite" [ [p_var "t1"] ; [p_var "t2"] ; [p_var "t3"] ]
                      , Case
                          ( CoreApp (t_var "meaning", t_var "t1")
                          , [ CaseScope (p_operator "true" [], meaning (t_var "t2"))
                            ; CaseScope (p_operator "false" [], meaning (t_var "t3"))
                            ] ) )
                  ; CaseScope
                      ( p_operator "ap" [ [p_var "f"]; [p_var "arg"] ]
                      , CoreApp (meaning @@ t_var "f", meaning @@ t_var "arg") )
                  ; CaseScope
                      ( p_operator "fun" [ [p_var "scope"] ]
                      , Term (t_operator
                          "lambda"
                          [ scope @@ t_operator "list" []
                          ; scope @@ Var ((), "scope")
                          ])
                      )
                  ] ) ) )
      )

  let%test "dynamics as expected" =
    Caml.(parse_defn dynamics_str |> erase_defn = dynamics)
end)
;;

let%test_module "Core eval" =
  (module struct
    let eval_str = fun str ->
      let core = parse_term str in
      let result = match eval core with
        | Error (msg, tm) -> msg ^ ": " ^ to_string tm
        | Ok result -> Nominal.pp_term_str result
      in
      print_string result

    let%expect_test _ = eval_str "{1}"; [%expect{| 1 |}]
    let%expect_test _ = eval_str "{foo(1)}"; [%expect{| foo(1) |}]
    let%expect_test _ = eval_str "{true()}"; [%expect{| true() |}]
    let%expect_test _ = eval_str "{false()}"; [%expect{| false() |}]
    let%expect_test _ = eval_str
      {|match {true()} with {
          | true() -> {false()}
          | false() -> {true()}
        }
      |};
      [%expect{| false() |}]
    let%expect_test _ =
      eval_str {|(\(x: bool()) -> x) {true()}|};
      [%expect{| true() |}]
    let%expect_test _ = eval_str "{add(1; 2)}"; [%expect{| 3 |}]
    let%expect_test _ = eval_str "{sub(1; 2)}"; [%expect{| -1 |}]
    (* let%expect_test _ = eval_str "sub 1 2"; [%expect{| -1 |}] *)
  end)
;;

let%test_module "Core pretty" =
  (module struct
    let pretty width str = print_string (match
        Angstrom.parse_string ~consume:All
          Angstrom.(Lvca_util.Angstrom.whitespace *> ParseCore.term)
          str
      with
      | Error err -> err
      | Ok core ->
          let fmt = Format.str_formatter in
          Format.pp_set_geometry fmt ~max_indent:width ~margin:(width + 1);
          pp fmt core;
          Format.flush_str_formatter ())

    let%expect_test _ =
      pretty 22 "match {true()} with { true() -> {false()} | false() -> {true()} }";
      [%expect{|
        match {true()} with {
          | true()
            -> {false()}
          | false()
            -> {true()}
        } |}]

    let%expect_test _ =
      pretty 23 "match {true()} with { true() -> {false()} | false() -> {true()} }";
      [%expect{|
        match {true()} with {
          | true() -> {false()}
          | false() -> {true()}
        } |}]

    let%expect_test _ =
      pretty 25 "match x with { _ -> {1} }";
      [%expect{| match x with { _ -> {1} } |}]

    let%expect_test _ =
      pretty 24 "match x with { _ -> {1} }";
      [%expect{|
        match x with {
          | _ -> {1}
        } |}]

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
      pretty 20 "let x = {true()} in not x";
      [%expect{|
        let x = {true()} in
        not x |}]
  end)
;;

let%test_module "Core eval in dynamics" =
  (module struct
    let eval_in = fun dynamics_str str ->
      let Defn (_imports, defn) = parse_defn dynamics_str in
      let core = parse_term str in
      match eval (CoreApp (defn, core)) with
        | Error (msg, tm) -> msg ^ ": " ^ to_string tm
        | Ok result -> Nominal.pp_term_str result

    let dynamics_str =
      {|\(tm : ty()) -> match tm with {
  | true() -> {true()}
  | false() -> {false()}
  | ite(t1; t2; t3) -> match meaning t1 with {
    | true()  -> meaning t2
    | false() -> meaning t3
  }
  | ap(f; arg) -> (meaning f) (meaning arg)
  | fun(scope) -> {lambda(list(); scope)} // TODO: add type
}
      |}

    let%expect_test _ =
      print_string @@ eval_in dynamics_str "{true()}";
      [%expect{| true() |}]

    let id_dynamics = {|\(tm : ty()) -> tm|}

    let%expect_test _ =
      print_string @@ eval_in id_dynamics "{true()}";
      [%expect{| true() |}]

    let%expect_test _ =
      print_string @@ eval_in id_dynamics "{lambda(tm. tm; list(ty()))}";
      [%expect{| lambda(tm. tm; list(ty())) |}]
  end)
;;
