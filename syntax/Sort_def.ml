open Base
open Lvca_util

type t = Sort_def of (string * Kind.t option) list * Operator_def.t list

let equivalent
    ?(info_eq = fun _ _ -> true)
    (Sort_def (vars1, ops1))
    (Sort_def (vars2, ops2))
  =
  List.equal
    (Tuple2.equal String.( = ) (Option.equal Kind.(equivalent ~info_eq)))
    vars1
    vars2
  && List.equal Operator_def.(equivalent ~info_eq) ops1 ops2
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let kind_check env sort_name (Sort_def (vars, operators)) =
  let update_env env name n =
    Map.update env name ~f:(function
        | None -> Int.Set.singleton n
        | Some set -> Set.add set n)
  in
  let env = update_env env sort_name (List.length vars) in
  List.fold operators ~init:env ~f:Operator_def.kind_check
;;

let pp ~name ppf (Sort_def (sort_vars, operator_defs)) =
  let open Fmt in
  let pp_sort_var ppf (name, kind_opt) =
    match kind_opt with
    | None -> string ppf name
    | Some kind -> pf ppf "(%s : %a)" name Kind.pp kind
  in
  let pp_sort_vars ppf vars =
    match vars with [] -> () | _ -> Fmt.pf ppf " %a" (list pp_sort_var) vars
  in
  match operator_defs with
  | [] -> pf ppf "%s%a :=" name pp_sort_vars sort_vars
  | [ single_def ] ->
    pf ppf "%s%a := @[%a@]" name pp_sort_vars sort_vars Operator_def.pp single_def
  | _ ->
    pf
      ppf
      "%s%a :=@,@[<v 2>  | %a@]"
      name
      pp_sort_vars
      sort_vars
      (list ~sep:(any "@,| ") Operator_def.pp)
      operator_defs
;;

let parse =
  let open Lvca_parsing in
  let module Ws = C_comment_parser in
  let bar = Ws.char '|' in
  let sort_var_decl =
    choice
      ~failure_msg:"looking for a (lower-case) identifier or parens"
      [ (Ws.lower_identifier Lvca_util.String.Set.empty >>| fun name -> name, None)
      ; (Ws.parens Kind.Parse.decl >>| fun (name, kind) -> name, Some kind)
      ]
    <?> "sort variable declaration"
  in
  lift4
    (fun name vars _assign op_defs -> name, Sort_def (vars, op_defs))
    (Ws.lower_identifier Lvca_util.String.Set.empty)
    (many sort_var_decl)
    (Ws.string ":=")
    (option '|' bar *> sep_by bar Operator_def.parse)
  <?> "sort definition"
;;

let%test_module _ =
  (module struct
    let test_parse =
      Lvca_parsing.(parse_string_or_failwith (C_comment_parser.junk *> parse))
    ;;

    let parse_print str =
      let pp ppf (name, sort_def) = pp ppf ~name sort_def in
      str |> test_parse |> Fmt.pr "%a\n" pp
    ;;

    let%expect_test _ =
      parse_print {|foo := Foo() // comment|};
      [%expect "foo := Foo()"]
    ;;

    let%expect_test _ =
      parse_print {|foo x := Foo()|};
      [%expect {|foo x := Foo()|}]
    ;;

    let%expect_test _ =
      parse_print
        {|
// comment
tm :=  // comment
  | Add(tm; tm)  // comment
  | Lit(integer)  // comment
    |};
      [%expect {|
    tm :=
      | Add(tm; tm)
      | Lit(integer)
    |}]
    ;;

    let%expect_test _ =
      let foo = Sort.mk_Name "foo" in
      let sort_def =
        Sort_def
          ( []
          , [ Operator_def.mk "Foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ])
            ; Operator_def.mk
                "Bar"
                (Arity.mk
                   [ Valence.Valence
                       ( [ Sort_pattern { pattern_sort = foo; var_sort = foo }
                         ; Sort_binding foo
                         ]
                       , foo )
                   ])
            ] )
      in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect
        {|
        foo :=
          | Foo(integer)
          | Bar(foo[foo]. foo. foo) |}]
    ;;

    let%expect_test _ =
      let sort_def =
        Sort_def
          ( []
          , [ Operator_def.mk "Foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ]) ]
          )
      in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect {| foo := Foo(integer) |}]
    ;;

    let%expect_test _ =
      let sort_def =
        Sort_def
          ( [ "a", None ]
          , [ Operator_def.mk "Foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ]) ]
          )
      in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect {| foo a := Foo(integer) |}]
    ;;

    let%expect_test _ =
      let sort_def = Sort_def ([ "a", Some (Kind.mk 2) ], []) in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect {| foo (a : * -> *) := |}]
    ;;
  end)
;;
