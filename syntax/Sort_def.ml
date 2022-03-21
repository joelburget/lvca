open Base
open Lvca_util

type t = Sort_def of (string * Kind.t option) list * Operator_def.t list * string list

let equivalent
    ?(info_eq = fun _ _ -> true)
    (Sort_def (vars1, ops1, var_names1))
    (Sort_def (vars2, ops2, var_names2))
  =
  List.equal
    (Tuple2.equal String.( = ) (Option.equal Kind.(equivalent ~info_eq)))
    vars1
    vars2
  && List.equal Operator_def.(equivalent ~info_eq) ops1 ops2
  && List.equal String.( = ) var_names1 var_names2
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )
let binds_vars (Sort_def (_, _, vs)) = match vs with [] -> false | _ -> true

let find_operator_def (Sort_def (_, op_defs, _)) name =
  List.find op_defs ~f:(fun (Operator_def (_, name', _)) -> String.(name' = name))
;;

let kind_check env sort_name (Sort_def (vars, operators, _var_names)) =
  let update_env env name n =
    Map.update env name ~f:(function
        | None -> Int.Set.singleton n
        | Some set -> Set.add set n)
  in
  let env = update_env env sort_name (List.length vars) in
  List.fold operators ~init:env ~f:Operator_def.kind_check
;;

let pp ~name ppf (Sort_def (sort_vars, operator_defs, var_names)) =
  let open Fmt in
  let pp_sort_var ppf (name, kind_opt) =
    match kind_opt with
    | None -> string ppf name
    | Some kind -> pf ppf "(%s : %a)" name Kind.pp kind
  in
  let pp_sort_vars ppf vars =
    match vars with [] -> () | _ -> Fmt.pf ppf " %a" (list pp_sort_var) vars
  in
  let pp_var_names = hovbox (list string ~sep:comma) in
  match operator_defs, var_names with
  | [], [] -> pf ppf "%s%a := ;" name pp_sort_vars sort_vars
  | [], _ -> pf ppf "%s%a :=@;  \\ %a" name pp_sort_vars sort_vars pp_var_names var_names
  | [ single_def ], [] ->
    pf ppf "%s%a := @[%a@];" name pp_sort_vars sort_vars Operator_def.pp single_def
  | _, [] ->
    pf
      ppf
      "%s%a :=@,@[<v 2>  | %a@,;@]"
      name
      pp_sort_vars
      sort_vars
      (list ~sep:(any "@,| ") Operator_def.pp)
      operator_defs
  | _ ->
    pf
      ppf
      "%s%a :=@,@[<v 2>  | %a@,\\ %a@]"
      name
      pp_sort_vars
      sort_vars
      (list ~sep:(any "@,| ") Operator_def.pp)
      operator_defs
      pp_var_names
      var_names
;;

let parse' =
  let open Lvca_parsing.Parser in
  let open Construction in
  let bar = symbol "|" in
  let sort_var_decl =
    choice
      ~failure_msg:"looking for a (lower-case) identifier or parens"
      [ (lower_identifier >>| fun (_range, name) -> name, None)
      ; (parens Kind.Parse.decl >>| fun (_range, (name, kind)) -> name, Some kind)
      ]
    <?> "sort variable declaration"
  in
  let p =
    let+ vars = star sort_var_decl
    and+ _ = symbol ":="
    and+ op_defs =
      choice
        ~failure_msg:"Expected a set of operator definitions"
        [ bar *> sep_by bar Operator_def.parse; sep_by bar Operator_def.parse ]
    and+ var_names =
      choice
        ~failure_msg:{|Expected a `;` or `\` variables list|}
        [ symbol ";" *> return None
        ; (symbol "\\" *> sep_by (symbol ",") lower_identifier
          >>| fun idents -> idents |> List.map ~f:snd |> Option.some)
        ]
    in
    let var_names = match var_names with None -> [] | Some vs -> vs in
    Sort_def (vars, op_defs, var_names)
  in
  p <?> "sort definition (sans identifier)"
;;

let parse =
  let open Lvca_parsing.Parser in
  let open Construction in
  let p =
    let+ _, name = lower_identifier
    and+ sort_def = parse' in
    name, sort_def
  in
  p <?> "sort definition"
;;

let%test_module _ =
  (module struct
    let test_parse = Lvca_parsing.Parser.parse_string_or_failwith parse

    let parse_print str =
      let pp ppf (name, sort_def) = pp ppf ~name sort_def in
      Fmt.pr "%a@." pp (test_parse str)
    ;;

    let%expect_test _ =
      parse_print {|foo := Foo(); // comment|};
      [%expect "foo := Foo();"]
    ;;

    let%expect_test _ =
      parse_print {|foo x := Foo();|};
      [%expect {|foo x := Foo();|}]
    ;;

    let%expect_test _ =
      parse_print
        {|
// comment
tm :=  // comment
  | Add(tm; tm)  // comment
  | Lit(integer)  // comment
  \ x, y
    |};
      [%expect {|
    tm :=
      | Add(tm; tm)
      | Lit(integer)
      \ x, y
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
            ]
          , [ "x"; "y" ] )
      in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect
        {|
        foo :=
          | Foo(integer)
          | Bar(foo[foo]. foo. foo)
          \ x, y |}]
    ;;

    let%expect_test _ =
      let sort_def =
        Sort_def
          ( []
          , [ Operator_def.mk "Foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ]) ]
          , [] )
      in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect {| foo := Foo(integer); |}]
    ;;

    let%expect_test _ =
      let sort_def =
        Sort_def
          ( [ "a", None ]
          , [ Operator_def.mk "Foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ]) ]
          , [] )
      in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect {| foo a := Foo(integer); |}]
    ;;

    let%expect_test _ =
      let sort_def = Sort_def ([ "a", Some (Kind.mk 2) ], [], []) in
      Fmt.pr "%a" (pp ~name:"foo") sort_def;
      [%expect {| foo (a : * -> *) := ; |}]
    ;;
  end)
;;
