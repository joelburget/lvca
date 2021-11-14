open Base
open Lvca_util

type t =
  { externals : (string * Kind.t) list
  ; sort_defs : (string * Sort_def.t) list
  }

module Unordered = struct
  type t =
    { externals : Kind.t String.Map.t
    ; sort_defs : Sort_def.t String.Map.t
    }
end

let mk_unordered { externals; sort_defs } =
  match String.Map.of_alist externals, String.Map.of_alist sort_defs with
  | `Ok externals, `Ok sort_defs -> `Ok Unordered.{ externals; sort_defs }
  | `Duplicate_key k, _ | _, `Duplicate_key k -> `Duplicate_key k
;;

let equivalent ?(info_eq = fun _ _ -> true) t1 t2 =
  let sort_defs_eq =
    List.equal (Tuple2.equal String.( = ) Sort_def.(equivalent ~info_eq))
  in
  let externals_eq = List.equal (Tuple2.equal String.( = ) Kind.(equivalent ~info_eq)) in
  externals_eq t1.externals t2.externals && sort_defs_eq t1.sort_defs t2.sort_defs
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

module Lookup_error = struct
  type t =
    | Sort_not_found of String.Set.t
    | Operator_not_found of String.Set.t

  open Fmt

  let pp ppf = function
    | Sort_not_found sort_names ->
      pf
        ppf
        "sort not found (options: {%a})"
        (list string ~sep:comma)
        (Set.to_list sort_names)
    | Operator_not_found op_names ->
      pf
        ppf
        "operator not found (options: {%a})"
        (list string ~sep:comma)
        (Set.to_list op_names)
  ;;
end

let lookup_operator { externals = _; sort_defs } sort_name op_name =
  let open Result.Let_syntax in
  let%bind (Sort_def (vars, operator_defs)) =
    match
      List.find_map sort_defs ~f:(fun (name, def) ->
          if String.(name = sort_name) then Some def else None)
    with
    | None ->
      let options = sort_defs |> List.map ~f:fst |> String.Set.of_list in
      Error (Lookup_error.Sort_not_found options)
    | Some it -> Ok it
  in
  let%map result =
    match
      List.find operator_defs ~f:(fun (Operator_def (_, op_def_name, _)) ->
          String.(op_def_name = op_name))
    with
    | None ->
      let options =
        operator_defs
        |> List.map ~f:(fun (Operator_def (_, op_def_name, _)) -> op_def_name)
        |> String.Set.of_list
      in
      Error (Lookup_error.Operator_not_found options)
    | Some opdef -> Ok opdef
  in
  vars, result
;;

let pp ppf { externals; sort_defs } =
  let sep ppf () = Stdlib.Format.pp_force_newline ppf () in
  let pp_externals =
    let pp_external = Fmt.(pair ~sep:(any " : ") string Kind.pp) in
    Fmt.(list pp_external ~sep)
  in
  let pp_sort_def ppf (name, sort_def) = Sort_def.pp ~name ppf sort_def in
  let sep ppf () =
    sep ppf ();
    sep ppf ()
  in
  Fmt.pf ppf "%a\n\n%a" pp_externals externals Fmt.(list pp_sort_def ~sep) sort_defs
;;

type kind_map = int String.Map.t
type kind_mismap = Int.Set.t String.Map.t

let kind_check { externals; sort_defs } =
  let env =
    externals
    |> List.map ~f:(fun (name, Kind (_, n)) -> name, n)
    |> String.Map.of_alist_exn
    |> Map.map ~f:Int.Set.singleton
  in
  let mismap =
    sort_defs
    |> List.fold ~init:env ~f:(fun env (sort_name, sort_def) ->
           Sort_def.kind_check env sort_name sort_def)
  in
  let fine_vars, mismapped_vars =
    mismap
    |> Map.to_alist
    |> List.partition_map ~f:(fun (name, mismap) ->
           match Set.length mismap with
           | 1 -> Either.First (name, Set.min_elt_exn mismap)
           | _ -> Either.Second (name, mismap))
  in
  match mismapped_vars with
  | [] -> Ok (String.Map.of_alist_exn fine_vars)
  | _ -> Error (String.Map.of_alist_exn mismapped_vars)
;;

let parse =
  let open Lvca_parsing in
  lift2
    (fun externals sort_defs -> { externals; sort_defs })
    (many Kind.Parse.decl)
    (many1 Sort_def.parse)
  <?> "abstract syntax"
;;

let%test_module _ =
  (module struct
    let tm_def =
      let tm = Sort.mk_Name "tm" in
      let tm_v = Valence.Valence ([], tm) in
      let integer = Sort.mk_Name "integer" in
      let integer_v = Valence.Valence ([], integer) in
      ( "tm"
      , Sort_def.Sort_def
          ( []
          , [ Operator_def.mk "Add" (Arity.mk [ tm_v; tm_v ])
            ; Operator_def.mk "Lit" (Arity.mk [ integer_v ])
            ] ) )
    ;;

    let ( = ) = equivalent

    let%test_unit _ =
      let parsed =
        Lvca_parsing.(parse_string_or_failwith (C_comment_parser.junk *> parse))
          {|
// comment
integer : *

// comment
tm :=
  | Add(tm; // comment
  tm)
  | Lit(integer)

// comment
empty :=
// comment
      |}
      in
      let expected =
        { externals = [ "integer", Kind.mk 1 ]
        ; sort_defs = [ tm_def; "empty", Sort_def ([], []) ]
        }
      in
      assert (parsed = expected)
    ;;

    let kind_check str =
      let lang =
        Lvca_parsing.(parse_string_or_failwith (C_comment_parser.junk *> parse) str)
      in
      match kind_check lang with
      | Ok map ->
        Stdio.printf "okay\n";
        map |> Map.iteri ~f:(fun ~key ~data -> Stdio.printf "%s: %d\n" key data)
      | Error mismap ->
        Stdio.printf "failed\n";
        Map.iteri mismap ~f:(fun ~key ~data ->
            data
            |> Set.to_list
            |> List.map ~f:Int.to_string
            |> String.concat ~sep:", "
            |> Stdio.printf "%s: %s\n" key)
    ;;

    let%expect_test _ =
      kind_check {|tm a := Foo(a)|};
      [%expect {|
        okay
        a: 0
        tm: 1 |}]
    ;;

    let%expect_test _ =
      kind_check {|tm a := Foo(a) | Bar(a b)|};
      [%expect {|
        failed
        a: 0, 1 |}]
    ;;

    let%expect_test _ =
      kind_check {|tm := Foo(a) | Bar(a b)|};
      [%expect {|
        failed
        a: 0, 1 |}]
    ;;
  end)
;;

let%test_module "Parser" =
  (module struct
    open Lvca_provenance

    let parse = Lvca_parsing.(parse_string_or_failwith (C_comment_parser.junk *> parse))
    let tm_sort = Sort.mk_Name "tm"
    let tm_valence = Valence.Valence ([], tm_sort)
    let ty_sort = Sort.mk_Name "ty"
    let ty_valence = Valence.Valence ([], ty_sort)
    let foo_sort = Sort.mk_Name "foo"
    let x_sort = Sort.mk_Name "x"
    let ( = ) = equivalent

    let%test _ =
      parse "bool := True() | False()"
      (*     0123456789012345678901234 *)
      = { externals = []
        ; sort_defs =
            [ ( "bool"
              , Sort_def
                  ( []
                  , [ Operator_def
                        ( Provenance.of_range (Opt_range.mk 8 14)
                        , "True"
                        , Arity (Provenance.of_range (Opt_range.mk 12 14), []) )
                    ; Operator_def
                        ( Provenance.of_range (Opt_range.mk 17 24)
                        , "False"
                        , Arity (Provenance.of_range (Opt_range.mk 22 24), []) )
                    ] ) )
            ]
        }
    ;;

    let%test _ =
      parse
        {|
      integer : *
      list : * -> *

      ty :=
        | Bool()
        | Arr(ty; ty)

      tm :=
        | App(tm; tm)
        | Lam(tm. tm)

      foo (x : *) :=
        | Foo(foo[x]. x; x. x)
        | Bar(x)
      |}
      =
      let externals = [ "integer", Kind.mk 1; "list", Kind.mk 2 ] in
      let sort_defs =
        [ ( "ty"
          , Sort_def.Sort_def
              ( []
              , [ Operator_def.mk "Bool" (Arity.mk [])
                ; Operator_def.mk "Arr" (Arity.mk [ ty_valence; ty_valence ])
                ] ) )
        ; ( "tm"
          , Sort_def
              ( []
              , [ Operator_def.mk "App" (Arity.mk [ tm_valence; tm_valence ])
                ; Operator_def.mk
                    "Lam"
                    (Arity.mk [ Valence ([ Sort_binding tm_sort ], tm_sort) ])
                ] ) )
        ; ( "foo"
          , Sort_def
              ( [ "x", Some (Kind.mk 1) ]
              , [ Operator_def.mk
                    "Foo"
                    (Arity.mk
                       [ Valence
                           ( [ Sort_pattern { pattern_sort = foo_sort; var_sort = x_sort }
                             ]
                           , x_sort )
                       ; Valence ([ Sort_binding x_sort ], x_sort)
                       ])
                ; Operator_def.mk "Bar" (Arity.mk [ Valence ([], x_sort) ])
                ] ) )
        ]
      in
      { externals; sort_defs }
    ;;

    let ( = ) = List.equal (Tuple2.equal String.( = ) Kind.( = ))

    let%test _ =
      let lang =
        parse {|
      integer : *
      list : * -> *

      foo := Foo()
      |}
      in
      lang.externals
      = [ "integer", Kind.Kind (Provenance.of_range (Opt_range.mk 17 18), 1)
        ; "list", Kind (Provenance.of_range (Opt_range.mk 32 38), 2)
        ]
    ;;

    let parse_print str = str |> parse |> Fmt.pr "%a\n" pp

    let%expect_test _ =
      parse_print
        {|
list : * -> *
integer : *
string : *

value :=
  | Unit()
  | Lit_int(integer)
  | Lit_str(string)

match_line :=
  | Match_line(value[value]. term)

term :=
  | Lambda(value. term)
  | Alt_lambda(term. term)
  | Match(list match_line)
  | Value(value)
   |};
      [%expect
        {|
list : * -> *
integer : *
string : *

value :=
  | Unit()
  | Lit_int(integer)
  | Lit_str(string)

match_line := Match_line(value[value]. term)

term :=
  | Lambda(value. term)
  | Alt_lambda(term. term)
  | Match(list match_line)
  | Value(value)
   |}]
    ;;

    let%expect_test _ =
      parse_print "empty :=";
      [%expect "empty :="]
    ;;
  end)
;;
