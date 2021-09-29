(** Types for representing languages *)

open Base
open Lvca_provenance
open Lvca_util

let test_parse_with p str =
  Lvca_parsing.(parse_string (whitespace *> p) str) |> Result.ok_or_failwith
;;

module Kind = struct
  type t = Kind of Provenance.t * int

  let mk ?(provenance = Provenance.of_here [%here]) n = Kind (provenance, n)

  let equivalent ?(info_eq = fun _ _ -> true) (Kind (i1, k1)) (Kind (i2, k2)) =
    info_eq i1 i2 && Int.(k1 = k2)
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
  let info (Kind (i, _)) = i

  let pp ppf (Kind (_info, k)) =
    (* TODO open_loc ppf info; *)
    Fmt.(list ~sep:(any " -> ") (any "*")) ppf (List.init k ~f:(Fn.const ()))
  ;;

  (* close_loc ppf info *)

  module Parse = struct
    open Lvca_parsing

    let t =
      sep_by1 (Ws.string "->") (Ws.char '*')
      >>~ (fun location stars -> Kind (`Parse_located location, List.length stars))
      <?> "kind"
    ;;

    let decl =
      lift3 (fun ident _colon kind -> ident, kind) Ws.identifier (Ws.char ':') t
      <?> "kind declaration"
    ;;

    let%test_module "parsing" =
      (module struct
        let pp_decl ppf (name, decl) = Fmt.pf ppf "%s: %a" name pp decl

        let%expect_test _ =
          let x = test_parse_with decl "foo: * -> *" in
          Fmt.pr "%a" pp_decl x;
          [%expect {|foo: * -> *|}]
        ;;

        (* TODO
        let%expect_test _ =
          let x =
            test_parse_with
              (many t)
              {|
              * -> *
              *
              |}
          in
          let pp ppf kind =
            Fmt.pf
              ppf
              "%a"
              pp
              kind
              Fmt.(option string)
              (kind |> info |> Commented.get_comment)
          in
          Fmt.(pr "%a" (list ~sep:cut pp) x);
          [%expect
            {|
            * -> *
            *
            |}]
        ;;
           *)

        let%expect_test _ =
          let x =
            test_parse_with
              (many decl)
              {|
            foo: * -> *
            bar: * -> *
            |}
          in
          Fmt.(pr "%a" (list pp_decl) x);
          [%expect {|
          foo: * -> *
          bar: * -> *
          |}]
        ;;
      end)
    ;;
  end
end

module Pattern_sort = struct
  type t =
    { pattern_sort : Sort.t
    ; var_sort : Sort.t
    }

  let equivalent ?(info_eq = fun _ _ -> true) ps1 ps2 =
    Sort.equivalent ~info_eq ps1.pattern_sort ps2.pattern_sort
    && Sort.equivalent ~info_eq ps1.var_sort ps2.var_sort
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let pp ppf { pattern_sort; var_sort } =
    match pattern_sort with
    | Sort.Name _ -> Fmt.pf ppf "%a[%a]" Sort.pp pattern_sort Sort.pp var_sort
    | _ -> Fmt.pf ppf "(%a)[%a]" Sort.pp pattern_sort Sort.pp var_sort
  ;;

  let instantiate env { pattern_sort; var_sort } =
    { pattern_sort = Sort.instantiate env pattern_sort
    ; var_sort = Sort.instantiate env var_sort
    }
  ;;
end

module Sort_slot = struct
  type t =
    | Sort_binding of Sort.t
    | Sort_pattern of Pattern_sort.t

  let equivalent ?(info_eq = fun _ _ -> true) slot1 slot2 =
    match slot1, slot2 with
    | Sort_binding s1, Sort_binding s2 -> Sort.equivalent ~info_eq s1 s2
    | Sort_pattern ps1, Sort_pattern ps2 -> Pattern_sort.equivalent ~info_eq ps1 ps2
    | _, _ -> false
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let pp ppf = function
    | Sort_binding sort -> Sort.pp ppf sort
    | Sort_pattern ps -> Pattern_sort.pp ppf ps
  ;;

  let instantiate env = function
    | Sort_binding s -> Sort_binding (Sort.instantiate env s)
    | Sort_pattern ps -> Sort_pattern (Pattern_sort.instantiate env ps)
  ;;

  let kind_check env = function
    | Sort_binding sort -> Sort.kind_check env sort
    | Sort_pattern { pattern_sort; var_sort } ->
      [ pattern_sort; var_sort ] |> List.fold ~init:env ~f:Sort.kind_check
  ;;

  let parse =
    let open Lvca_parsing in
    Sort.parse
    >>= fun sort ->
    choice
      [ (Ws.brackets Sort.parse
        >>| fun var_sort -> Sort_pattern { pattern_sort = sort; var_sort })
      ; return (Sort_binding sort)
      ]
    <?> "sort slot"
  ;;
end

module Valence = struct
  type t = Valence of Sort_slot.t list * Sort.t

  let equivalent
      ?(info_eq = fun _ _ -> true)
      (Valence (slots1, sort1))
      (Valence (slots2, sort2))
    =
    List.equal Sort_slot.(equivalent ~info_eq) slots1 slots2
    && Sort.(equivalent ~info_eq) sort1 sort2
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let pp ppf (Valence (binders, result)) =
    match binders with
    | [] -> Sort.pp ppf result
    | _ ->
      Fmt.pf ppf "%a. %a" Fmt.(list ~sep:(any ".@ ") Sort_slot.pp) binders Sort.pp result
  ;;

  let instantiate env = function
    | Valence (binding_sort_slots, body_sort) ->
      Valence
        ( List.map binding_sort_slots ~f:(Sort_slot.instantiate env)
        , Sort.instantiate env body_sort )
  ;;

  let kind_check env (Valence (binding_slots, value_sort)) =
    let env = binding_slots |> List.fold ~init:env ~f:Sort_slot.kind_check in
    Sort.kind_check env value_sort
  ;;

  let parse =
    let open Lvca_parsing in
    let t =
      sep_by1 (Ws.char '.') Sort_slot.parse
      >>= fun slots ->
      let binders, body_slot = List.unsnoc slots in
      match body_slot with
      | Sort_slot.Sort_binding body_sort -> return (Valence (binders, body_sort))
      | _ ->
        fail
          (Fmt.str
             "Expected a simple sort, instead found a pattern sort (%a)"
             Sort_slot.pp
             body_slot)
    in
    t <?> "valence"
  ;;
end

module Arity = struct
  type t = Arity of Provenance.t * Valence.t list

  let mk ?(provenance = Provenance.of_here [%here]) valences = Arity (provenance, valences)

  let pp ppf (Arity (_info, valences)) =
    (* TODO open_loc ppf info; *)
    Fmt.(parens (list ~sep:semi Valence.pp)) ppf valences
  ;;

  (* close_loc ppf info *)

  let equivalent
      ?(info_eq = fun _ _ -> true)
      (Arity (i1, valences1))
      (Arity (i2, valences2))
    =
    info_eq i1 i2 && List.equal Valence.(equivalent ~info_eq) valences1 valences2
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let instantiate env (Arity (info, valences)) =
    Arity (info, List.map ~f:(Valence.instantiate env) valences)
  ;;

  let parse =
    let open Lvca_parsing in
    Ws.parens (sep_by (Ws.char ';') Valence.parse)
    >>| (fun valences -> Arity (Provenance.of_here [%here], valences))
    <?> "arity"
  ;;

  let%test_module "parsing" =
    (module struct
      let none = `Empty
      let tm = Sort.Name (none, "tm")
      let tm_v = Valence.Valence ([], tm)
      let integer = Sort.Name (none, "integer")
      let integer_v = Valence.Valence ([], integer)

      let%test_unit _ =
        assert (test_parse_with parse "(integer)" = Arity (none, [ integer_v ]))
      ;;

      let%test_unit _ =
        assert (test_parse_with parse "(tm; tm)" = Arity (none, [ tm_v; tm_v ]))
      ;;

      let%test_unit _ =
        assert (
          test_parse_with parse "(tm. tm)"
          = Arity (none, [ Valence.Valence ([ Sort_binding tm ], tm) ]))
      ;;

      let%test_unit _ =
        assert (test_parse_with parse "(tm)" = Arity (none, [ Valence.Valence ([], tm) ]))
      ;;

      let%test_unit _ =
        assert (
          test_parse_with parse "(tm[tm]. tm)"
          = Arity
              ( none
              , [ Valence.Valence
                    ([ Sort_pattern { pattern_sort = tm; var_sort = tm } ], tm)
                ] ))
      ;;

      let expect_okay str =
        match Lvca_parsing.parse_string parse str with
        | Ok _ -> ()
        | Error msg -> Stdio.print_string msg
      ;;

      let%expect_test _ = expect_okay "(tm[tm]. tm[tm]. tm)"
      let%expect_test _ = expect_okay "((foo bar)[baz quux]. tm)"
      let%expect_test _ = expect_okay "((foo bar)[baz quux]. tm)"
      let%expect_test _ = expect_okay "((foo bar)[baz quux]. tm)"
      let%test_unit _ = assert (test_parse_with parse "()" = Arity (none, []))
      let%test_unit _ = assert (test_parse_with parse "()" = Arity (`Empty, []))
    end)
  ;;
end

module Operator_def = struct
  type t = Operator_def of Provenance.t * string * Arity.t

  let mk ?(provenance = Provenance.of_here [%here]) name arity =
    Operator_def (provenance, name, arity)
  ;;

  let equivalent
      ?(info_eq = fun _ _ -> true)
      (Operator_def (info1, name1, arity1))
      (Operator_def (info2, name2, arity2))
    =
    info_eq info1 info2
    && String.(name1 = name2)
    && Arity.equivalent ~info_eq arity1 arity2
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let kind_check env (Operator_def (_info, _name, Arity (_, valences))) =
    List.fold valences ~init:env ~f:Valence.kind_check
  ;;

  let pp ppf (Operator_def (_info, name, arity)) =
    (* TODO open_loc ppf info; *)
    Fmt.pf ppf "%s%a" name Arity.pp arity
  ;;

  (* close_loc ppf info *)

  let parse =
    let open Lvca_parsing in
    Ws.identifier
    >>= (fun ident ->
          Arity.parse
          >>~ fun location arity -> Operator_def (`Parse_located location, ident, arity))
    <?> "operator definition"
  ;;

  let%test_module "parsing" =
    (module struct
      let%test_unit _ =
        let info1 = `Parse_located (Opt_range.mk 0 5) in
        let info2 = `Parse_located (Opt_range.mk 3 5) in
        let parsed = test_parse_with parse "foo()" in
        assert (parsed = Operator_def (info1, "foo", Arity (info2, [])))
      ;;
    end)
  ;;
end

module Sort_def = struct
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
    let bar = Ws.char '|' in
    let sort_var_decl =
      choice
        ~failure_msg:"looking for an identifier or parens"
        [ (Ws.identifier >>| fun name -> name, None)
        ; (Ws.parens Kind.Parse.decl >>| fun (name, kind) -> name, Some kind)
        ]
      <?> "sort variable declaration"
    in
    lift4
      (fun name vars _assign op_defs -> name, Sort_def (vars, op_defs))
      Ws.identifier
      (many sort_var_decl)
      (Ws.string ":=")
      (option '|' bar *> sep_by bar Operator_def.parse)
    <?> "sort definition"
  ;;

  let%test_module _ =
    (module struct
      let test_parse = test_parse_with parse

      let parse_print str =
        let pp ppf (name, sort_def) = pp ppf ~name sort_def in
        str |> test_parse |> Fmt.pr "%a\n" pp
      ;;

      let%expect_test _ =
        parse_print {|foo := foo()|};
        [%expect "foo := foo()"]
      ;;

      let%expect_test _ =
        parse_print {|foo x := foo()|};
        [%expect {|foo x := foo()|}]
      ;;

      let%expect_test _ =
        parse_print {|tm :=
  | add(tm; tm)
  | lit(integer)
    |};
        [%expect {|
    tm :=
      | add(tm; tm)
      | lit(integer)
    |}]
      ;;

      let%expect_test _ =
        let foo = Sort.mk_Name "foo" in
        let sort_def =
          Sort_def
            ( []
            , [ Operator_def.mk "foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ])
              ; Operator_def.mk
                  "bar"
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
          | foo(integer)
          | bar(foo[foo]. foo. foo) |}]
      ;;

      let%expect_test _ =
        let sort_def =
          Sort_def
            ( []
            , [ Operator_def.mk "foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ])
              ] )
        in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo := foo(integer) |}]
      ;;

      let%expect_test _ =
        let sort_def =
          Sort_def
            ( [ "a", None ]
            , [ Operator_def.mk "foo" (Arity.mk [ Valence ([], Sort.mk_Name "integer") ])
              ] )
        in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo a := foo(integer) |}]
      ;;

      let%expect_test _ =
        let sort_def = Sort_def ([ "a", Some (Kind.mk 2) ], []) in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo (a : * -> *) := |}]
      ;;
    end)
  ;;
end

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

let lookup_operator { externals = _; sort_defs } sort_name op_name =
  let open Option.Let_syntax in
  let%bind (Sort_def (vars, operator_defs)) =
    List.find_map sort_defs ~f:(fun (name, def) ->
        if String.(name = sort_name) then Some def else None)
  in
  let%map result =
    List.find operator_defs ~f:(fun (Operator_def (_, op_def_name, _)) ->
        String.(op_def_name = op_name))
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
          , [ Operator_def.mk "add" (Arity.mk [ tm_v; tm_v ])
            ; Operator_def.mk "lit" (Arity.mk [ integer_v ])
            ] ) )
    ;;

    let%test_unit _ =
      let parsed =
        test_parse_with
          parse
          {|
integer : *

tm :=
  | add(tm; tm)
  | lit(integer)

empty :=
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
      let lang = test_parse_with parse str in
      match kind_check lang with
      | Ok map ->
        Stdio.printf "okay\n";
        map |> Map.iteri ~f:(fun ~key ~data -> Stdio.printf "%s: %d\n" key data)
      | Error mismap ->
        Stdio.printf "failed\n";
        mismap
        |> Map.iteri ~f:(fun ~key ~data ->
               let inferred_kinds_str =
                 data
                 |> Set.to_list
                 |> List.map ~f:Int.to_string
                 |> String.concat ~sep:", "
               in
               Stdio.printf "%s: %s\n" key inferred_kinds_str)
    ;;

    let%expect_test _ =
      kind_check {|tm a := foo(a)|};
      [%expect {|
        okay
        a: 0
        tm: 1 |}]
    ;;

    let%expect_test _ =
      kind_check {|tm a := foo(a) | bar(a b)|};
      [%expect {|
        failed
        a: 0, 1 |}]
    ;;

    let%expect_test _ =
      kind_check {|tm := foo(a) | bar(a b)|};
      [%expect {|
        failed
        a: 0, 1 |}]
    ;;
  end)
;;

let%test_module "Parser" =
  (module struct
    open Lvca_provenance

    let parse = test_parse_with parse
    let tm_sort = Sort.mk_Name "tm"
    let tm_valence = Valence.Valence ([], tm_sort)
    let ty_sort = Sort.mk_Name "ty"
    let ty_valence = Valence.Valence ([], ty_sort)
    let foo_sort = Sort.mk_Name "foo"
    let x_sort = Sort.mk_Name "x"

    let%test _ =
      parse "bool := true() | false()"
      (*     0123456789012345678901234 *)
      = { externals = []
        ; sort_defs =
            [ ( "bool"
              , Sort_def
                  ( []
                  , [ Operator_def
                        ( `Parse_located (Opt_range.mk 8 14)
                        , "true"
                        , Arity (`Parse_located (Opt_range.mk 12 14), []) )
                    ; Operator_def
                        ( `Parse_located (Opt_range.mk 17 24)
                        , "false"
                        , Arity (`Parse_located (Opt_range.mk 22 24), []) )
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
        | bool()
        | arr(ty; ty)

      tm :=
        | app(tm; tm)
        | lam(tm. tm)

      foo (x : *) :=
        | foo(foo[x]. x; x. x)
        | bar(x)
      |}
      =
      let externals = [ "integer", Kind.mk 1; "list", Kind.mk 2 ] in
      let sort_defs =
        [ ( "ty"
          , Sort_def.Sort_def
              ( []
              , [ Operator_def.mk "bool" (Arity.mk [])
                ; Operator_def.mk "arr" (Arity.mk [ ty_valence; ty_valence ])
                ] ) )
        ; ( "tm"
          , Sort_def
              ( []
              , [ Operator_def.mk "app" (Arity.mk [ tm_valence; tm_valence ])
                ; Operator_def.mk
                    "lam"
                    (Arity.mk [ Valence ([ Sort_binding tm_sort ], tm_sort) ])
                ] ) )
        ; ( "foo"
          , Sort_def
              ( [ "x", Some (Kind.mk 1) ]
              , [ Operator_def.mk
                    "foo"
                    (Arity.mk
                       [ Valence
                           ( [ Sort_pattern { pattern_sort = foo_sort; var_sort = x_sort }
                             ]
                           , x_sort )
                       ; Valence ([ Sort_binding x_sort ], x_sort)
                       ])
                ; Operator_def.mk "bar" (Arity.mk [ Valence ([], x_sort) ])
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
      = [ "integer", Kind.Kind (`Parse_located (Opt_range.mk 17 18), 1)
        ; "list", Kind (`Parse_located (Opt_range.mk 32 38), 2)
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
  | unit()
  | lit_int(integer)
  | lit_str(string)

match_line :=
  | match_line(value[value]. term)

term :=
  | lambda(value. term)
  | alt_lambda(term. term)
  | match(list match_line)
  | value(value)
   |};
      [%expect
        {|
list : * -> *
integer : *
string : *

value :=
  | unit()
  | lit_int(integer)
  | lit_str(string)

match_line := match_line(value[value]. term)

term :=
  | lambda(value. term)
  | alt_lambda(term. term)
  | match(list match_line)
  | value(value)
   |}]
    ;;

    let%expect_test _ =
      parse_print "empty :=";
      [%expect "empty :="]
    ;;
  end)
;;
