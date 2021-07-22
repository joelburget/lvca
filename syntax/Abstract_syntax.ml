(** Types for representing languages *)

open Base
open Lvca_provenance
open Lvca_util

let test_parse_with p str =
  Lvca_parsing.(parse_string (whitespace *> p) str) |> Result.ok_or_failwith
;;

module Kind = struct
  type 'info t = Kind of 'info * int

  let equal ~info_eq (Kind (i1, k1)) (Kind (i2, k2)) = info_eq i1 i2 && Int.(k1 = k2)
  let info (Kind (i, _)) = i
  let map_info ~f (Kind (i, k)) = Kind (f i, k)

  let pp_generic ~open_loc ~close_loc ppf (Kind (info, k)) =
    open_loc ppf info;
    Fmt.(list ~sep:(any " -> ") (any "*")) ppf (List.init k ~f:(Fn.const ()));
    close_loc ppf info
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  module Parse = struct
    open Lvca_parsing

    let t ~comment =
      sep_by1 (Ws.string "->") (Ws.char '*')
      >>== (fun Parse_result.{ value = stars; range } ->
             option' comment
             >>|| fun { value = comment; _ } ->
             { value = Kind (Commented.{ range; comment }, List.length stars); range })
      <?> "kind"
    ;;

    let decl ~comment =
      lift3
        (fun ident _colon kind -> ident, kind)
        Ws.identifier
        (Ws.char ':')
        (t ~comment)
      <?> "kind declaration"
    ;;

    let%test_module "parsing" =
      (module struct
        let pp_decl ppf (name, decl) = Fmt.pf ppf "%s: %a" name pp decl

        let%expect_test _ =
          let x =
            test_parse_with
              (decl ~comment:Lvca_parsing.c_comment)
              "foo: * -> * // comment"
          in
          Fmt.pr "%a" pp_decl x;
          [%expect {|foo: * -> *|}]
        ;;

        let%expect_test _ =
          let x =
            test_parse_with
              (many (t ~comment:Lvca_parsing.c_comment))
              {|
              * -> * // comment 1
              * // comment 2
              |}
          in
          let pp ppf kind =
            Fmt.pf
              ppf
              "%a //%a"
              pp
              kind
              Fmt.(option string)
              (kind |> info |> Commented.get_comment)
          in
          Fmt.(pr "%a" (list ~sep:cut pp) x);
          [%expect
            {|
            * -> * // comment 1
            * // comment 2
            |}]
        ;;

        let%expect_test _ =
          let x =
            test_parse_with
              (many (decl ~comment:Lvca_parsing.c_comment))
              {|
            foo: * -> * // comment 1
            bar: * -> * // comment 2
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
  type 'info t =
    { pattern_sort : 'info Sort.t
    ; var_sort : 'info Sort.t
    }

  let equal ~info_eq ps1 ps2 =
    Sort.equal ~info_eq ps1.pattern_sort ps2.pattern_sort
    && Sort.equal ~info_eq ps1.var_sort ps2.var_sort
  ;;

  let map_info ~f { pattern_sort; var_sort } =
    { pattern_sort = Sort.map_info ~f pattern_sort; var_sort = Sort.map_info ~f var_sort }
  ;;

  let pp_generic ~open_loc ~close_loc ppf { pattern_sort; var_sort } =
    let sort_pp = Sort.pp_generic ~open_loc ~close_loc in
    match pattern_sort with
    | Sort.Name _ -> Fmt.pf ppf "%a[%a]" sort_pp pattern_sort sort_pp var_sort
    | _ -> Fmt.pf ppf "(%a)[%a]" sort_pp pattern_sort sort_pp var_sort
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  let instantiate env { pattern_sort; var_sort } =
    { pattern_sort = Sort.instantiate env pattern_sort
    ; var_sort = Sort.instantiate env var_sort
    }
  ;;
end

module Sort_slot = struct
  type 'info t =
    | Sort_binding of 'info Sort.t
    | Sort_pattern of 'info Pattern_sort.t

  let map_info ~f = function
    | Sort_binding s -> Sort_binding (Sort.map_info ~f s)
    | Sort_pattern { pattern_sort; var_sort } ->
      Sort_pattern
        { pattern_sort = Sort.map_info ~f pattern_sort
        ; var_sort = Sort.map_info ~f var_sort
        }
  ;;

  let equal ~info_eq slot1 slot2 =
    match slot1, slot2 with
    | Sort_binding s1, Sort_binding s2 -> Sort.equal ~info_eq s1 s2
    | Sort_pattern ps1, Sort_pattern ps2 -> Pattern_sort.equal ~info_eq ps1 ps2
    | _, _ -> false
  ;;

  let pp_generic ~open_loc ~close_loc ppf = function
    | Sort_binding sort -> Sort.pp_generic ~open_loc ~close_loc ppf sort
    | Sort_pattern ps -> Pattern_sort.pp_generic ~open_loc ~close_loc ppf ps
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  let instantiate env = function
    | Sort_binding s -> Sort_binding (Sort.instantiate env s)
    | Sort_pattern ps -> Sort_pattern (Pattern_sort.instantiate env ps)
  ;;

  let kind_check env = function
    | Sort_binding sort -> Sort.kind_check env sort
    | Sort_pattern { pattern_sort; var_sort } ->
      [ pattern_sort; var_sort ] |> List.fold ~init:env ~f:Sort.kind_check
  ;;

  let parse ~comment =
    let open Lvca_parsing in
    Sort.parse ~comment
    >>= fun sort ->
    choice
      [ (Ws.brackets (Sort.parse ~comment)
        >>| fun var_sort -> Sort_pattern { pattern_sort = sort; var_sort })
      ; return (Sort_binding sort)
      ]
    <?> "sort slot"
  ;;
end

module Valence = struct
  type 'info t = Valence of 'info Sort_slot.t list * 'info Sort.t

  let equal ~info_eq (Valence (slots1, sort1)) (Valence (slots2, sort2)) =
    List.equal (Sort_slot.equal ~info_eq) slots1 slots2 && Sort.equal ~info_eq sort1 sort2
  ;;

  let map_info ~f (Valence (slots, sort)) =
    Valence (List.map ~f:(Sort_slot.map_info ~f) slots, Sort.map_info ~f sort)
  ;;

  let pp_generic ~open_loc ~close_loc ppf (Valence (binders, result)) =
    let sort_pp = Sort.pp_generic ~open_loc ~close_loc in
    match binders with
    | [] -> sort_pp ppf result
    | _ ->
      Fmt.pf
        ppf
        "%a. %a"
        Fmt.(list ~sep:(any ".@ ") (Sort_slot.pp_generic ~open_loc ~close_loc))
        binders
        sort_pp
        result
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

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

  let parse ~comment =
    let open Lvca_parsing in
    let t =
      sep_by1 (Ws.char '.') (Sort_slot.parse ~comment)
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
  type 'info t = Arity of 'info * 'info Valence.t list

  let pp_generic ~open_loc ~close_loc ppf (Arity (info, valences)) =
    open_loc ppf info;
    Fmt.(parens (list ~sep:semi (Valence.pp_generic ~open_loc ~close_loc))) ppf valences;
    close_loc ppf info
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  let equal ~info_eq (Arity (i1, valences1)) (Arity (i2, valences2)) =
    info_eq i1 i2 && List.equal (Valence.equal ~info_eq) valences1 valences2
  ;;

  let map_info ~f (Arity (info, valences)) =
    Arity (f info, List.map ~f:(Valence.map_info ~f) valences)
  ;;

  let erase arity = map_info ~f:(Fn.const ()) arity

  let instantiate env (Arity (info, valences)) =
    Arity (info, List.map ~f:(Valence.instantiate env) valences)
  ;;

  let parse ~comment =
    let open Lvca_parsing in
    Ws.parens (sep_by (Ws.char ';') (Valence.parse ~comment))
    >>== (fun Parse_result.{ value = valences; range } ->
           option' comment
           >>|| fun { value = comment; range = comment_range } ->
           let range = Opt_range.union range comment_range in
           let value = Arity (Commented.{ range; comment }, valences) in
           Parse_result.{ value; range })
    <?> "arity"
  ;;

  let%test_module "parsing" =
    (module struct
      let none = Commented.none
      let tm = Sort.Name (none, "tm")
      let tm_v = Valence.Valence ([], tm)
      let integer = Sort.Name (none, "integer")
      let integer_v = Valence.Valence ([], integer)
      let ( = ) = equal ~info_eq:(fun _ _ -> true)
      let parse = parse ~comment:Lvca_parsing.no_comment

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
      let%expect_test _ = expect_okay "((foo bar)[baz quux]. tm)  // comment"
      let%test_unit _ = assert (test_parse_with parse "()" = Arity (none, []))
      (* let%test_unit _ = assert (test_parse_with parse "()  // comment" = []) *)
    end)
  ;;
end

module Operator_def = struct
  type 'info t = Operator_def of 'info * string * 'info Arity.t

  let equal
      ~info_eq
      (Operator_def (info1, name1, arity1))
      (Operator_def (info2, name2, arity2))
    =
    info_eq info1 info2 && String.(name1 = name2) && Arity.equal ~info_eq arity1 arity2
  ;;

  let map_info ~f (Operator_def (info, name, arity)) =
    Operator_def (f info, name, Arity.map_info ~f arity)
  ;;

  let kind_check env (Operator_def (_info, _name, Arity (_, valences))) =
    List.fold valences ~init:env ~f:Valence.kind_check
  ;;

  let pp_generic ~open_loc ~close_loc ppf (Operator_def (info, name, arity)) =
    open_loc ppf info;
    Fmt.pf ppf "%s%a" name (Arity.pp_generic ~open_loc ~close_loc) arity;
    close_loc ppf info
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  let parse ~comment =
    let open Lvca_parsing in
    Ws.identifier
    >>== (fun Parse_result.{ value = ident; range = ident_range } ->
           Arity.parse ~comment
           >>== fun Parse_result.{ value = arity; range = arity_range } ->
           option' comment
           >>|| fun { value = comment; range = comment_range } ->
           let range = Opt_range.list_range [ ident_range; arity_range; comment_range ] in
           let info = Commented.{ range; comment } in
           Parse_result.{ value = Operator_def (info, ident, arity); range })
    <?> "operator definition"
  ;;

  let%test_module "parsing" =
    (module struct
      let ( = ) = equal ~info_eq:(Commented.equal String.( = ))

      let%test_unit _ =
        let info1 = Commented.{ range = Opt_range.mk 0 5; comment = None } in
        let info2 = Commented.{ range = Opt_range.mk 3 5; comment = None } in
        let parsed = test_parse_with (parse ~comment:Lvca_parsing.no_comment) "foo()" in
        assert (parsed = Operator_def (info1, "foo", Arity (info2, [])))
      ;;

      (*
      let%test_unit _ =
        let info = Commented.{ range = Opt_range.mk 0 16; comment = Some " comment" } in
        let str = "foo() // comment" in
        (*         01234567890123456 *)
        assert (
          test_parse_with (parse ~comment:Lvca_parsing.no_comment) str
          = Operator_def (info, "foo", []))
      ;;
      *)
    end)
  ;;
end

module Sort_def = struct
  type 'info t =
    | Sort_def of (string * 'info Kind.t option) list * 'info Operator_def.t list

  let equal ~info_eq (Sort_def (vars1, ops1)) (Sort_def (vars2, ops2)) =
    List.equal
      (Tuple2.equal String.( = ) (Option.equal (Kind.equal ~info_eq)))
      vars1
      vars2
    && List.equal (Operator_def.equal ~info_eq) ops1 ops2
  ;;

  let map_info ~f (Sort_def (vars, op_defs)) =
    let vars =
      vars
      |> List.map ~f:(fun (name, kind_opt) ->
             match kind_opt with
             | None -> name, None
             | Some kind -> name, Some (Kind.map_info ~f kind))
    in
    let op_defs = op_defs |> List.map ~f:(Operator_def.map_info ~f) in
    Sort_def (vars, op_defs)
  ;;

  let erase sd = map_info ~f:(Fn.const ()) sd

  let kind_check env sort_name (Sort_def (vars, operators)) =
    let update_env env name n =
      Map.update env name ~f:(function
          | None -> Int.Set.singleton n
          | Some set -> Set.add set n)
    in
    let env = update_env env sort_name (List.length vars) in
    List.fold operators ~init:env ~f:Operator_def.kind_check
  ;;

  let pp_generic ~open_loc ~close_loc ~name ppf (Sort_def (sort_vars, operator_defs)) =
    let open Fmt in
    let pp_sort_var ppf (name, kind_opt) =
      match kind_opt with
      | None -> string ppf name
      | Some kind -> pf ppf "(%s : %a)" name (Kind.pp_generic ~open_loc ~close_loc) kind
    in
    let pp_sort_vars ppf vars =
      match vars with [] -> () | _ -> Fmt.pf ppf " %a" (list pp_sort_var) vars
    in
    let pp_op_def = Operator_def.pp_generic ~open_loc ~close_loc in
    match operator_defs with
    | [] -> pf ppf "%s%a :=" name pp_sort_vars sort_vars
    | [ single_def ] ->
      pf ppf "%s%a := @[%a@]" name pp_sort_vars sort_vars pp_op_def single_def
    | _ ->
      pf
        ppf
        "%s%a :=@,@[<v 2>  | %a@]"
        name
        pp_sort_vars
        sort_vars
        (list ~sep:(any "@,| ") pp_op_def)
        operator_defs
  ;;

  let pp ~name ppf t =
    pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ~name ppf t
  ;;

  let parse ~comment =
    let open Lvca_parsing in
    let assign = Ws.string ":=" in
    let bar = Ws.char '|' in
    let sort_var_decl =
      choice
        ~failure_msg:"looking for an identifier or parens"
        [ (Ws.identifier >>| fun name -> name, None)
        ; (Ws.parens (Kind.Parse.decl ~comment) >>| fun (name, kind) -> name, Some kind)
        ]
      <?> "sort variable declaration"
    in
    lift4
      (fun name vars _assign op_defs -> name, Sort_def (vars, op_defs))
      Ws.identifier
      (many sort_var_decl)
      assign
      (option '|' bar *> sep_by bar (Operator_def.parse ~comment))
    <?> "sort definition"
  ;;

  let%test_module _ =
    (module struct
      let parse = parse ~comment:Lvca_parsing.no_comment

      let test_parse str =
        test_parse_with parse str |> Tuple2.map2 ~f:(map_info ~f:Commented.get_range)
      ;;

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
        let foo = Sort.Name ((), "foo") in
        let sort_def =
          Sort_def
            ( []
            , [ Operator_def.Operator_def
                  ((), "foo", Arity ((), [ Valence ([], Sort.Name ((), "integer")) ]))
              ; Operator_def
                  ( ()
                  , "bar"
                  , Arity
                      ( ()
                      , [ Valence
                            ( [ Sort_pattern { pattern_sort = foo; var_sort = foo }
                              ; Sort_binding foo
                              ]
                            , foo )
                        ] ) )
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
            , [ Operator_def.Operator_def
                  ((), "foo", Arity ((), [ Valence ([], Sort.Name ((), "integer")) ]))
              ] )
        in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo := foo(integer) |}]
      ;;

      let%expect_test _ =
        let sort_def =
          Sort_def
            ( [ "a", None ]
            , [ Operator_def.Operator_def
                  ((), "foo", Arity ((), [ Valence ([], Sort.Name ((), "integer")) ]))
              ] )
        in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo a := foo(integer) |}]
      ;;

      let%expect_test _ =
        let sort_def = Sort_def ([ "a", Some (Kind ((), 2)) ], []) in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo (a : * -> *) := |}]
      ;;
    end)
  ;;
end

type 'info t =
  { externals : (string * 'info Kind.t) list
  ; sort_defs : (string * 'info Sort_def.t) list
  }

module Unordered = struct
  type 'info t =
    { externals : 'info Kind.t String.Map.t
    ; sort_defs : 'info Sort_def.t String.Map.t
    }
end

let mk_unordered { externals; sort_defs } =
  match String.Map.of_alist externals, String.Map.of_alist sort_defs with
  | `Ok externals, `Ok sort_defs -> `Ok Unordered.{ externals; sort_defs }
  | `Duplicate_key k, _ | _, `Duplicate_key k -> `Duplicate_key k
;;

let equal info_eq t1 t2 =
  let sort_defs_eq = List.equal (Tuple2.equal String.( = ) (Sort_def.equal ~info_eq)) in
  let externals_eq = List.equal (Tuple2.equal String.( = ) (Kind.equal ~info_eq)) in
  externals_eq t1.externals t2.externals && sort_defs_eq t1.sort_defs t2.sort_defs
;;

let map_info ~f { externals; sort_defs } =
  { externals = externals |> List.map ~f:(fun (name, kind) -> name, Kind.map_info ~f kind)
  ; sort_defs =
      List.map ~f:(fun (name, sort_def) -> name, Sort_def.map_info ~f sort_def) sort_defs
  }
;;

let erase_info t = map_info ~f:(Fn.const ()) t

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

let pp_generic ~open_loc ~close_loc ppf { externals; sort_defs } =
  let pp_externals =
    Fmt.(list (pair ~sep:(any " : ") string (Kind.pp_generic ~open_loc ~close_loc)))
  in
  let pp_sort_def ppf (name, sort_def) =
    Sort_def.pp_generic ~open_loc ~close_loc ~name ppf sort_def
  in
  Fmt.pf ppf "%a@,%a" pp_externals externals Fmt.(list pp_sort_def) sort_defs
;;

let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

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

let parse ~comment =
  let open Lvca_parsing in
  lift2
    (fun externals sort_defs -> { externals; sort_defs })
    (many (Kind.Parse.decl ~comment))
    (many1 (Sort_def.parse ~comment))
  <?> "abstract syntax"
;;

let%test_module _ =
  (module struct
    let tm_def =
      let tm = Sort.Name ((), "tm") in
      let tm_v = Valence.Valence ([], tm) in
      let integer = Sort.Name ((), "integer") in
      let integer_v = Valence.Valence ([], integer) in
      ( "tm"
      , Sort_def.Sort_def
          ( []
          , [ Operator_def ((), "add", Arity ((), [ tm_v; tm_v ]))
            ; Operator_def ((), "lit", Arity ((), [ integer_v ]))
            ] ) )
    ;;

    let parse = parse ~comment:Lvca_parsing.c_comment

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
        |> erase_info
      in
      let expected =
        { externals = [ "integer", Kind ((), 1) ]
        ; sort_defs = [ tm_def; "empty", Sort_def ([], []) ]
        }
      in
      assert (equal Unit.( = ) parsed expected)
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

    let parse =
      test_parse_with (parse ~comment:Lvca_parsing.no_comment)
      >> map_info ~f:Commented.get_range
    ;;

    (* let ( = ) = equal Opt_range.( = ) *)
    let tm_sort = Sort.Name ((), "tm")
    let tm_valence = Valence.Valence ([], tm_sort)
    let ty_sort = Sort.Name ((), "ty")
    let ty_valence = Valence.Valence ([], ty_sort)
    let foo_sort = Sort.Name ((), "foo")
    let x_sort = Sort.Name ((), "x")

    (*
    let%test _ =
      parse "bool := true() | false()"
      (*     0123456789012345678901234 *)
      = { externals = []
        ; sort_defs =
            [ ( "bool"
              , Sort_def
                  ( []
                  , [ Operator_def (Opt_range.mk 8 12, "true", [])
                    ; Operator_def (Opt_range.mk 17 22, "false", [])
                    ] ) )
            ]
        }
    ;;
    *)

    let ( = ) = equal Unit.( = )

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
      |> erase_info
      =
      let externals = [ "integer", Kind.Kind ((), 1); "list", Kind ((), 2) ] in
      let sort_defs =
        [ ( "ty"
          , Sort_def.Sort_def
              ( []
              , [ Operator_def ((), "bool", Arity ((), []))
                ; Operator_def ((), "arr", Arity ((), [ ty_valence; ty_valence ]))
                ] ) )
        ; ( "tm"
          , Sort_def
              ( []
              , [ Operator_def ((), "app", Arity ((), [ tm_valence; tm_valence ]))
                ; Operator_def
                    ( ()
                    , "lam"
                    , Arity ((), [ Valence ([ Sort_binding tm_sort ], tm_sort) ]) )
                ] ) )
        ; ( "foo"
          , Sort_def
              ( [ "x", Some (Kind ((), 1)) ]
              , [ Operator_def
                    ( ()
                    , "foo"
                    , Arity
                        ( ()
                        , [ Valence
                              ( [ Sort_pattern
                                    { pattern_sort = foo_sort; var_sort = x_sort }
                                ]
                              , x_sort )
                          ; Valence ([ Sort_binding x_sort ], x_sort)
                          ] ) )
                ; Operator_def ((), "bar", Arity ((), [ Valence ([], x_sort) ]))
                ] ) )
        ]
      in
      { externals; sort_defs }
    ;;

    let ( = ) =
      List.equal (Tuple2.equal String.( = ) (Kind.equal ~info_eq:Opt_range.( = )))
    ;;

    let%test _ =
      let lang =
        parse {|
      integer : *
      list : * -> *

      foo := Foo()
      |}
      in
      lang.externals
      = [ "integer", Kind.Kind (Opt_range.mk 17 18, 1)
        ; "list", Kind (Opt_range.mk 32 38, 2)
        ]
    ;;
  end)
;;
