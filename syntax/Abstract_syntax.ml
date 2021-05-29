(** Types for representing languages *)

open Base
open Lvca_util

let test_parse_with : 'a Lvca_parsing.t -> string -> 'a =
 fun p str ->
  match Lvca_parsing.parse_string p str with
  | Ok value -> value
  | Error msg -> failwith msg
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

    let t =
      sep_by1 (string "->") (char '*')
      >>|| (fun (Parse_result.{ value = stars; range; _ } as parse_result) ->
             { parse_result with value = Kind (range, List.length stars) })
      <?> "kind"
    ;;

    let decl =
      lift3 (fun ident _colon kind -> ident, kind) identifier (char ':') t
      <?> "kind declaration"
    ;;
  end
end

module PatternSort = struct
  type 'info t =
    { pattern_sort : 'info Sort.t
    ; var_sort : 'info Sort.t
    }

  let equal ~info_eq ps1 ps2 =
    Sort.equal info_eq ps1.pattern_sort ps2.pattern_sort
    && Sort.equal info_eq ps1.var_sort ps2.var_sort
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

module SortSlot = struct
  type 'info t =
    | SortBinding of 'info Sort.t
    | SortPattern of 'info PatternSort.t

  let map_info ~f = function
    | SortBinding s -> SortBinding (Sort.map_info ~f s)
    | SortPattern { pattern_sort; var_sort } ->
      SortPattern
        { pattern_sort = Sort.map_info ~f pattern_sort
        ; var_sort = Sort.map_info ~f var_sort
        }
  ;;

  let equal ~info_eq slot1 slot2 =
    match slot1, slot2 with
    | SortBinding s1, SortBinding s2 -> Sort.equal info_eq s1 s2
    | SortPattern ps1, SortPattern ps2 -> PatternSort.equal ~info_eq ps1 ps2
    | _, _ -> false
  ;;

  let pp_generic ~open_loc ~close_loc ppf = function
    | SortBinding sort -> Sort.pp_generic ~open_loc ~close_loc ppf sort
    | SortPattern ps -> PatternSort.pp_generic ~open_loc ~close_loc ppf ps
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  let instantiate env = function
    | SortBinding s -> SortBinding (Sort.instantiate env s)
    | SortPattern ps -> SortPattern (PatternSort.instantiate env ps)
  ;;

  let kind_check env = function
    | SortBinding sort -> Sort.kind_check env sort
    | SortPattern { pattern_sort; var_sort } ->
      [ pattern_sort; var_sort ] |> List.fold ~init:env ~f:Sort.kind_check
  ;;

  module Parse = struct
    module Sort = Sort.Parse
    open Lvca_parsing

    let t =
      Sort.t
      >>= fun sort ->
      choice
        [ (brackets Sort.t
          >>| fun var_sort -> SortPattern { pattern_sort = sort; var_sort })
        ; return (SortBinding sort)
        ]
      <?> "sort slot"
    ;;
  end
end

module Valence = struct
  type 'info t = Valence of 'info SortSlot.t list * 'info Sort.t

  let equal ~info_eq (Valence (slots1, sort1)) (Valence (slots2, sort2)) =
    List.equal (SortSlot.equal ~info_eq) slots1 slots2 && Sort.equal info_eq sort1 sort2
  ;;

  let map_info ~f (Valence (slots, sort)) =
    Valence (List.map ~f:(SortSlot.map_info ~f) slots, Sort.map_info ~f sort)
  ;;

  let pp_generic ~open_loc ~close_loc ppf (Valence (binders, result)) =
    let sort_pp = Sort.pp_generic ~open_loc ~close_loc in
    match binders with
    | [] -> sort_pp ppf result
    | _ ->
      Fmt.pf
        ppf
        "%a. %a"
        Fmt.(list ~sep:(any ".@ ") (SortSlot.pp_generic ~open_loc ~close_loc))
        binders
        sort_pp
        result
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  let instantiate env = function
    | Valence (binding_sort_slots, body_sort) ->
      Valence
        ( List.map binding_sort_slots ~f:(SortSlot.instantiate env)
        , Sort.instantiate env body_sort )
  ;;

  let kind_check env (Valence (binding_slots, value_sort)) =
    let env = binding_slots |> List.fold ~init:env ~f:SortSlot.kind_check in
    Sort.kind_check env value_sort
  ;;

  module Parse = struct
    module ParseSortSlot = SortSlot.Parse
    open Lvca_parsing

    let t =
      let t' =
        sep_by1 (char '.') ParseSortSlot.t
        >>= fun slots ->
        let binders, body_slot = List.unsnoc slots in
        match body_slot with
        | SortSlot.SortBinding body_sort -> return (Valence (binders, body_sort))
        | _ ->
          fail
            (Fmt.str
               "Expected a simple sort, instead found a pattern sort (%a)"
               SortSlot.pp
               body_slot)
      in
      t' <?> "valence"
    ;;
  end
end

module Arity = struct
  type 'info t = 'info Valence.t list

  let pp_generic ~open_loc ~close_loc ppf t =
    Fmt.(parens (list ~sep:semi (Valence.pp_generic ~open_loc ~close_loc))) ppf t
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t
  let equal ~info_eq = List.equal (Valence.equal ~info_eq)
  let map_info ~f = List.map ~f:(Valence.map_info ~f)
  let erase arity = map_info ~f:(Fn.const ()) arity
  let instantiate env = List.map ~f:(Valence.instantiate env)

  module Parse = struct
    let t = Lvca_parsing.(parens (sep_by (char ';') Valence.Parse.t) <?> "arity")
  end

  let%test_module _ =
    (module struct
      let tm = Sort.Name (None, "tm")
      let tm_v = Valence.Valence ([], tm)
      let integer = Sort.Name (None, "integer")
      let integer_v = Valence.Valence ([], integer)
      let ( = ) = equal ~info_eq:(fun _ _ -> true)

      let%test_unit _ = assert (test_parse_with Parse.t "(integer)" = [ integer_v ])
      let%test_unit _ = assert (test_parse_with Parse.t "(tm; tm)" = [ tm_v; tm_v ])

      let%test_unit _ =
        assert (
          test_parse_with Parse.t "(tm. tm)"
          = [ Valence.Valence ([ SortBinding tm ], tm) ])
      ;;

      let%test_unit _ =
        assert (test_parse_with Parse.t "(tm)" = [ Valence.Valence ([], tm) ])
      ;;

      let%test_unit _ =
        assert (
          test_parse_with Parse.t "(tm[tm]. tm)"
          = [ Valence.Valence ([ SortPattern { pattern_sort = tm; var_sort = tm } ], tm) ])
      ;;

      let expect_okay str =
        match Lvca_parsing.parse_string Parse.t str with
        | Ok _ -> ()
        | Error msg -> Stdio.print_string msg
      ;;

      let%expect_test _ =
        expect_okay "(tm[tm]. tm[tm]. tm)";
        [%expect]
      ;;

      let%expect_test _ =
        expect_okay "((foo bar)[baz quux]. tm)";
        [%expect]
      ;;

      let%test_unit _ = assert (test_parse_with Parse.t "()" = [])
    end)
  ;;
end

module OperatorDef = struct
  type 'info t = OperatorDef of string * 'info Arity.t

  let equal ~info_eq (OperatorDef (name1, arity1)) (OperatorDef (name2, arity2)) =
    String.(name1 = name2) && Arity.equal ~info_eq arity1 arity2
  ;;

  let map_info ~f (OperatorDef (name, arity)) = OperatorDef (name, Arity.map_info ~f arity)

  let kind_check env (OperatorDef (_name, arity)) =
    arity |> List.fold ~init:env ~f:Valence.kind_check
  ;;

  let pp_generic ~open_loc ~close_loc ppf (OperatorDef (name, arity)) =
    Fmt.pf ppf "%s%a" name (Arity.pp_generic ~open_loc ~close_loc) arity
  ;;

  let pp ppf t = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf t

  module Parse = struct
    open Lvca_parsing

    let t =
      lift2 (fun ident arity -> OperatorDef (ident, arity)) identifier Arity.Parse.t
      <?> "operator definition"
    ;;
  end

  let%test_module _ =
    (module struct
      let%test_unit _ =
        let ( = ) = equal ~info_eq:(fun _ _ -> true) in
        assert (test_parse_with Parse.t "foo()" = OperatorDef ("foo", []))
      ;;
    end)
  ;;
end

module Sort_def = struct
  type 'info t =
    | Sort_def of (string * 'info Kind.t option) list * 'info OperatorDef.t list

  let equal ~info_eq (Sort_def (vars1, ops1)) (Sort_def (vars2, ops2)) =
    List.equal
      (Tuple2.equal String.( = ) (Option.equal (Kind.equal ~info_eq)))
      vars1
      vars2
    && List.equal (OperatorDef.equal ~info_eq) ops1 ops2
  ;;

  let map_info ~f (Sort_def (vars, op_defs)) =
    let vars =
      vars
      |> List.map ~f:(fun (name, kind_opt) ->
             match kind_opt with
             | None -> name, None
             | Some kind -> name, Some (Kind.map_info ~f kind))
    in
    let op_defs = op_defs |> List.map ~f:(OperatorDef.map_info ~f) in
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
    List.fold operators ~init:env ~f:OperatorDef.kind_check
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
    let pp_op_def = OperatorDef.pp_generic ~open_loc ~close_loc in
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

  module Parse = struct
    open Lvca_parsing

    let assign = string ":="
    let bar = char '|'

    let sort_var_decl =
      choice
        [ (identifier >>| fun name -> name, None)
        ; (parens Kind.Parse.decl >>| fun (name, kind) -> name, Some kind)
        ]
      <?> "sort variable declaration"
    ;;

    let t =
      lift4
        (fun name vars _assign op_defs -> name, Sort_def (vars, op_defs))
        identifier
        (many sort_var_decl)
        assign
        (option '|' bar *> sep_by bar OperatorDef.Parse.t)
      <?> "sort definition"
    ;;
  end

  let%test_module _ =
    (module struct
      let ( = ) = Tuple2.equal String.( = ) (equal ~info_eq:(fun _ _ -> true))

      let%test_unit _ =
        assert (
          test_parse_with Parse.t {|foo := foo()|}
          = ("foo", Sort_def ([], [ OperatorDef ("foo", []) ])))
      ;;

      let%test_unit _ =
        assert (
          test_parse_with Parse.t {|foo x := foo()|}
          = ("foo", Sort_def ([ "x", None ], [ OperatorDef ("foo", []) ])))
      ;;

      let tm_def =
        let tm = Sort.Name (None, "tm") in
        let tm_v = Valence.Valence ([], tm) in
        let integer = Sort.Name (None, "integer") in
        let integer_v = Valence.Valence ([], integer) in
        ( "tm"
        , Sort_def
            ( []
            , [ OperatorDef ("add", [ tm_v; tm_v ]); OperatorDef ("lit", [ integer_v ]) ]
            ) )
      ;;

      let%test_unit _ =
        assert (
          test_parse_with Parse.t {|tm :=
  | add(tm; tm)
  | lit(integer)
      |}
          = tm_def)
      ;;

      let%expect_test _ =
        let foo = Sort.Name ((), "foo") in
        let sort_def =
          Sort_def
            ( []
            , [ OperatorDef.OperatorDef
                  ("foo", [ Valence ([], Sort.Name ((), "integer")) ])
              ; OperatorDef
                  ( "bar"
                  , [ Valence
                        ( [ SortPattern { pattern_sort = foo; var_sort = foo }
                          ; SortBinding foo
                          ]
                        , foo )
                    ] )
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
            , [ OperatorDef.OperatorDef
                  ("foo", [ Valence ([], Sort.Name ((), "integer")) ])
              ] )
        in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo := foo(integer) |}]
      ;;

      let%expect_test _ =
        let sort_def =
          Sort_def
            ( [ "a", None ]
            , [ OperatorDef.OperatorDef
                  ("foo", [ Valence ([], Sort.Name ((), "integer")) ])
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
    List.find operator_defs ~f:(fun (OperatorDef (op_def_name, _)) ->
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

module Parse = struct
  open Lvca_parsing

  let t =
    lift2
      (fun externals sort_defs -> { externals; sort_defs })
      (many Kind.Parse.decl)
      (many1 Sort_def.Parse.t)
    <?> "abstract syntax"
  ;;

  let whitespace_t = whitespace *> t
end

let%test_module _ =
  (module struct
    let tm_def =
      let tm = Sort.Name ((), "tm") in
      let tm_v = Valence.Valence ([], tm) in
      let integer = Sort.Name ((), "integer") in
      let integer_v = Valence.Valence ([], integer) in
      ( "tm"
      , Sort_def.Sort_def
          ([], [ OperatorDef ("add", [ tm_v; tm_v ]); OperatorDef ("lit", [ integer_v ]) ])
      )
    ;;

    let%test_unit _ =
      let parsed =
        test_parse_with
          Parse.whitespace_t
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
      let lang = test_parse_with Parse.whitespace_t str in
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
