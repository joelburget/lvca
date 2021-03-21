(** Types for representing languages *)

open Base
module SMap = Lvca_util.String.Map
module Tuple2 = Lvca_util.Tuple2
module ISet = Lvca_util.Int.Set

let test_parse_with : 'a ParseUtil.t -> string -> 'a =
 fun p str ->
  match Angstrom.parse_string ~consume:All p str with
  | Ok (t, _pos) -> t
  | Error msg -> failwith msg
;;

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

  let pp ppf = function
    | SortBinding sort -> Sort.pp ppf sort
    | SortPattern ps -> PatternSort.pp ppf ps
  ;;

  let instantiate env = function
    | SortBinding s -> SortBinding (Sort.instantiate env s)
    | SortPattern ps -> SortPattern (PatternSort.instantiate env ps)
  ;;

  let kind_check env = function
    | SortBinding sort -> Sort.kind_check env sort
    | SortPattern { pattern_sort; var_sort } ->
      [ pattern_sort; var_sort ] |> List.fold ~init:env ~f:Sort.kind_check
  ;;
end

module Kind = struct
  type t = Kind of int

  let ( = ) (Kind k1) (Kind k2) = Int.(k1 = k2)

  let pp ppf (Kind k) =
    Fmt.(pf ppf "%a" (list ~sep:(any " -> ") (any "*")) (List.init k ~f:(Fn.const ())))
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    open Parsers

    let t =
      sep_by1 (string "->") (char '*')
      >>| (fun stars -> Kind (List.length stars))
      <?> "kind"
    ;;

    let decl =
      lift3 (fun ident _colon kind -> ident, kind) identifier (char ':') t
      <?> "kind declaration"
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

  let pp ppf (Valence (binders, result)) =
    match binders with
    | [] -> Sort.pp ppf result
    | _ ->
      Fmt.pf ppf "%a. %a" Fmt.(list ~sep:(any ".@ ") SortSlot.pp) binders Sort.pp result
  ;;

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
end

module Arity = struct
  type 'info t = 'info Valence.t list

  let pp t = Fmt.(parens (list ~sep:semi Valence.pp)) t
  let equal ~info_eq = List.equal (Valence.equal ~info_eq)
  let map_info ~f = List.map ~f:(Valence.map_info ~f)
  let erase arity = map_info ~f:(Fn.const ()) arity
  let instantiate env = List.map ~f:(Valence.instantiate env)

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module ParseSort = Sort.Parse (Comment)
    module Parsers = ParseUtil.Mk (Comment)
    open Parsers

    exception BuildArityError of string

    type sort_sequence_entry =
      | Sort of OptRange.t Sort.t
      | Bracketed of OptRange.t Sort.t
      | Dot
      | Semi

    type sort_sequence = sort_sequence_entry list

    let sort_to_string sort = Fmt.to_to_string Sort.pp sort

    let str_of_entry = function
      | Sort sort -> sort_to_string sort
      | Bracketed sort -> "[" ^ sort_to_string sort ^ "]"
      | Dot -> "."
      | Semi -> ";"
    ;;

    (* Fold a sequence of sorts, '.'s, and ';'s to an arity. *)
    let build_arity sort_sequence =
      let binding_sorts : OptRange.t SortSlot.t Queue.t = Queue.create () in
      let rec go : sort_sequence -> OptRange.t Valence.t list = function
        | [] -> []
        (* s. ss *)
        | Sort sort :: Dot :: sorts ->
          Queue.enqueue binding_sorts (SortBinding sort);
          go sorts
        (* s1[s2]. ss *)
        | Sort pattern_sort :: Bracketed var_sort :: Dot :: sorts ->
          Queue.enqueue binding_sorts (SortPattern { pattern_sort; var_sort });
          go sorts
        (* s; ss *)
        | Sort sort :: Semi :: sorts | Sort sort :: ([] as sorts) ->
          let binding_sorts_lst = Queue.to_list binding_sorts in
          Queue.clear binding_sorts;
          Valence (binding_sorts_lst, sort) :: go sorts
        | sorts ->
          let entries_str = sorts |> List.map ~f:str_of_entry |> String.concat ~sep:" " in
          let binding_sorts_lst = Queue.to_list binding_sorts in
          let sorts_str =
            binding_sorts_lst
            |> List.map ~f:(Fmt.to_to_string SortSlot.pp)
            |> String.concat ~sep:". "
          in
          let msg =
            match binding_sorts_lst with
            | [] -> entries_str
            | _ -> entries_str ^ " " ^ sorts_str
          in
          raise (BuildArityError (Printf.sprintf "Unexpected sequence of sorts: %s" msg))
      in
      go sort_sequence
    ;;

    let t =
      parens
        (many
           (choice
              [ char ';' >>| Fn.const Semi
              ; char '.' >>| Fn.const Dot
              ; (ParseSort.t >>| fun sort -> Sort sort)
              ; (brackets ParseSort.t >>| fun sort -> Bracketed sort)
              ])
        >>== fun ~pos sort_sequence ->
        try return ~pos (build_arity sort_sequence) with BuildArityError msg -> fail msg)
      <?> "arity"
    ;;
  end

  let%test_module _ =
    (module struct
      module Parse = Parse (ParseUtil.NoComment)

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
        match Angstrom.parse_string ~consume:All Parse.t str with
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

  let pp ppf (OperatorDef (name, arity)) = Fmt.pf ppf "%s%a" name Arity.pp arity

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    module ParseArity = Arity.Parse (Comment)
    open Parsers

    let t =
      lift2 (fun ident arity -> OperatorDef (ident, arity)) identifier ParseArity.t
      <?> "operator definition"
    ;;
  end

  let%test_module _ =
    (module struct
      module Parse = Parse (ParseUtil.NoComment)

      let%test_unit _ =
        let ( = ) = equal ~info_eq:(fun _ _ -> true) in
        assert (test_parse_with Parse.t "foo()" = OperatorDef ("foo", []))
      ;;
    end)
  ;;
end

module SortDef = struct
  type 'info t = SortDef of (string * Kind.t option) list * 'info OperatorDef.t list

  let equal ~info_eq (SortDef (vars1, ops1)) (SortDef (vars2, ops2)) =
    List.equal (Tuple2.equal String.( = ) (Option.equal Kind.( = ))) vars1 vars2
    && List.equal (OperatorDef.equal ~info_eq) ops1 ops2
  ;;

  let map_info ~f (SortDef (vars, op_defs)) =
    SortDef (vars, op_defs |> List.map ~f:(OperatorDef.map_info ~f))
  ;;

  let erase sd = map_info ~f:(Fn.const ()) sd

  let kind_check env sort_name (SortDef (vars, operators)) =
    let update_env env name n =
      Map.update env name ~f:(function
          | None -> ISet.singleton n
          | Some set -> Set.add set n)
    in
    let env = update_env env sort_name (List.length vars) in
    List.fold operators ~init:env ~f:OperatorDef.kind_check
  ;;

  let pp ~name ppf (SortDef (sort_vars, operator_defs)) =
    let open Fmt in
    let pp_sort_var ppf (name, kind_opt) =
      match kind_opt with
      | None -> pf ppf "%s" name
      | Some kind -> pf ppf "(%s : %a)" name Kind.pp kind
    in
    let pp_sort_vars ppf vars =
      match vars with [] -> () | _ -> Fmt.pf ppf " %a" (list pp_sort_var) vars
    in
    match operator_defs with
    | [] -> pf ppf "%s%a :=" name pp_sort_vars sort_vars
    | [ single_def ] ->
      pf ppf "%s%a := @[%a@]" name pp_sort_vars sort_vars OperatorDef.pp single_def
    | _ ->
      pf
        ppf
        "%s%a :=@,@[<v 2>  | %a@]"
        name
        pp_sort_vars
        sort_vars
        (list ~sep:(any "@,| ") OperatorDef.pp)
        operator_defs
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    module OperatorDef = OperatorDef.Parse (Comment)
    module Kind = Kind.Parse (Comment)
    open Parsers

    let assign = string ":="
    let bar = char '|'

    let sort_var_decl =
      choice
        [ (identifier >>| fun name -> name, None)
        ; (parens Kind.decl >>| fun (name, kind) -> name, Some kind)
        ]
      <?> "sort variable declaration"
    ;;

    let t =
      lift4
        (fun name vars _assign op_defs -> name, SortDef (vars, op_defs))
        identifier
        (many sort_var_decl)
        assign
        (option '|' bar *> sep_by bar OperatorDef.t)
      <?> "sort definition"
    ;;
  end

  let%test_module _ =
    (module struct
      module Parse = Parse (ParseUtil.NoComment)

      let ( = ) = Tuple2.equal String.( = ) (equal ~info_eq:(fun _ _ -> true))

      let%test_unit _ =
        assert (
          test_parse_with Parse.t {|foo := foo()|}
          = ("foo", SortDef ([], [ OperatorDef ("foo", []) ])))
      ;;

      let%test_unit _ =
        assert (
          test_parse_with Parse.t {|foo x := foo()|}
          = ("foo", SortDef ([ "x", None ], [ OperatorDef ("foo", []) ])))
      ;;

      let tm_def =
        let tm = Sort.Name (None, "tm") in
        let tm_v = Valence.Valence ([], tm) in
        let integer = Sort.Name (None, "integer") in
        let integer_v = Valence.Valence ([], integer) in
        ( "tm"
        , SortDef
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
          SortDef
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
          SortDef
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
          SortDef
            ( [ "a", None ]
            , [ OperatorDef.OperatorDef
                  ("foo", [ Valence ([], Sort.Name ((), "integer")) ])
              ] )
        in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo a := foo(integer) |}]
      ;;

      let%expect_test _ =
        let sort_def = SortDef ([ "a", Some (Kind 2) ], []) in
        Fmt.pr "%a" (pp ~name:"foo") sort_def;
        [%expect {| foo (a : * -> *) := |}]
      ;;
    end)
  ;;
end

type 'info t =
  { externals : (string * Kind.t) list
  ; sort_defs : (string * 'info SortDef.t) list
  }

module Unordered = struct
  type 'info t =
    { externals : Kind.t SMap.t
    ; sort_defs : 'info SortDef.t SMap.t
    }
end

let mk_unordered { externals; sort_defs } =
  match SMap.of_alist externals, SMap.of_alist sort_defs with
  | `Ok externals, `Ok sort_defs -> `Ok Unordered.{ externals; sort_defs }
  | `Duplicate_key k, _ | _, `Duplicate_key k -> `Duplicate_key k
;;

let equal info_eq t1 t2 =
  let sort_defs_eq = List.equal (Tuple2.equal String.( = ) (SortDef.equal ~info_eq)) in
  let externals_eq = List.equal (Tuple2.equal String.( = ) Kind.( = )) in
  externals_eq t1.externals t2.externals && sort_defs_eq t1.sort_defs t2.sort_defs
;;

let map_info ~f { externals; sort_defs } =
  { externals
  ; sort_defs =
      List.map ~f:(fun (name, sort_def) -> name, SortDef.map_info ~f sort_def) sort_defs
  }
;;

let erase_info t = map_info ~f:(Fn.const ()) t

let lookup_operator { externals = _; sort_defs } sort_name op_name =
  let open Option.Let_syntax in
  let%bind (SortDef (vars, operator_defs)) =
    List.find_map sort_defs ~f:(fun (name, def) ->
        if String.(name = sort_name) then Some def else None)
  in
  let%map result =
    List.find operator_defs ~f:(fun (OperatorDef (op_def_name, _)) ->
        String.(op_def_name = op_name))
  in
  vars, result
;;

let pp ppf { externals; sort_defs } =
  let pp_externals = Fmt.(list (pair ~sep:(any " : ") string Kind.pp)) in
  let pp_sort_def ppf (name, sort_def) = SortDef.pp ~name ppf sort_def in
  Fmt.pf ppf "%a@,%a" pp_externals externals Fmt.(list pp_sort_def) sort_defs
;;

type kind_map = int SMap.t
type kind_mismap = ISet.t SMap.t

let kind_check { externals; sort_defs } =
  let env =
    externals
    |> List.map ~f:(fun (name, Kind n) -> name, n)
    |> SMap.of_alist_exn
    |> Map.map ~f:ISet.singleton
  in
  let mismap =
    sort_defs
    |> List.fold ~init:env ~f:(fun env (sort_name, sort_def) ->
           SortDef.kind_check env sort_name sort_def)
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
  | [] -> Ok (SMap.of_alist_exn fine_vars)
  | _ -> Error (SMap.of_alist_exn mismapped_vars)
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Kind = Kind.Parse (Comment)
  module SortDef = SortDef.Parse (Comment)
  open Parsers

  let t =
    lift2
      (fun externals sort_defs -> { externals; sort_defs })
      (many Kind.decl)
      (many1 SortDef.t)
    <?> "abstract syntax"
  ;;

  let whitespace_t = junk *> t
end

let%test_module _ =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let tm_def =
      let tm = Sort.Name ((), "tm") in
      let tm_v = Valence.Valence ([], tm) in
      let integer = Sort.Name ((), "integer") in
      let integer_v = Valence.Valence ([], integer) in
      ( "tm"
      , SortDef.SortDef
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
        { externals = [ "integer", Kind 1 ]
        ; sort_defs = [ tm_def; "empty", SortDef ([], []) ]
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
