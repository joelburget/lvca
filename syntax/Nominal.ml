open Base
open Stdio
module Cbor = Lvca_util.Cbor
module Json = Lvca_util.Json
module String = Lvca_util.String
module Tuple2 = Lvca_util.Tuple2

let array_map f args = args |> List.map ~f |> Array.of_list |> Json.array

type 'info term =
  | Operator of 'info * string * 'info scope list
  | Var of 'info * string
  | Primitive of 'info Primitive.t

and 'info scope = Scope of 'info Pattern.t list * 'info term

let rec term_equal info_eq t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (scope_equal info_eq) scopes1 scopes2
  | Primitive p1, Primitive p2 -> Primitive.equal ~info_eq p1 p2
  | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false

and scope_equal info_eq (Scope (pats1, tm1)) (Scope (pats2, tm2)) =
  List.equal (Pattern.equal ~info_eq) pats1 pats2 && term_equal info_eq tm1 tm2
;;

let info = function
  | Operator (info, _, _) | Var (info, _) -> info
  | Primitive p -> Primitive.info p
;;

let rec term_pp_generic ~open_loc ~close_loc ~pp_pat ppf tm =
  let list, string, semi, pf = Fmt.(list, string, semi, pf) in
  open_loc ppf (info tm);
  (match tm with
  | Operator (_, tag, subtms) ->
    pf
      ppf
      "@[<hv>%s(%a)@]"
      tag
      (list ~sep:semi (scope_pp_generic ~open_loc ~close_loc ~pp_pat))
      subtms
  | Var (_, v) -> string ppf v
  | Primitive p ->
    Primitive.pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf p);
  close_loc ppf (info tm)

and scope_pp_generic ~open_loc ~close_loc ~pp_pat ppf (Scope (bindings, body)) =
  let any, list, pf = Fmt.(any, list, pf) in
  let pp_body = term_pp_generic ~open_loc ~close_loc ~pp_pat in
  match bindings with
  | [] -> pp_body ppf body
  | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") pp_pat) bindings pp_body body
;;

let rec term_jsonify tm =
  let array, string = Json.(array, string) in
  match tm with
  | Operator (_, tag, tms) ->
    array [| string "o"; string tag; array_map scope_jsonify tms |]
  | Var (_, name) -> array [| string "v"; string name |]
  | Primitive p -> array [| string "p"; Primitive.jsonify p |]

and scope_jsonify (Scope (pats, body)) : Json.t =
  let body = term_jsonify body in
  Json.array [| array_map Pattern.jsonify pats; body |]
;;

let rec term_unjsonify =
  let open Option.Let_syntax in
  Json.(
    function
    | Array [| String "o"; String tag; Array scopes |] ->
      let%map scopes' =
        scopes |> Array.to_list |> List.map ~f:scope_unjsonify |> Option.all
      in
      Operator ((), tag, scopes')
    | Array [| String "v"; String name |] -> Some (Var ((), name))
    | Array [| String "p"; prim |] ->
      let%map prim = Primitive.unjsonify prim in
      Primitive prim
    | _ -> None)

and scope_unjsonify =
  Json.(
    function
    | Array [||] -> None
    | Array arr ->
      let open Option.Let_syntax in
      let binders, body = arr |> Array.to_list |> Lvca_util.List.unsnoc in
      let%bind binders' = binders |> List.map ~f:Pattern.unjsonify |> Option.all in
      let%bind body' = term_unjsonify body in
      Some (Scope (binders', body'))
    | _ -> None)
;;

let rec term_map_info ~f = function
  | Operator (info, name, pats) ->
    Operator (f info, name, List.map pats ~f:(scope_map_info ~f))
  | Var (info, name) -> Var (f info, name)
  | Primitive prim -> Primitive (Primitive.map_info ~f prim)

and scope_map_info ~f (Scope (binders, tm)) =
  let binders' = List.map binders ~f:(Pattern.map_info ~f) in
  let tm = term_map_info ~f tm in
  Scope (binders', tm)
;;

let rec term_subst_all ctx tm =
  match tm with
  | Primitive _ -> tm
  | Var (_loc, name) -> (match Map.find ctx name with Some v -> v | None -> tm)
  | Operator (info, name, scopes) ->
    Operator (info, name, List.map scopes ~f:(scope_subst_all ctx))

and scope_subst_all ctx (Scope (pats, tm)) = Scope (pats, term_subst_all ctx tm)

module Term = struct
  type 'info t = 'info term =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Primitive.t

  let equal = term_equal
  let info = info
  let pp_generic = term_pp_generic

  let pp ppf tm =
    pp_generic
      ~open_loc:(fun _ _ -> ())
      ~close_loc:(fun _ _ -> ())
      ~pp_pat:Pattern.pp
      ppf
      tm
  ;;

  let pp_range ppf tm =
    pp_generic
      ~open_loc:OptRange.open_stag
      ~close_loc:OptRange.close_stag
      ~pp_pat:Pattern.pp_range
      ppf
      tm
  ;;

  let pp_ranges ppf tm =
    pp_generic
      ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
      ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
      ~pp_pat:Pattern.pp_ranges
      ppf
      tm
  ;;

  let pp_str tm = Fmt.to_to_string pp tm
  let serialize tm = tm |> term_jsonify |> Cbor.encode
  let deserialize buf = buf |> Cbor.decode |> Option.bind ~f:term_unjsonify
  let hash tm = tm |> serialize |> Lvca_util.Sha256.hash
  let map_info = term_map_info
  let erase tm = map_info ~f:(fun _ -> ()) tm
  let subst_all = term_subst_all
  let jsonify = term_jsonify
  let unjsonify = term_unjsonify

  let rec match_pattern ~info_eq pat tm =
    match pat, tm with
    | Pattern.Ignored _, _ -> Some String.Map.empty
    | Var (_, name), tm -> Some (String.Map.singleton name tm)
    | Primitive p1, Primitive p2 ->
      if Primitive.equal ~info_eq p1 p2 then Some String.Map.empty else None
    | Primitive _, _ -> None
    | Operator (_, name1, pats), Operator (_, name2, scopes) ->
      if String.(name1 = name2)
      then (
        match List.map2 pats scopes ~f:(match_scope ~info_eq) with
        | Ok zipped ->
          (match String.Map.join_helper zipped with
          | `Ok result -> result
          | `Duplicate_key k ->
            failwith (Printf.sprintf "invariant violation: duplicate key: %s" k))
        | Unequal_lengths -> None)
      else None
    | _ -> None

  and match_scope ~info_eq pat (Scope (binders, tm)) =
    match binders with [] -> match_pattern ~info_eq pat tm | _ -> None
  ;;

  let free_vars tm =
    let module S = String.Set in
    let rec free_vars bound_vars = function
      | Operator (_, _, scopes) ->
        scopes |> List.map ~f:(scope_free_vars bound_vars) |> S.union_list
      | Var (_, name) -> if Set.mem bound_vars name then S.empty else S.singleton name
      | Primitive _ -> S.empty
    and scope_free_vars bound_vars (Scope (binders, tm)) =
      let bound_vars =
        binders
        |> List.map ~f:Pattern.vars_of_pattern
        |> S.union_list
        |> Set.union bound_vars
      in
      free_vars bound_vars tm
    in
    free_vars S.empty tm
  ;;

  let rec select_path ~path tm =
    match path with
    | [] -> Ok tm
    | i :: path ->
      (match tm with
      | Var (_, name) ->
        Error (Printf.sprintf "select_path: hit var %s but path not finished" name)
      | Primitive _ -> Error "select_path: hit primitive but path not finished"
      | Operator (_, _, scopes) ->
        (match List.nth scopes i with
        | None ->
          Error
            (Printf.sprintf
               "select_path: path index %n too high (only %n scopes)"
               i
               (List.length scopes))
        | Some (Scope (_pats, tm)) -> select_path ~path tm))
  ;;

  let valence_to_string v = Fmt.to_to_string AbstractSyntax.Valence.pp v

  let check lang =
    let lookup_operator = AbstractSyntax.lookup_operator lang in
    let check_pattern = Pattern.check lang in
    let rec check_term var_sorts expected_sort tm =
      let result =
        match tm with
        | Var (_, v) ->
          (match Map.find var_sorts v with
          | None ->
            Some
              (CheckFailure.err (Printf.sprintf "Unknown variable %s (is it bound?)" v))
          | Some var_sort ->
            if Sort.equal
                 Unit.( = )
                 (Sort.erase_info var_sort)
                 (Sort.erase_info expected_sort)
            then None
            else (
              let sort_to_string = Fmt.to_to_string Sort.pp in
              Some
                (CheckFailure.err
                   (Printf.sprintf
                      "Variable %s has unexpected sort (saw: %s) (expected: %s)"
                      v
                      (sort_to_string var_sort)
                      (sort_to_string expected_sort)))))
        | Primitive p ->
          (match Primitive.check p expected_sort with
          | None -> None
          | Some msg -> Some (CheckFailure.err msg))
        | Operator (_, operator_name, op_scopes) ->
          let sort_name, sort_args =
            match expected_sort with
            | Sort.Name (_, sort_name) -> sort_name, []
            | Sort.Ap (_, sort_name, sort_args) -> sort_name, sort_args
          in
          (match lookup_operator sort_name operator_name with
          | None ->
            Some
              (CheckFailure.err
                 (Printf.sprintf
                    "Nominal.check: failed to find operator %s in sort %s"
                    operator_name
                    sort_name))
          | Some (sort_vars, OperatorDef (_, arity)) ->
            (* TODO: kind check *)
            let sort_vars = sort_vars |> List.map ~f:Tuple2.get1 in
            let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
            let concrete_arity = AbstractSyntax.Arity.instantiate sort_env arity in
            check_slots var_sorts concrete_arity op_scopes)
      in
      Option.map result ~f:(fun { message; stack } ->
          CheckFailure.
            { message
            ; stack = { term = Either.Second tm; sort = expected_sort } :: stack
            })
    and check_slots var_sorts valences scopes =
      match List.zip scopes valences with
      | Unequal_lengths ->
        Some
          (CheckFailure.err
             (Printf.sprintf
                "Wrong number of subterms (%u) for this arity (%s)"
                (List.length scopes)
                (valences |> List.map ~f:valence_to_string |> String.concat ~sep:", ")))
      | Ok scope_valences ->
        List.find_map scope_valences ~f:(fun (scope, valence) ->
            check_scope var_sorts valence scope)
    and check_scope var_sorts valence (Scope (binders, body)) =
      let (Valence (binder_sorts, body_sort)) = valence in
      match List.zip binder_sorts binders with
      | Unequal_lengths ->
        Some
          (CheckFailure.err
             (Printf.sprintf
                "Wrong number of binders (%u) for this valence (%s) (expected %u)"
                (List.length binders)
                (valence_to_string valence)
                (List.length binder_sorts)))
      | Ok binders ->
        let binders_env =
          binders
          |> List.map ~f:(fun (slot, pat) ->
                 match slot, pat with
                 | SortBinding sort, Var (_, _v) ->
                   check_pattern ~pattern_sort:sort ~var_sort:sort pat
                 | SortBinding _sort, _ ->
                   Error
                     (CheckFailure.err
                        "Fixed-valence binders must all be vars (no patterns)")
                 | SortPattern { pattern_sort; var_sort }, _ ->
                   check_pattern ~pattern_sort ~var_sort pat)
          |> List.map
               ~f:
                 (Result.map_error
                    ~f:(CheckFailure.map_frame_terms ~f:(fun pat -> Either.First pat)))
        in
        (* Check the body with the new binders environment *)
        (match Result.all binders_env with
        | Error err -> Some err
        | Ok binders_env ->
          (match String.Map.strict_unions binders_env with
          | `Ok binders_env (* check every term in body for an error *) ->
            check_term
              (Lvca_util.Map.union_right_biased var_sorts binders_env)
              body_sort
              body
          | `Duplicate_key k ->
            Some
              (CheckFailure.err
                 (* TODO: should this definitely not be allowed? Seems okay. *)
                 (Printf.sprintf
                    "Did you mean to bind the same variable (%s) twice in the same set \
                     of patterns? That's not allowed!"
                    k))))
    in
    check_term String.Map.empty
  ;;

  let rec to_pattern = function
    | Var (info, name) ->
      let v =
        if String.is_substring_at name ~pos:0 ~substring:"_"
        then Pattern.Ignored (info, String.slice name 1 0)
        else Var (info, name)
      in
      Ok v
    | Operator (info, name, tms) ->
      let open Result.Let_syntax in
      let%map subtms = tms |> List.map ~f:scope_to_patterns |> Result.all in
      Pattern.Operator (info, name, subtms)
    | Primitive prim -> Ok (Primitive prim)

  and scope_to_patterns = function
    | Scope ([], tm) -> to_pattern tm
    | Scope (binders, tm) ->
      let binders' = List.map binders ~f:Pattern.erase in
      let tm = erase tm in
      Error (Scope (binders', tm))
  ;;

  let rec of_pattern = function
    | Pattern.Operator (info, name, pats) ->
      Operator
        ( info
        , name (* TODO: should the scope really inherit the 'info? *)
        , List.map pats ~f:(fun pat -> Scope ([], of_pattern pat)) )
    | Primitive prim -> Primitive prim
    | Var (info, name) -> Var (info, name)
    | Ignored (info, name) -> Var (info, "_" ^ name)
  ;;

  module Primitive' = Primitive

  (*
    module Primitive = struct
      let check lang sort pat = check lang sort pat
    end
       *)

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    module Primitive = Primitive.Parse (Comment)

    type tm_or_sep =
      | Tm of OptRange.t t
      | Sep of char

    type 'info term = 'info t

    let t : OptRange.t term ParseUtil.t =
      let open Parsers in
      fix (fun term ->
          let tm_or_sep : tm_or_sep ParseUtil.t =
            choice
              [ (fun c -> Sep c) <$> choice [ char '.'; char ';' ]
              ; (fun tm -> Tm tm) <$> term
              ]
          in
          (* (b11. ... b1n. t11, ... t1n; b21. ... b2n. t21, ... t2n) *)
          let accumulate
              : OptRange.t -> string -> tm_or_sep list -> OptRange.t term ParseUtil.t
            =
           fun range tag tokens ->
            (* terms encountered between '.'s, before hitting ',' / ';' *)
            let binding_queue : OptRange.t term Queue.t = Queue.create () in
            (* scopes encountered *)
            let scope_queue : OptRange.t scope Queue.t = Queue.create () in
            let rec go = function
              | [] -> return ~pos:range (Operator (range, tag, Queue.to_list scope_queue))
              | Tm tm :: Sep '.' :: rest ->
                Queue.enqueue binding_queue tm;
                go rest
              | Tm tm :: Sep ';' :: rest (* Note: allow trailing ';' *)
              | Tm tm :: ([] as rest) ->
                (match
                   binding_queue |> Queue.to_list |> List.map ~f:to_pattern |> Result.all
                 with
                | Error _ ->
                  fail "Unexpectedly found a variable binding in pattern position"
                | Ok binders ->
                  Queue.clear binding_queue;
                  Queue.enqueue scope_queue (Scope (binders, tm));
                  go rest)
              | _ -> fail "Malformed term"
            in
            go tokens
          in
          pos
          >>= fun p1 ->
          choice
            [ (Primitive.t >>| fun prim -> Primitive prim)
            ; (identifier
              >>= fun ident ->
              choice
                [ (parens (many tm_or_sep)
                  >>= fun tokens ->
                  pos >>= fun p2 -> accumulate (OptRange.mk p1 p2) ident tokens)
                ; (pos >>| fun p2 -> Var (OptRange.mk p1 p2, ident))
                ])
            ])
      <?> "term"
    ;;

    let whitespace_t = Parsers.(junk *> t)
  end

  module Properties = struct
    module Parse = Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive'.Parse (ParseUtil.NoComment)
    open PropertyResult
    module Primitive = Primitive'

    let parse = ParseUtil.parse_string Parse.t

    let json_round_trip1 t =
      match t |> jsonify |> unjsonify with
      | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
      | Some t' -> PropertyResult.check Caml.(t = t') (Fmt.str "%a <> %a" pp t' pp t)
    ;;

    let json_round_trip2 json =
      match json |> unjsonify with
      | None -> Uninteresting
      | Some t ->
        PropertyResult.check
          Lvca_util.Json.(jsonify t = json)
          "jsonify t <> json (TODO: print)"
    ;;

    let string_round_trip1 t =
      match t |> pp_str |> parse with
      | Ok t' ->
        let t'' = erase t' in
        PropertyResult.check Caml.(t'' = t) (Fmt.str "%a <> %a" pp t'' pp t)
      | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (pp_str t) msg)
    ;;

    let string_round_trip2 str =
      match parse str with
      | Error _ -> Uninteresting
      | Ok t ->
        let str' = pp_str t in
        if Base.String.(str' = str)
        then Ok
        else (
          match parse str with
          | Error msg -> Failed msg
          | Ok t' ->
            let str'' = pp_str t' in
            PropertyResult.check
              String.(str'' = str')
              (Fmt.str {|"%s" <> "%s"|} str'' str'))
    ;;

    (* malformed input *)
  end
end

module Scope = struct
  type 'info t = 'info scope = Scope of 'info Pattern.t list * 'info term

  let equal = scope_equal
  let pp_generic = scope_pp_generic

  let pp ppf tm =
    pp_generic
      ~open_loc:(fun _ _ -> ())
      ~close_loc:(fun _ _ -> ())
      ~pp_pat:Pattern.pp
      ppf
      tm
  ;;

  let pp_range ppf tm =
    pp_generic
      ~open_loc:OptRange.open_stag
      ~close_loc:OptRange.close_stag
      ~pp_pat:Pattern.pp_range
      ppf
      tm
  ;;

  let pp_ranges ppf tm =
    pp_generic
      ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
      ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
      ~pp_pat:Pattern.pp_ranges
      ppf
      tm
  ;;

  let pp_str scope = Fmt.to_to_string pp scope
  let erase (Scope (pats, tm)) = Scope (List.map pats ~f:Pattern.erase, Term.erase tm)
  let subst_all = scope_subst_all
  let map_info = scope_map_info
  let jsonify = scope_jsonify
  let unjsonify = scope_unjsonify
end

let%test_module "Nominal" =
  (module struct
    let print_serialize tm =
      tm
      |> Term.serialize
      |> Bytes.to_array
      |> Array.iter ~f:(fun char -> printf "%02x" (Char.to_int char))
    ;;

    let print_hash tm = printf "%s" (Term.hash tm)
    let ( = ) = Caml.( = )
    let tm = Term.Var ((), "x")
    let j_tm = Json.(Array [| String "v"; String "x" |])

    let%test _ = Term.jsonify tm = j_tm

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 8261766178 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6 |}]
    ;;

    let tm = Term.Operator ((), "Z", [])

    let%test _ = Term.jsonify tm = Json.(Array [| String "o"; String "Z"; Array [||] |])

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615a80 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c |}]
    ;;

    let tm =
      Term.Operator ((), "S", [ Scope.Scope ([ Pattern.Var ((), "x") ], Var ((), "x")) ])
    ;;

    let%test _ =
      Term.jsonify tm
      = Json.(
          Array
            [| String "o"
             ; String "S"
             ; Array
                 [| (* scopes *)
                    Array [| (* scope *)
                             Array [| j_tm |]; (* binders *)
                                               j_tm |]
                 |]
            |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615381828182617661788261766178 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 391e4a6e3dc6964d60c642c52416d18b102dca357a3e4953834dfefc0e02dfbc |}]
    ;;

    let tm = Term.Primitive ((), PrimInteger (Z.of_string "12345"))

    let%test _ =
      Term.jsonify tm
      = Json.(Array [| String "p"; Array [| String "i"; String "12345" |] |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 826170826169653132333435 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5 |}]
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 826170826169653132333435 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5 |}]
    ;;

    let to_pattern_exn tm =
      match Term.to_pattern tm with
      | Ok pat -> pat
      | Error _ -> failwith "failed to convert term to pattern"
    ;;

    let%test _ = to_pattern_exn (Var (1, "abc")) = Var (1, "abc")
    let%test _ = to_pattern_exn (Var (2, "_abc")) = Ignored (2, "abc")
    let%test _ = to_pattern_exn (Var (3, "_")) = Ignored (3, "")
    let%test _ = Term.of_pattern (Ignored (4, "abc")) = Var (4, "_abc")
    let%test _ = Term.of_pattern (Ignored (5, "")) = Var (5, "_")
  end)
;;

let%test_module "TermParser" =
  (module struct
    let ( = ) = Caml.( = )

    module ParseNominal = Term.Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)

    let parse = ParseUtil.parse_string ParseNominal.whitespace_t
    let parse_erase str = parse str |> Result.map ~f:Term.erase

    let print_parse str =
      match parse str with
      | Error msg -> print_string ("failed: " ^ msg)
      | Ok tm ->
        Fmt.pr "%a\n" Term.pp tm;
        Fmt.pr "%a" Term.pp_range tm
    ;;

    let%test _ = parse_erase "x" = Ok (Var ((), "x"))
    let%test _ = parse_erase "123" = Ok (Primitive ((), PrimInteger (Z.of_int 123)))
    let%test _ = parse_erase "\"abc\"" = Ok (Primitive ((), PrimString "abc"))

    let x = Term.Var ((), "x")
    let t = Term.Operator ((), "true", [])

    let%test _ =
      parse_erase "lam(x. x)"
      = Ok (Operator ((), "lam", [ Scope ([ Var ((), "x") ], x) ]))
    ;;

    let match_line a b = Term.Operator ((), "match_line", [ Scope ([ a ], b) ])

    let match_lines subtms =
      Term.Operator
        ((), "match_lines", Base.List.map subtms ~f:(fun tm -> Scope.Scope ([], tm)))
    ;;

    let%test _ = parse_erase {| match() |} = Ok (Operator ((), "match", []))

    let%test _ =
      parse_erase {| match(x; x) |}
      = Ok (Operator ((), "match", [ Scope ([], x); Scope ([], x) ]))
    ;;

    let%test _ =
      parse_erase {| match(true(); true()) |}
      = Ok (Operator ((), "match", [ Scope ([], t); Scope ([], t) ]))
    ;;

    let%test _ =
      parse_erase {| match(x;) |} = Ok (Operator ((), "match", [ Scope ([], x) ]))
    ;;

    let%test _ =
      parse_erase
        {|
    match(x; match_lines(
      match_line(foo(). true());
      match_line(bar(_; _x; y). y)
    )) |}
      = Ok
          (Operator
             ( ()
             , "match"
             , [ Scope ([], x)
               ; Scope
                   ( []
                   , match_lines
                       [ match_line
                           (Pattern.Operator ((), "foo", []))
                           (Operator ((), "true", []))
                       ; match_line
                           (Pattern.Operator
                              ( ()
                              , "bar"
                              , [ Ignored ((), ""); Ignored ((), "x"); Var ((), "y") ] ))
                           (Var ((), "y"))
                       ] )
               ] ))
    ;;

    let%expect_test _ =
      print_parse {|"str"|};
      (*012345*)
      [%expect {|
      "str"
      <{0,5}>"str"</{0,5}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      (*0123*)
      [%expect {|
      a()
      <{0,3}>a()</{0,3}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      (*01234*)
      [%expect {|
      a(b)
      <{0,4}>a(<{2,3}>b</{2,3}>)</{0,4}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c)|};
      (*0123456*)
      [%expect
        {|
      a(b; c)
      <{0,6}>a(<{2,3}>b</{2,3}>; <{4,5}>c</{4,5}>)</{0,6}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;d;e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b; c; d; e)
      <{0,11}>a(<{2,3}>b</{2,3}>; <{4,5}>c</{4,5}>; <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>)</{0,11}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c;d;e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b. c; d; e)
      <{0,11}>a(<{2,3}>b</{2,3}>. <{4,5}>c</{4,5}>; <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>)</{0,11}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c;d;e;f;g)|};
      (*012345678901234*)
      [%expect
        {|
      a(b. c; d; e; f; g)
      <{0,14}>a(<{2,3}>b</{2,3}>. <{4,5}>c</{4,5}>; <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>; <{10,11}>f</{10,11}>; <{12,13}>g</{12,13}>)</{0,14}>
    |}]
    ;;
  end)
;;

let%test_module "check" =
  (module struct
    module AbstractSyntaxParse = AbstractSyntax.Parse (ParseUtil.NoComment)
    module Parser = Term.Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)
    module SortParse = Sort.Parse (ParseUtil.NoComment)

    let parse_lang lang_str =
      ParseUtil.parse_string AbstractSyntaxParse.whitespace_t lang_str
      |> Result.ok_or_failwith
    ;;

    let parse_term term_str =
      ParseUtil.parse_string Parser.t term_str |> Result.ok_or_failwith
    ;;

    let parse_sort str = ParseUtil.parse_string SortParse.t str

    let lang_desc =
      {|
value :=
  | unit()
  | lit_int(integer)
  | lit_str(string)
  | list(list value)

list a :=
  | nil()
  | cons(a; list a)

match_line :=
  | match_line(value[value]. term)

term :=
  | lambda(value. term)
  | alt_lambda(term. term)
  | match(match_line)
  | value(value)

test := foo(term[term]. term)
      |}
    ;;

    let language = parse_lang lang_desc

    let check_term' sort_str tm_str =
      match parse_sort sort_str with
      | Error msg -> Fmt.epr "%s" msg
      | Ok sort ->
        let pp ppf CheckFailure.{ term; sort } =
          match term with
          | Either.First pat ->
            Fmt.pf ppf "- @[pattern: %a,@ sort: %a@]" Pattern.pp pat Sort.pp sort
          | Second tm -> Fmt.pf ppf "- @[term: %a,@ sort: %a@]" Term.pp tm Sort.pp sort
        in
        (match tm_str |> parse_term |> Term.check language sort with
        | Some failure -> Fmt.epr "%a" (CheckFailure.pp pp) failure
        | None -> ())
    ;;

    let%expect_test _ =
      check_term' "term" "lambda(a. value(a))";
      [%expect]
    ;;

    (*
    let%expect_test _ =
      check_term'
        "term"
        {|match(
        match_line(
          list(cons(a; nil())).
          value(list(cons(a; cons(a; nil()))))
        ),
        match_line(_. value(list(nil())))
      )
    |};
      [%expect
        {|
        Expected a single term, but found a list
        stack:
        - term: match(match_line(list(cons(a; nil())).
                      value(list(cons(a; cons(a; nil()))))),
                match_line(_. value(list(nil())))),
          sort: term |}]
    ;;
    *)

    let%expect_test _ =
      check_term' "term" "unit()";
      [%expect
        {|
      Nominal.check: failed to find operator unit in sort term
      stack:
      - term: unit(), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "lambda(a. b)";
      [%expect
        {|
      Unknown variable b (is it bound?)
      stack:
      - term: lambda(a. b), sort: term
      - term: b, sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "lambda(val. alt_lambda(tm. val))";
      [%expect
        {|
      Variable val has unexpected sort (saw: value) (expected: term)
      stack:
      - term: lambda(val. alt_lambda(tm. val)), sort: term
      - term: alt_lambda(tm. val), sort: term
      - term: val, sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "value" {|lit_int("foo")|};
      [%expect
        {|
      Unexpected sort (integer) for a primitive ("foo")
      stack:
      - term: lit_int("foo"), sort: value
      - term: "foo", sort: integer |}]
    ;;

    let%expect_test _ =
      check_term' "value" "lit_str(123)";
      [%expect
        {|
      Unexpected sort (string) for a primitive (123)
      stack:
      - term: lit_str(123), sort: value
      - term: 123, sort: string |}]
    ;;

    let%expect_test _ =
      check_term' "term" "lambda(a; b)";
      [%expect
        {|
      Wrong number of subterms (2) for this arity (value. term)
      stack:
      - term: lambda(a; b), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "value" "lit_int(1; 2)";
      [%expect
        {|
      Wrong number of subterms (2) for this arity (integer)
      stack:
      - term: lit_int(1; 2), sort: value |}]
    ;;

    let%expect_test _ =
      check_term' "match_line" "match_line(a. b. value(a))";
      [%expect
        {|
      Wrong number of binders (2) for this valence (value[value]. term) (expected 1)
      stack:
      - term: match_line(a. b. value(a)), sort: match_line |}]
    ;;

    let%expect_test _ =
      check_term' "match_line" "match_line(a. a)";
      [%expect
        {|
      Variable a has unexpected sort (saw: value) (expected: term)
      stack:
      - term: match_line(a. a), sort: match_line
      - term: a, sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "lambda(a. b. a)";
      [%expect
        {|
      Wrong number of binders (2) for this valence (value. term) (expected 1)
      stack:
      - term: lambda(a. b. a), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "lambda(list(cons(a; cons(b; nil))). value(a))";
      [%expect
        {|
      Fixed-valence binders must all be vars (no patterns)
      stack:
      - term: lambda(list(cons(a; cons(b; nil))). value(a)), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "match(a. a)";
      [%expect
        {|
      Wrong number of binders (1) for this valence (match_line) (expected 0)
      stack:
      - term: match(a. a), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "integer" "1";
      [%expect]
    ;;

    let%expect_test _ =
      check_term' "float" "1.";
      [%expect]
    ;;

    let%expect_test _ =
      check_term' "char" "'a'";
      [%expect]
    ;;

    let%expect_test _ =
      check_term' "string" {|"str"|};
      [%expect]
    ;;
  end)
;;
