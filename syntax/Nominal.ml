open Base
open Stdio
open Lvca_provenance
open Lvca_util

module Types = struct
  type 'info term =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Primitive_impl.All.t

  and 'info scope = Scope of 'info Pattern.t list * 'info term
end

let info = function
  | Types.Operator (info, _, _) | Var (info, _) -> info
  | Primitive p -> Primitive_impl.All.info p
;;

module Equal = struct
  let rec term ~info_eq t1 t2 =
    match t1, t2 with
    | Types.Operator (i1, name1, scopes1), Types.Operator (i2, name2, scopes2) ->
      info_eq i1 i2
      && String.(name1 = name2)
      && List.equal (scope ~info_eq) scopes1 scopes2
    | Primitive p1, Primitive p2 -> Primitive_impl.All.equal ~info_eq p1 p2
    | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
    | _, _ -> false

  and scope ~info_eq (Scope (pats1, tm1)) (Scope (pats2, tm2)) =
    List.equal (Pattern.equal ~info_eq) pats1 pats2 && term ~info_eq tm1 tm2
  ;;
end

module PpGeneric = struct
  let rec term ~open_loc ~close_loc ppf tm =
    let list, string, semi, pf = Fmt.(list, string, semi, pf) in
    open_loc ppf (info tm);
    (match tm with
    | Operator (_, tag, subtms) ->
      pf ppf "@[<hv>%s(%a)@]" tag (list ~sep:semi (scope ~open_loc ~close_loc)) subtms
    | Var (_, v) -> string ppf v
    | Primitive p ->
      (* Note: open_loc and close_loc intentionally nops because we already
         show the location here. *)
      Primitive_impl.All.pp_generic
        ~open_loc:(fun _ _ -> ())
        ~close_loc:(fun _ _ -> ())
        ppf
        p);
    close_loc ppf (info tm)

  and scope ~open_loc ~close_loc ppf (Scope (bindings, body)) =
    let any, list, pf = Fmt.(any, list, pf) in
    let pp_body = term ~open_loc ~close_loc in
    match bindings with
    | [] -> pp_body ppf body
    | _ ->
      pf
        ppf
        "%a.@ %a"
        (list ~sep:(any ".@ ") (Pattern.pp_generic ~open_loc ~close_loc))
        bindings
        pp_body
        body
  ;;
end

module Jsonify = struct
  open Json

  let rec term tm =
    match tm with
    | Types.Operator (_, tag, tms) ->
      array [| string "o"; string tag; array_map scope tms |]
    | Var (_, name) -> array [| string "v"; string name |]
    | Primitive p -> array [| string "p"; Primitive_impl.All.jsonify p |]

  and scope (Types.Scope (pats, body)) : Json.t =
    array [| array_map Pattern.jsonify pats; term body |]
  ;;
end

module Unjsonify = struct
  open Json
  open Option.Let_syntax

  let rec term = function
    | Array [| String "o"; String tag; Array scopes |] ->
      let%map scopes' = scopes |> Array.to_list |> List.map ~f:scope |> Option.all in
      Types.Operator ((), tag, scopes')
    | Array [| String "v"; String name |] -> Some (Var ((), name))
    | Array [| String "p"; prim |] ->
      let%map prim = Primitive_impl.All.unjsonify prim in
      Types.Primitive prim
    | _ -> None

  and scope = function
    | Array [||] -> None
    | Array arr ->
      let binders, body = arr |> Array.to_list |> List.unsnoc in
      let%bind binders = binders |> List.map ~f:Pattern.unjsonify |> Option.all in
      let%map body = term body in
      Types.Scope (binders, body)
    | _ -> None
  ;;
end

module MapInfo = struct
  let rec term ~f = function
    | Types.Operator (info, name, pats) ->
      Types.Operator (f info, name, List.map pats ~f:(scope ~f))
    | Var (info, name) -> Var (f info, name)
    | Primitive prim -> Primitive (Primitive_impl.All.map_info ~f prim)

  and scope ~f (Scope (binders, tm)) =
    let binders = List.map binders ~f:(Pattern.map_info ~f) in
    let tm = term ~f tm in
    Scope (binders, tm)
  ;;
end

module SubstAll = struct
  let rec term ctx tm =
    match tm with
    | Types.Primitive _ -> tm
    | Var (_loc, name) -> (match Map.find ctx name with Some v -> v | None -> tm)
    | Operator (info, name, scopes) ->
      Operator (info, name, List.map scopes ~f:(scope ctx))

  and scope ctx (Scope (pats, tm)) = Scope (pats, term ctx tm)
end

let bound_vars binders =
  binders |> List.map ~f:Pattern.vars_of_pattern |> String.Set.union_list
;;

module Rename = struct
  let rec term x y tm =
    match tm with
    | Types.Primitive _ -> tm
    | Var (info, name) -> if String.(name = x) then Var (info, y) else tm
    | Operator (info, name, scopes) ->
      Operator (info, name, List.map scopes ~f:(scope x y))

  and scope x y (Scope (binders, tm) as scope) =
    let bound_vars = bound_vars binders in
    if Set.mem bound_vars x then scope else Scope (binders, term x y tm)
  ;;
end

module Term = struct
  type 'info t = 'info Types.term =
    | Operator of 'info * string * 'info Types.scope list
    | Var of 'info * string
    | Primitive of 'info Primitive_impl.All.t

  let to_nominal x = x
  let of_nominal x = Ok x
  let equal = Equal.term
  let map_info = MapInfo.term
  let subst_all = SubstAll.term
  let rename = Rename.term
  let jsonify = Jsonify.term
  let unjsonify = Unjsonify.term
  let pp_generic = PpGeneric.term
  let info = info
  let pp ppf tm = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm

  let pp_range ppf tm =
    pp_generic ~open_loc:Range.open_stag ~close_loc:Range.close_stag ppf tm
  ;;

  let pp_ranges ppf tm =
    pp_generic ~open_loc:Ranges.open_stag ~close_loc:Ranges.close_stag ppf tm
  ;;

  let pp_opt_range ppf tm =
    pp_generic ~open_loc:Opt_range.open_stag ~close_loc:Opt_range.close_stag ppf tm
  ;;

  let pp_source_range ppf tm =
    pp_generic ~open_loc:Source_range.open_stag ~close_loc:Source_range.close_stag ppf tm
  ;;

  let pp_source_ranges ppf tm =
    pp_generic
      ~open_loc:Source_ranges.open_stag
      ~close_loc:Source_ranges.close_stag
      ppf
      tm
  ;;

  let erase tm = map_info ~f:(fun _ -> ()) tm
  let serialize tm = tm |> jsonify |> Cbor.encode
  let deserialize buf = buf |> Cbor.decode |> Option.bind ~f:unjsonify
  let hash tm = tm |> serialize |> Sha256.hash

  let rec match_pattern ~info_eq pat tm =
    match pat, tm with
    | Pattern.Var (_, name), tm -> Some (String.Map.singleton name tm)
    | Primitive p1, Primitive p2 ->
      if Primitive_impl.All.equal ~info_eq p1 p2 then Some String.Map.empty else None
    | Primitive _, _ -> None
    | Operator (_, name1, pats), Operator (_, name2, scopes) ->
      if String.(name1 = name2)
      then (
        match List.map2 pats scopes ~f:(match_scope ~info_eq) with
        | Ok zipped ->
          (match String.Map.join_helper zipped with
          | `Ok result -> result
          | `Duplicate_key k -> invariant_violation ~here:[%here] ("duplicate key: " ^ k))
        | Unequal_lengths -> None)
      else None
    | _ -> None

  and match_scope ~info_eq pat (Scope (binders, tm)) =
    match binders with [] -> match_pattern ~info_eq pat tm | _ -> None
  ;;

  let free_vars tm =
    let module S = String.Set in
    let rec free_vars bound_vars' = function
      | Operator (_, _, scopes) ->
        scopes |> List.map ~f:(scope_free_vars bound_vars') |> S.union_list
      | Var (_, name) -> if Set.mem bound_vars' name then S.empty else S.singleton name
      | Primitive _ -> S.empty
    and scope_free_vars bound_vars' (Scope (binders, tm)) =
      let bound_vars' = binders |> bound_vars |> Set.union bound_vars' in
      free_vars bound_vars' tm
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

  let check lang =
    let lookup_operator = Abstract_syntax.lookup_operator lang in
    let check_pattern = Pattern.check lang in
    let rec check_term var_sorts expected_sort tm =
      let result =
        match tm with
        | Var (_, v) ->
          (match Map.find var_sorts v with
          | None ->
            Some
              (Check_failure.err (Printf.sprintf "Unknown variable %s (is it bound?)" v))
          | Some var_sort ->
            if Sort.equal
                 ~info_eq:Unit.( = )
                 (Sort.erase_info var_sort)
                 (Sort.erase_info expected_sort)
            then None
            else
              Some
                (Check_failure.err
                   (Fmt.str
                      "Variable %s has unexpected sort (saw: %a) (expected: %a)"
                      v
                      Sort.pp
                      var_sort
                      Sort.pp
                      expected_sort)))
        | Primitive p ->
          (match Primitive_impl.All.check p expected_sort with
          | None -> None
          | Some msg -> Some (Check_failure.err msg))
        | Operator (_, operator_name, op_scopes) ->
          let sort_name, sort_args = Sort.split expected_sort in
          (match lookup_operator sort_name operator_name with
          | None ->
            Some
              (Check_failure.err
                 (Printf.sprintf
                    "Nominal.check: failed to find operator %s in sort %s"
                    operator_name
                    sort_name))
          | Some (sort_vars, Operator_def (_, _, arity)) ->
            (* TODO: kind check *)
            let sort_vars = sort_vars |> List.map ~f:Tuple2.get1 in
            let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
            let concrete_arity = Abstract_syntax.Arity.instantiate sort_env arity in
            check_slots var_sorts concrete_arity op_scopes)
      in
      Option.map result ~f:(fun { message; stack } ->
          Check_failure.
            { message
            ; stack = { term = Either.Second tm; sort = expected_sort } :: stack
            })
    and check_slots var_sorts (Arity (_, valences)) scopes =
      match List.zip scopes valences with
      | Unequal_lengths ->
        Some
          (Check_failure.err
             Fmt.(
               str
                 "Wrong number of subterms (%u) for this arity (%a)"
                 (List.length scopes)
                 (list ~sep:comma Abstract_syntax.Valence.pp)
                 valences))
      | Ok scope_valences ->
        List.find_map scope_valences ~f:(fun (scope, valence) ->
            check_scope var_sorts valence scope)
    and check_scope var_sorts valence (Scope (binders, body)) =
      let (Valence (binder_sorts, body_sort)) = valence in
      match List.zip binder_sorts binders with
      | Unequal_lengths ->
        Some
          (Check_failure.err
             Fmt.(
               str
                 "Wrong number of binders (%u) for this valence (%a) (expected %u)"
                 (List.length binders)
                 Abstract_syntax.Valence.pp
                 valence
                 (List.length binder_sorts)))
      | Ok binders ->
        let binders_env =
          binders
          |> List.map ~f:(fun (slot, pat) ->
                 match slot, pat with
                 | Sort_binding sort, Var (_, _v) ->
                   check_pattern ~pattern_sort:sort ~var_sort:sort pat
                 | Sort_binding _sort, _ ->
                   Error
                     (Check_failure.err
                        "Fixed-valence binders must all be vars (no patterns)")
                 | Sort_pattern { pattern_sort; var_sort }, _ ->
                   check_pattern ~pattern_sort ~var_sort pat)
          |> List.map
               ~f:
                 (Result.map_error
                    ~f:(Check_failure.map_frame_terms ~f:(fun pat -> Either.First pat)))
        in
        (* Check the body with the new binders environment *)
        (match Result.all binders_env with
        | Error err -> Some err
        | Ok binders_env ->
          (match String.Map.strict_unions binders_env with
          | `Ok binders_env (* check every term in body for an error *) ->
            check_term (Map.union_right_biased var_sorts binders_env) body_sort body
          | `Duplicate_key k ->
            Some
              (Check_failure.err
                 (* TODO: should this definitely not be allowed? Seems okay. *)
                 (Printf.sprintf
                    "Did you mean to bind the same variable (%s) twice in the same set \
                     of patterns? That's not allowed!"
                    k))))
    in
    check_term String.Map.empty
  ;;

  let rec to_pattern = function
    | Var (info, name) -> Ok (Pattern.Var (info, name))
    | Operator (info, name, tms) ->
      let open Result.Let_syntax in
      let%map subtms = tms |> List.map ~f:scope_to_pattern |> Result.all in
      Pattern.Operator (info, name, subtms)
    | Primitive prim -> Ok (Primitive prim)

  and scope_to_pattern = function
    | Scope ([], tm) -> to_pattern tm
    | Scope (binders, tm) ->
      Error (Types.Scope (List.map binders ~f:Pattern.erase, erase tm))
  ;;

  let rec of_pattern = function
    | Pattern.Operator (info, name, pats) ->
      Operator
        ( info
        , name (* TODO: should the scope really inherit the 'info? *)
        , List.map pats ~f:(fun pat -> Types.Scope ([], of_pattern pat)) )
    | Primitive prim -> Primitive prim
    | Var (info, name) -> Var (info, name)
  ;;

  let parse ~comment ~parse_prim =
    let open Lvca_parsing in
    fix (fun term ->
        let slot =
          sep_by1 (Ws.char '.') term
          >>== fun Parse_result.{ value; range } ->
          let binders, tm = List.unsnoc value in
          match binders |> List.map ~f:to_pattern |> Result.all with
          | Error _ -> fail "Unexpectedly found a variable binding in pattern position"
          | Ok binders -> return ~range (Types.Scope (binders, tm))
        in
        choice
          ~failure_msg:"looking for a primitive or identifier (for a var or operator)"
          [ parse_prim
          ; (Ws.identifier
            >>== fun Parse_result.{ value = ident; range = ident_range } ->
            choice
              [ (Ws.parens (sep_end_by (Ws.char ';') slot)
                >>== fun { value = slots; range = parens_range } ->
                option' comment
                >>|| fun { value = comment; _ } ->
                let range = Opt_range.union ident_range parens_range in
                Parse_result.
                  { value = Operator (Commented.{ range; comment }, ident, slots); range }
                )
              ; (option' comment
                >>| fun comment -> Var (Commented.{ range = ident_range; comment }, ident)
                )
              ])
          ])
    <?> "term"
  ;;

  let parse' ~comment =
    parse
      ~comment
      ~parse_prim:
        Lvca_parsing.(Primitive_impl.All.parse ~comment >>| fun prim -> Primitive prim)
  ;;

  let parse_no_comment = parse' ~comment:Lvca_parsing.no_comment

  module Properties = struct
    open Property_result

    let parse = Lvca_parsing.parse_string parse_no_comment
    let to_string tm = Fmt.to_to_string pp tm
    let ( = ) = equal ~info_eq:Unit.( = )

    let json_round_trip1 t =
      match t |> jsonify |> unjsonify with
      | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
      | Some t' -> Property_result.check (t = t') (Fmt.str "%a <> %a" pp t' pp t)
    ;;

    let json_round_trip2 json =
      match json |> unjsonify with
      | None -> Uninteresting
      | Some t ->
        Property_result.check Json.(jsonify t = json) "jsonify t <> json (TODO: print)"
    ;;

    let string_round_trip1 t =
      match t |> to_string |> parse with
      | Ok t' ->
        let t' = erase t' in
        Property_result.check (t' = t) (Fmt.str "%a <> %a" pp t' pp t)
      | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
    ;;

    let string_round_trip2 str =
      match parse str with
      | Error _ -> Uninteresting
      | Ok t ->
        let str' = to_string t in
        if Base.String.(str' = str)
        then Ok
        else (
          match parse str with
          | Error msg -> Failed msg
          | Ok t' ->
            let str'' = to_string t' in
            Property_result.check
              String.(str'' = str')
              (Fmt.str {|"%s" <> "%s"|} str'' str'))
    ;;

    (* malformed input *)
  end
end

module Scope = struct
  type 'info t = 'info Types.scope = Scope of 'info Pattern.t list * 'info Types.term

  let equal = Equal.scope
  let pp_generic = PpGeneric.scope
  let subst_all = SubstAll.scope
  let rename = Rename.scope
  let map_info = MapInfo.scope
  let jsonify = Jsonify.scope
  let unjsonify = Unjsonify.scope
  let pp ppf tm = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm

  let pp_range ppf tm =
    pp_generic ~open_loc:Range.open_stag ~close_loc:Range.close_stag ppf tm
  ;;

  let pp_ranges ppf tm =
    pp_generic ~open_loc:Ranges.open_stag ~close_loc:Ranges.close_stag ppf tm
  ;;

  let pp_opt_range ppf tm =
    pp_generic ~open_loc:Opt_range.open_stag ~close_loc:Opt_range.close_stag ppf tm
  ;;

  let pp_source_range ppf tm =
    pp_generic ~open_loc:Source_range.open_stag ~close_loc:Source_range.close_stag ppf tm
  ;;

  let pp_source_ranges ppf tm =
    pp_generic
      ~open_loc:Source_ranges.open_stag
      ~close_loc:Source_ranges.close_stag
      ppf
      tm
  ;;

  let erase (Scope (pats, tm)) = Scope (List.map pats ~f:Pattern.erase, Term.erase tm)
end

module Convertible = struct
  module type S = sig
    include Language_object_intf.S

    val to_nominal : 'info t -> 'info Term.t
    val of_nominal : 'info Term.t -> ('info t, 'info Term.t) Result.t
  end

  module type Extended_s = sig
    include S

    val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
    (* TODO: should they be comparable as well? *)

    val erase : _ t -> unit t
    val pp : _ t Fmt.t
    val to_string : _ t -> string

    (* TODO: to_pattern, of_pattern *)

    val select_path
      :  path:int list
      -> 'info t
      -> ('info t, (string, 'info Term.t) Base.Either.t) Result.t

    (** {1 Serialization} *)
    include Language_object_intf.Json_convertible with type 'info t := 'info t

    include Language_object_intf.Serializable with type 'info t := 'info t

    (** {1 Printing / Parsing} *)
    val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

    val pp_opt_range : Lvca_provenance.Opt_range.t t Fmt.t
    val parse : comment:'a Lvca_parsing.t -> 'a Commented.t t Lvca_parsing.t
  end

  module Extend (Object : S) : Extended_s with type 'info t = 'info Object.t = struct
    include Object

    let erase tm = Object.map_info ~f:(fun _ -> ()) tm
    let equal ~info_eq t1 t2 = Term.equal ~info_eq (to_nominal t1) (to_nominal t2)

    let pp_generic ~open_loc ~close_loc ppf tm =
      Term.pp_generic ~open_loc ~close_loc ppf (to_nominal tm)
    ;;

    let pp_opt_range ppf tm =
      pp_generic
        ~open_loc:Lvca_provenance.Opt_range.open_stag
        ~close_loc:Lvca_provenance.Opt_range.close_stag
        ppf
        tm
    ;;

    let pp ppf tm = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf tm
    let to_string tm = Fmt.to_to_string pp tm

    let select_path ~path tm =
      match tm |> Object.to_nominal |> Term.select_path ~path with
      | Ok tm ->
        (match Object.of_nominal tm with
        | Ok tm -> Ok tm
        | Error tm -> Error (Either.Second tm))
      | Error msg -> Error (Either.First msg)
    ;;

    let jsonify tm = tm |> Object.to_nominal |> Term.jsonify

    let unjsonify json =
      let open Option.Let_syntax in
      let%bind nom = Term.unjsonify json in
      match Object.of_nominal nom with Ok tm -> Some tm | Error _ -> None
    ;;

    let serialize tm = tm |> jsonify |> Cbor.encode
    let deserialize buf = buf |> Cbor.decode |> Option.bind ~f:unjsonify
    let hash tm = tm |> serialize |> Sha256.hash

    let parse ~comment =
      let open Lvca_parsing in
      Term.parse' ~comment
      >>= fun nom ->
      match of_nominal nom with
      | Error nom -> fail Fmt.(str "Parse: failed to convert %a from nominal" Term.pp nom)
      | Ok tm -> return tm
    ;;
  end

  module Check_parse_pretty (Object : S) :
    Properties_intf.Parse_pretty_s with type 'info t = 'info Object.t = struct
    open Property_result
    module Object = Extend (Object)
    open Object

    type 'info t = 'info Object.t

    let to_string = Fmt.to_to_string Object.pp
    let parse = Lvca_parsing.parse_string (parse ~comment:Lvca_parsing.no_comment)

    let string_round_trip1 t =
      match t |> to_string |> parse with
      | Ok t' ->
        let t'' = Object.erase t' in
        Property_result.check
          Object.(equal ~info_eq:Unit.( = ) t'' t)
          (Fmt.str "%a <> %a" pp t'' pp t)
      | Error msg -> Failed (Fmt.str {|parse_string "%a": %s|} pp t msg)
    ;;

    let string_round_trip2 str =
      match parse str with
      | Error _ -> Uninteresting
      | Ok t ->
        let str' = t |> Object.erase |> to_string in
        if String.(str' = str)
        then Ok
        else (
          match parse str with
          | Error msg -> Failed msg
          | Ok t' ->
            let str'' = t' |> Object.erase |> to_string in
            Property_result.check
              String.(str'' = str')
              (Fmt.str {|"%s" <> "%s"|} str'' str'))
    ;;
  end

  module Check_json (Object : S) :
    Properties_intf.Json_s with type 'info t = 'info Object.t = struct
    open Property_result
    module Object = Extend (Object)
    open Object

    type 'info t = 'info Object.t

    let json_round_trip1 t =
      match t |> Object.jsonify |> Object.unjsonify with
      | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
      | Some t' ->
        Property_result.check
          (Object.equal ~info_eq:Unit.( = ) t t')
          (Fmt.str "%a <> %a" pp t' pp t)
    ;;

    let json_round_trip2 json =
      match json |> Object.unjsonify with
      | None -> Uninteresting
      | Some t ->
        Property_result.check
          Json.(Object.jsonify t = json)
          "jsonify t <> json (TODO: print)"
    ;;
  end

  module Check_properties (Object : S) :
    Properties_intf.S with type 'info t = 'info Object.t = struct
    include Check_parse_pretty (Object)
    include Check_json (Object)
  end
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
    let ( = ) = Json.( = )
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

    let tm = Term.Primitive ((), Integer (Z.of_string "12345"))

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

    let ( = ) = Pattern.equal ~info_eq:Int.( = )

    let%test _ = to_pattern_exn (Var (1, "abc")) = Var (1, "abc")
    let%test _ = to_pattern_exn (Var (2, "_abc")) = Var (2, "_abc")
    let%test _ = to_pattern_exn (Var (3, "_")) = Var (3, "_")

    let ( = ) = Term.equal ~info_eq:Int.( = )

    let%test _ = Term.of_pattern (Var (4, "_abc")) = Var (4, "_abc")
    let%test _ = Term.of_pattern (Var (5, "_")) = Var (5, "_")

    let parse_exn str =
      let parse = Lvca_parsing.(parse_string (whitespace *> Term.parse_no_comment)) in
      match parse str with Error msg -> failwith msg | Ok tm -> tm
    ;;

    let pp = Fmt.pr "%a\n" Term.pp

    let%expect_test _ =
      "a" |> parse_exn |> Term.rename "a" "b" |> pp;
      [%expect {| b |}]
    ;;

    let%expect_test _ =
      "foo(a; b; c; 1; 'c')" |> parse_exn |> Term.rename "a" "b" |> pp;
      [%expect {| foo(b; b; c; 1; 'c') |}]
    ;;

    let%expect_test _ =
      "foo(lam(a. a); lam(b. b); lam(a. b); lam(b. a))"
      |> parse_exn
      |> Term.rename "a" "b"
      |> pp;
      [%expect {| foo(lam(a. a); lam(b. b); lam(a. b); lam(b. b)) |}]
    ;;
  end)
;;

let%test_module "TermParser" =
  (module struct
    let ( = ) = Result.equal (Term.equal ~info_eq:Unit.( = )) String.( = )
    let parse = Lvca_parsing.(parse_string (whitespace *> Term.parse_no_comment))
    let parse_erase str = parse str |> Result.map ~f:Term.erase

    let print_parse str =
      match parse str with
      | Error msg -> print_string ("failed: " ^ msg)
      | Ok tm ->
        Fmt.pr "%a\n" Term.pp tm;
        Fmt.pr "%a" Term.pp_opt_range (Term.map_info ~f:Commented.get_range tm)
    ;;

    let%test _ = parse_erase "x" = Ok (Var ((), "x"))
    let%test _ = parse_erase "123" = Ok (Primitive ((), Integer (Z.of_int 123)))
    let%test _ = parse_erase "\"abc\"" = Ok (Primitive ((), String "abc"))

    let x = Term.Var ((), "x")
    let t = Term.Operator ((), "true", [])

    let%test _ =
      parse_erase "lam(x. x)"
      = Ok (Operator ((), "lam", [ Scope ([ Var ((), "x") ], x) ]))
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

    (* TODO
    let%test _ =
      parse_erase {| match(x;) |} = Ok (Operator ((), "match", [ Scope ([], x) ]))
    ;;
    *)

    let%expect_test _ =
      print_parse {|"str"|};
      (*            012345*)
      [%expect {|
      "str"
      <{0,5}>"str"</{0,5}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      (*            0123*)
      [%expect {|
      a()
      <{0,3}>a()</{0,3}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      (*            01234*)
      [%expect {|
      a(b)
      <{0,4}>a(<{2,3}>b</{2,3}>)</{0,4}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c)|};
      (*            0123456*)
      [%expect
        {|
      a(b; c)
      <{0,6}>a(<{2,3}>b</{2,3}>; <{4,5}>c</{4,5}>)</{0,6}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;d;e;)|};
      (*            012345678901*)
      [%expect
        {|
      a(b; c; d; e)
      <{0,11}>a(<{2,3}>b</{2,3}>; <{4,5}>c</{4,5}>; <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>)</{0,11}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c;d;e;)|};
      (*            012345678901*)
      [%expect
        {|
      a(b. c; d; e)
      <{0,11}>a(<{2,3}>b</{2,3}>. <{4,5}>c</{4,5}>; <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>)</{0,11}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c;d;e;f;g)|};
      (*            012345678901234*)
      [%expect
        {|
      a(b. c; d; e; f; g)
      <{0,14}>a(<{2,3}>b</{2,3}>. <{4,5}>c</{4,5}>; <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>; <{10,11}>f</{10,11}>; <{12,13}>g</{12,13}>)</{0,14}>
    |}]
    ;;

    let%expect_test _ =
      print_parse
        {|
match(x; match_lines(
  match_line(foo(). true());
  match_line(bar(_; _x; y). y)
)) |};
      [%expect
        {|
        match(x;
        match_lines(match_line(foo(). true()); match_line(bar(_; _x; y). y)))
        <{1,85}>
        match(<{7,8}>x</{7,8}>;
        <{10,84}>match_lines(<{25,50}>match_line(<{36,41}>foo()</{36,41}>. <{43,49}>true()</{43,49}>)</{25,50}>; <{54,82}>match_line(<{65,78}>bar(<{69,70}>_</{69,70}>; <{72,74}>_x</{72,74}>; <{76,77}>y</{76,77}>)</{65,78}>. <{80,81}>y</{80,81}>)</{54,82}>)</{10,84}>)</{1,85}> |}]
    ;;

    let%expect_test _ =
      print_parse {|Succ(Ifz(Zero(); x. x; Zero()))|};
      (*            01234567890123456789012345678901
                    0         1         2         3*)
      [%expect
        {|
      Succ(Ifz(Zero(); x. x; Zero()))
      <{0,31}>Succ(<{5,30}>Ifz(<{9,15}>Zero()</{9,15}>; <{17,18}>x</{17,18}>. <{20,21}>x</{20,21}>; <{23,29}>Zero()</{23,29}>)</{5,30}>)</{0,31}>
    |}]
    ;;
  end)
;;

let%test_module "check" =
  (module struct
    let parse_lang lang_str =
      Lvca_parsing.(
        parse_string (whitespace *> Abstract_syntax.parse ~comment:no_comment) lang_str)
      |> Result.ok_or_failwith
    ;;

    let parse_term term_str =
      Lvca_parsing.parse_string Term.parse_no_comment term_str |> Result.ok_or_failwith
    ;;

    let parse_sort str = Lvca_parsing.(parse_string (Sort.parse ~comment:no_comment) str)

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
        let pp ppf term =
          match term with
          | Either.First pat -> Fmt.pf ppf "pattern: %a" Pattern.pp pat
          | Second tm -> Fmt.pf ppf "term: %a" Term.pp tm
        in
        (match tm_str |> parse_term |> Term.check language sort with
        | Some failure -> Fmt.epr "%a" (Check_failure.pp pp) failure
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
