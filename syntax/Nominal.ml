open Base
open Stdio
open Lvca_util

module Types = struct
  type term =
    | Operator of Provenance.t * string * scope list
    | Var of Provenance.t * string
    | Primitive of Primitive_impl.All.t

  and scope = Scope of Pattern.t list * term
end

let mk_Operator ?(provenance = Provenance.of_here [%here]) name scopes =
  Types.Operator (provenance, name, scopes)
;;

let mk_Var ?(provenance = Provenance.of_here [%here]) name = Types.Var (provenance, name)
let mk_Primitive prim = Types.Primitive prim
let mk_Scope pats tm = Types.Scope (pats, tm)

let info = function
  | Types.Operator (info, _, _) | Var (info, _) -> info
  | Primitive p -> Primitive_impl.All.info p
;;

module Equivalent = struct
  let rec term ?(info_eq = fun _ _ -> true) t1 t2 =
    match t1, t2 with
    | Types.Operator (i1, name1, scopes1), Types.Operator (i2, name2, scopes2) ->
      info_eq i1 i2
      && String.(name1 = name2)
      && List.equal (scope ~info_eq) scopes1 scopes2
    | Primitive p1, Primitive p2 -> Primitive_impl.All.equivalent ~info_eq p1 p2
    | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
    | _, _ -> false

  and scope ?(info_eq = fun _ _ -> true) (Scope (pats1, tm1)) (Scope (pats2, tm2)) =
    List.equal (Pattern.equivalent ~info_eq) pats1 pats2 && term ~info_eq tm1 tm2
  ;;
end

module Pp = struct
  let rec term ppf tm =
    let list, string, semi, pf = Fmt.(list, string, semi, pf) in
    match tm with
    | Types.Operator (_, tag, subtms) ->
      Provenance.open_stag ppf (info tm);
      pf ppf "@[<hv>%s(%a)@]" tag (list ~sep:semi scope) subtms;
      Provenance.close_stag ppf (info tm)
    | Var (_, v) ->
      Provenance.open_stag ppf (info tm);
      string ppf v;
      Provenance.close_stag ppf (info tm)
    | Primitive p -> Primitive_impl.All.pp ppf p

  and scope ppf (Scope (bindings, body)) =
    let any, list, pf = Fmt.(any, list, pf) in
    match bindings with
    | [] -> term ppf body
    | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") Pattern.pp) bindings term body
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
      mk_Operator tag scopes'
    | Array [| String "v"; String name |] -> Some (mk_Var name)
    | Array [| String "p"; prim |] ->
      prim |> Primitive_impl.All.unjsonify |> Option.map ~f:mk_Primitive
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

let bound_vars binders =
  binders |> List.map ~f:Pattern.vars_of_pattern |> String.Set.union_list
;;

module Subst_all = struct
  let rec term ctx tm =
    match tm with
    | Types.Primitive _ -> tm
    | Var (_loc, name) -> (match Map.find ctx name with Some v -> v | None -> tm)
    | Operator (info, name, scopes) ->
      Operator (info, name, List.map scopes ~f:(scope ctx))

  and scope ctx (Scope (pats, tm) as scope) =
    let bound_vars = pats |> bound_vars |> Set.to_list in
    let ctx = List.fold bound_vars ~init:ctx ~f:Map.remove in
    if Map.is_empty ctx then scope else Scope (pats, term ctx tm)
  ;;
end

module Subst = struct
  let rec term ~name ~value tm =
    match tm with
    | Types.Primitive _ -> tm
    | Var (_loc, name') -> if String.(name' = name) then value else tm
    | Operator (info, name, scopes) ->
      Operator (info, name, List.map scopes ~f:(scope ~name ~value))

  and scope ~name ~value (Scope (pats, tm) as scope) =
    let bound_vars = bound_vars pats in
    if Set.mem bound_vars name then scope else Scope (pats, term ~name ~value tm)
  ;;
end

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
  type t = Types.term =
    | Operator of Provenance.t * string * Types.scope list
    | Var of Provenance.t * string
    | Primitive of Primitive_impl.All.t

  let to_nominal x = x
  let of_nominal x = Ok x
  let equivalent = Equivalent.term
  let ( = ) = equivalent ~info_eq:Provenance.( = )
  let subst_all = Subst_all.term
  let subst = Subst.term
  let rename = Rename.term
  let jsonify = Jsonify.term
  let unjsonify = Unjsonify.term
  let pp = Pp.term
  let info = info
  let serialize tm = tm |> jsonify |> Cbor.encode
  let deserialize buf = buf |> Cbor.decode |> Option.bind ~f:unjsonify
  let hash tm = tm |> serialize |> Sha256.hash

  let rec match_pattern pat tm =
    match pat, tm with
    | Pattern.Var (_, name), tm -> Some (String.Map.singleton name tm)
    | Primitive p1, Primitive p2 ->
      if Primitive_impl.All.( = ) p1 p2 then Some String.Map.empty else None
    | Primitive _, _ -> None
    | Operator (_, name1, pats), Operator (_, name2, scopes) ->
      if String.(name1 = name2)
      then (
        match List.map2 pats scopes ~f:match_scope with
        | Ok zipped ->
          (match String.Map.join_helper zipped with
          | `Ok result -> result
          | `Duplicate_key k -> invariant_violation ~here:[%here] ("duplicate key: " ^ k))
        | Unequal_lengths -> None)
      else None
    | _ -> None

  and match_scope pat (Scope (binders, tm)) =
    match binders with [] -> match_pattern pat tm | _ -> None
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

  type check_failure = (Pattern.t, t) Base.Either.t Check_failure.t

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
            if Sort.equivalent var_sort expected_sort
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
          | Error lookup_err ->
            Some
              (Check_failure.err
                 (Fmt.str
                    "Nominal.check: failed to find operator %s in sort %s: %a"
                    operator_name
                    sort_name
                    Abstract_syntax.Lookup_error.pp
                    lookup_err))
          | Ok (sort_vars, Operator_def (_, _, arity)) ->
            (* TODO: kind check *)
            let sort_vars = List.map sort_vars ~f:Tuple2.get1 in
            let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
            let concrete_arity = Arity.instantiate sort_env arity in
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
                 (list ~sep:comma Valence.pp)
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
                 Valence.pp
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
    | Scope (binders, tm) -> Error (Types.Scope (binders, tm))
  ;;

  let rec of_pattern = function
    | Pattern.Operator (info, name, pats) ->
      Operator
        ( info
        , name (* TODO: should the scope really inherit the ? *)
        , List.map pats ~f:(fun pat -> Types.Scope ([], of_pattern pat)) )
    | Primitive prim -> Primitive prim
    | Var (info, name) -> Var (info, name)
  ;;

  let parse reserved_words ~parse_prim =
    let open Lvca_parsing in
    let open C_comment_parser in
    fix (fun term ->
        let slot =
          sep_by1 (char '.') term
          >>== fun Parse_result.{ value; range } ->
          let binders, tm = List.unsnoc value in
          match binders |> List.map ~f:to_pattern |> Result.all with
          | Error _ -> fail "Unexpectedly found a variable binding in pattern position"
          | Ok binders -> return ~range (Types.Scope (binders, tm))
        in
        choice
          ~failure_msg:"looking for a primitive or identifier (for a var or operator)"
          [ parse_prim
          ; (lower_identifier reserved_words
            >>~ fun range name -> Var (Provenance.of_range range, name))
          ; make2
              (fun ~info ident slots ->
                mk_Operator ~provenance:(Provenance.of_range info) ident slots)
              (upper_identifier reserved_words)
              (parens (sep_end_by (char ';') slot))
          ])
    <?> "term"
  ;;

  let parse' reserved_words =
    parse
      reserved_words
      ~parse_prim:Lvca_parsing.(Primitive_impl.All.parse >>| fun prim -> Primitive prim)
  ;;

  module Properties = struct
    open Property_result

    let parse = Lvca_parsing.parse_string (parse' String.Set.empty)
    let to_string tm = Fmt.to_to_string pp tm

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
      | Ok t' -> Property_result.check (t' = t) (Fmt.str "%a <> %a" pp t' pp t)
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
  type t = Types.scope = Scope of Pattern.t list * Types.term

  let equivalent = Equivalent.scope
  let ( = ) = equivalent ~info_eq:Provenance.( = )
  let pp = Pp.scope
  let subst_all = Subst_all.scope
  let subst = Subst.scope
  let rename = Rename.scope
  let jsonify = Jsonify.scope
  let unjsonify = Unjsonify.scope
end

module Conversion_error = struct
  type t =
    | Scope of Provenance.t * Scope.t * string option
    | Term of Provenance.t * Term.t * string option

  let mk_Scope ?(provenance = Provenance.of_here [%here]) ?message scope =
    Scope (provenance, scope, message)
  ;;

  let mk_Term ?(provenance = Provenance.of_here [%here]) ?message term =
    Term (provenance, term, message)
  ;;

  let pp ppf = function
    | Scope (_, _, None) -> Fmt.pf ppf "Scope conversion error"
    | Scope (_, _, Some msg) -> Fmt.pf ppf "Scope conversion error (%s)" msg
    | Term (_, _, None) -> Fmt.pf ppf "Term conversion error"
    | Term (_, _, Some msg) -> Fmt.pf ppf "Term conversion error (%s)" msg
  ;;
end

module Convertible = struct
  module type S = sig
    include Language_object_intf.S

    val to_nominal : t -> Term.t
    val of_nominal : Term.t -> (t, Conversion_error.t) Result.t
  end

  module type Extended_s = sig
    include S

    val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
    val ( = ) : t -> t -> bool
    (* TODO: should they be comparable as well? *)

    (* TODO: to_pattern, of_pattern *)

    val subst_all : t String.Map.t -> t -> (t, Conversion_error.t) Result.t
    val subst : name:string -> value:t -> t -> (t, Conversion_error.t) Result.t
    val rename : string -> string -> t -> (t, Conversion_error.t) Result.t

    val select_path
      :  path:int list
      -> t
      -> (t, (string, Conversion_error.t) Base.Either.t) Result.t

    (** {1 Serialization} *)
    include Language_object_intf.Json_convertible with type t := t

    include Language_object_intf.Serializable with type t := t

    (** {1 Printing / Parsing} *)

    val pp : t Fmt.t
    val to_string : t -> string
    val parse : t Lvca_parsing.t
  end

  module Extend (Object : S) : Extended_s with type t = Object.t = struct
    include Object

    let equivalent ?info_eq t1 t2 =
      Term.equivalent ?info_eq (to_nominal t1) (to_nominal t2)
    ;;

    let ( = ) = equivalent ~info_eq:Provenance.( = )
    let pp ppf tm = Term.pp ppf (to_nominal tm)
    let to_string tm = Fmt.to_to_string pp tm

    let subst_all ctx t =
      let ctx = Map.map ctx ~f:to_nominal in
      t |> to_nominal |> Term.subst_all ctx |> of_nominal
    ;;

    let subst ~name ~value t =
      let value = to_nominal value in
      t |> to_nominal |> Term.subst ~name ~value |> of_nominal
    ;;

    let rename x y t = t |> to_nominal |> Term.rename x y |> of_nominal

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

    let parse =
      let open Lvca_parsing in
      Term.(parse' String.Set.empty)
      >>= fun nom ->
      match of_nominal nom with
      | Error err ->
        fail
          Fmt.(str "Extend.parse: Nominal conversion error: %a" Conversion_error.pp err)
      | Ok tm -> return tm
    ;;
  end

  module Check_parse_pretty (Object : S) :
    Properties_intf.Parse_pretty_s with type t = Object.t = struct
    open Property_result
    module Object = Extend (Object)
    open Object

    type t = Object.t

    let to_string = Fmt.to_to_string Object.pp
    let parse = Lvca_parsing.parse_string parse

    let string_round_trip1 t =
      match t |> to_string |> parse with
      | Ok t' -> Property_result.check Object.(t' = t) (Fmt.str "%a <> %a" pp t' pp t)
      | Error msg -> Failed (Fmt.str {|parse_string "%a": %s|} pp t msg)
    ;;

    let string_round_trip2 str =
      match parse str with
      | Error _ -> Uninteresting
      | Ok t ->
        let str' = to_string t in
        if String.(str' = str)
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
  end

  module Check_json (Object : S) : Properties_intf.Json_s with type t = Object.t = struct
    open Property_result
    module Object = Extend (Object)
    open Object

    type t = Object.t

    let json_round_trip1 t =
      match t |> Object.jsonify |> Object.unjsonify with
      | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
      | Some t' ->
        Property_result.check (Object.( = ) t t') (Fmt.str "%a <> %a" pp t' pp t)
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

  module Check_properties (Object : S) : Properties_intf.S with type t = Object.t = struct
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
    let tm = mk_Var "x"
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

    let tm = mk_Operator "Z" []

    let%test _ = Term.jsonify tm = Json.(Array [| String "o"; String "Z"; Array [||] |])

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615a80 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c |}]
    ;;

    let tm = mk_Operator "S" [ Scope.Scope ([ Pattern.mk_Var "x" ], mk_Var "x") ]

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

    let tm = mk_Primitive (Primitive_impl.All.mk_Integer (Z.of_string "12345"))

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

    let info = Provenance.of_here [%here]
    let ( = ) = Pattern.equivalent ~info_eq:(fun _ _ -> true)

    let%test _ = to_pattern_exn (Var (info, "abc")) = Var (info, "abc")
    let%test _ = to_pattern_exn (Var (info, "_abc")) = Var (info, "_abc")
    let%test _ = to_pattern_exn (Var (info, "_")) = Var (info, "_")

    let ( = ) = Term.equivalent ~info_eq:(fun _ _ -> true)

    let%test _ = Term.of_pattern (Var (info, "_abc")) = Var (info, "_abc")
    let%test _ = Term.of_pattern (Var (info, "_")) = Var (info, "_")

    let parse_exn str =
      let parse =
        Lvca_parsing.(parse_string (whitespace *> Term.parse' String.Set.empty))
      in
      match parse str with Error msg -> failwith msg | Ok tm -> tm
    ;;

    let pp = Fmt.pr "%a\n" Term.pp

    let%expect_test _ =
      "a" |> parse_exn |> Term.rename "a" "b" |> pp;
      [%expect
        {| <{ input = Input_unknown; range = {0,1} }>b</{ input = Input_unknown; range = {0,1} }> |}]
    ;;

    let%expect_test _ =
      "Foo(a; b; c; 1; 'c')" |> parse_exn |> Term.rename "a" "b" |> pp;
      [%expect
        {| <{ input = Input_unknown; range = {0,20} }>Foo(<{ input = Input_unknown; range = {4,5} }>b</{ input = Input_unknown; range = {4,5} }>; <{ input = Input_unknown; range = {7,8} }>b</{ input = Input_unknown; range = {7,8} }>; <{ input = Input_unknown; range = {10,11} }>c</{ input = Input_unknown; range = {10,11} }>; <{ input = Input_unknown; range = {13,14} }>1</{ input = Input_unknown; range = {13,14} }>; <{ input = Input_unknown; range = {16,19} }>'c'</{ input = Input_unknown; range = {16,19} }>)</{ input = Input_unknown; range = {0,20} }> |}]
    ;;

    let%expect_test _ =
      "Foo(Lam(a. a); Lam(b. b); Lam(a. b); Lam(b. a))"
      |> parse_exn
      |> Term.rename "a" "b"
      |> pp;
      [%expect
        {| <{ input = Input_unknown; range = {0,47} }>Foo(<{ input = Input_unknown; range = {4,13} }>Lam(<{ input = Input_unknown; range = {8,9} }>a</{ input = Input_unknown; range = {8,9} }>. <{ input = Input_unknown; range = {11,12} }>a</{ input = Input_unknown; range = {11,12} }>)</{ input = Input_unknown; range = {4,13} }>; <{ input = Input_unknown; range = {15,24} }>Lam(<{ input = Input_unknown; range = {19,20} }>b</{ input = Input_unknown; range = {19,20} }>. <{ input = Input_unknown; range = {22,23} }>b</{ input = Input_unknown; range = {22,23} }>)</{ input = Input_unknown; range = {15,24} }>; <{ input = Input_unknown; range = {26,35} }>Lam(<{ input = Input_unknown; range = {30,31} }>a</{ input = Input_unknown; range = {30,31} }>. <{ input = Input_unknown; range = {33,34} }>b</{ input = Input_unknown; range = {33,34} }>)</{ input = Input_unknown; range = {26,35} }>; <{ input = Input_unknown; range = {37,46} }>Lam(<{ input = Input_unknown; range = {41,42} }>b</{ input = Input_unknown; range = {41,42} }>. <{ input = Input_unknown; range = {44,45} }>b</{ input = Input_unknown; range = {44,45} }>)</{ input = Input_unknown; range = {37,46} }>)</{ input = Input_unknown; range = {0,47} }> |}]
    ;;
  end)
;;

let%test_module "TermParser" =
  (module struct
    let ( = ) = Result.equal Term.equivalent String.( = )
    let parse = Lvca_parsing.(parse_string (whitespace *> Term.parse' String.Set.empty))

    let print_parse str =
      match parse str with
      | Error msg -> print_string ("failed: " ^ msg)
      | Ok tm -> Fmt.pr "%a" Term.pp tm
    ;;

    let%test _ = parse "x" = Ok (mk_Var "x")

    let%test _ =
      parse "123" = Ok (mk_Primitive (Primitive_impl.All.mk_Integer (Z.of_int 123)))
    ;;

    let%expect_test _ =
      print_parse {|123  // comment|};
      (*            0123*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,3} }>123</{ input = Input_unknown; range = {0,3} }>
    |}]
    ;;

    let%test _ = parse "\"abc\"" = Ok (mk_Primitive (Primitive_impl.All.mk_String "abc"))

    let x = mk_Var "x"
    let t = mk_Operator "True" []

    let%test _ =
      parse "Lam(x. x)" = Ok (mk_Operator "Lam" [ Scope ([ Pattern.mk_Var "x" ], x) ])
    ;;

    let%test _ = parse {| Match() |} = Ok (mk_Operator "Match" [])

    let%test _ =
      parse {| Match(x; x) |} = Ok (mk_Operator "Match" [ Scope ([], x); Scope ([], x) ])
    ;;

    let%test _ =
      parse {| Match(True(); // comment
      True()) |}
      = Ok (mk_Operator "Match" [ Scope ([], t); Scope ([], t) ])
    ;;

    let%test _ = parse {| Match(x;) |} = Ok (mk_Operator "Match" [ Scope ([], x) ])

    let%expect_test _ =
      print_parse {|"str"|};
      (*            012345*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,5} }>"str"</{ input = Input_unknown; range = {0,5} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A()|};
      (*            0123*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,3} }>A()</{ input = Input_unknown; range = {0,3} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b)|};
      (*            01234*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,4} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>)</{ input = Input_unknown; range = {0,4} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b;c)|};
      (*            0123456*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,6} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>; <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>)</{ input = Input_unknown; range = {0,6} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b;c;d;e;)|};
      (*            012345678901*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,11} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>; <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>; <{ input = Input_unknown; range = {6,7} }>d</{ input = Input_unknown; range = {6,7} }>; <{ input = Input_unknown; range = {8,9} }>e</{ input = Input_unknown; range = {8,9} }>)</{ input = Input_unknown; range = {0,11} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b.c;d;e;)|};
      (*            012345678901*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,11} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>. <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>; <{ input = Input_unknown; range = {6,7} }>d</{ input = Input_unknown; range = {6,7} }>; <{ input = Input_unknown; range = {8,9} }>e</{ input = Input_unknown; range = {8,9} }>)</{ input = Input_unknown; range = {0,11} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b.c;d;e;f;g)|};
      (*            012345678901234*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,14} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>. <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>; <{ input = Input_unknown; range = {6,7} }>d</{ input = Input_unknown; range = {6,7} }>; <{ input = Input_unknown; range = {8,9} }>e</{ input = Input_unknown; range = {8,9} }>; <{ input = Input_unknown; range = {10,11} }>f</{ input = Input_unknown; range = {10,11} }>; <{ input = Input_unknown; range = {12,13} }>g</{ input = Input_unknown; range = {12,13} }>)</{ input = Input_unknown; range = {0,14} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse
        {|
Match(x; Match_lines(
  Match_line(Foo(). True());
  Match_line(Bar(_; _x; y). y)
)) |};
      [%expect
        {|
        <{ input = Input_unknown; range = {1,85} }>Match(<{ input = Input_unknown; range = {7,8} }>x</{ input = Input_unknown; range = {7,8} }>;
        <{ input = Input_unknown; range = {10,84} }>Match_lines(<{ input = Input_unknown; range = {25,50} }>Match_line(<{ input = Input_unknown; range = {36,41} }>Foo()</{ input = Input_unknown; range = {36,41} }>. <{ input = Input_unknown; range = {43,49} }>True()</{ input = Input_unknown; range = {43,49} }>)</{ input = Input_unknown; range = {25,50} }>; <{ input = Input_unknown; range = {54,82} }>Match_line(<{ input = Input_unknown; range = {65,78} }>Bar(<{ input = Input_unknown; range = {69,70} }>_</{ input = Input_unknown; range = {69,70} }>; <{ input = Input_unknown; range = {72,74} }>_x</{ input = Input_unknown; range = {72,74} }>; <{ input = Input_unknown; range = {76,77} }>y</{ input = Input_unknown; range = {76,77} }>)</{ input = Input_unknown; range = {65,78} }>. <{ input = Input_unknown; range = {80,81} }>y</{ input = Input_unknown; range = {80,81} }>)</{ input = Input_unknown; range = {54,82} }>)</{ input = Input_unknown; range = {10,84} }>)</{ input = Input_unknown; range = {1,85} }> |}]
    ;;

    let%expect_test _ =
      print_parse {|Succ(Ifz(Zero(); x. x; Zero()))|};
      (*            01234567890123456789012345678901
                    0         1         2         3*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,31} }>Succ(<{ input = Input_unknown; range = {5,30} }>Ifz(<{ input = Input_unknown; range = {9,15} }>Zero()</{ input = Input_unknown; range = {9,15} }>; <{ input = Input_unknown; range = {17,18} }>x</{ input = Input_unknown; range = {17,18} }>. <{ input = Input_unknown; range = {20,21} }>x</{ input = Input_unknown; range = {20,21} }>; <{ input = Input_unknown; range = {23,29} }>Zero()</{ input = Input_unknown; range = {23,29} }>)</{ input = Input_unknown; range = {5,30} }>)</{ input = Input_unknown; range = {0,31} }>
    |}]
    ;;

    let ( = ) = Term.equivalent
    let here = Provenance.of_here [%here]
    let parse_exn = parse >> Result.ok_or_failwith

    let%test _ = parse_exn "tm " = Var (here, "tm")
  end)
;;

let%test_module "check" =
  (module struct
    let parse_lang lang_str =
      Lvca_parsing.(parse_string (whitespace *> Abstract_syntax.parse) lang_str)
      |> Result.ok_or_failwith
    ;;

    let parse_term term_str =
      Lvca_parsing.parse_string (Term.parse' String.Set.empty) term_str
      |> Result.ok_or_failwith
    ;;

    let parse_sort str = Lvca_parsing.(parse_string (Sort.parse String.Set.empty) str)

    let lang_desc =
      {|
value :=
  | Unit()
  | Lit_int(integer)
  | Lit_str(string)
  | List(list value)

list a :=
  | Nil()
  | Cons(a; list a)

match_line :=
  | Match_line(value[value]. term)

term :=
  | Lambda(value. term)
  | Alt_lambda(term. term)
  | Match(match_line)
  | Value(value)

test := Foo(term[term]. term)
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
      check_term' "term" "Lambda(a. Value(a))";
      [%expect]
    ;;

    let%expect_test _ =
      check_term' "term" "Unit()";
      [%expect
        {|
      Nominal.check: failed to find operator Unit in sort term
      stack:
      - term: Unit(), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "Lambda(a. b)";
      [%expect
        {|
      Unknown variable b (is it bound?)
      stack:
      - term: Lambda(a. b), sort: term
      - term: b, sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "Lambda(val. Alt_lambda(tm. val))";
      [%expect
        {|
      Variable val has unexpected sort (saw: value) (expected: term)
      stack:
      - term: Lambda(val. Alt_lambda(tm. val)), sort: term
      - term: Alt_lambda(tm. val), sort: term
      - term: val, sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "value" {|Lit_int("foo")|};
      [%expect
        {|
      Unexpected sort (integer) for a primitive ("foo")
      stack:
      - term: Lit_int("foo"), sort: value
      - term: "foo", sort: integer |}]
    ;;

    let%expect_test _ =
      check_term' "value" "Lit_str(123)";
      [%expect
        {|
      Unexpected sort (string) for a primitive (123)
      stack:
      - term: Lit_str(123), sort: value
      - term: 123, sort: string |}]
    ;;

    let%expect_test _ =
      check_term' "term" "Lambda(a; b)";
      [%expect
        {|
      Wrong number of subterms (2) for this arity (value. term)
      stack:
      - term: Lambda(a; b), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "value" "Lit_int(1; 2)";
      [%expect
        {|
      Wrong number of subterms (2) for this arity (integer)
      stack:
      - term: Lit_int(1; 2), sort: value |}]
    ;;

    let%expect_test _ =
      check_term' "match_line" "Match_line(a. b. Value(a))";
      [%expect
        {|
      Wrong number of binders (2) for this valence (value[value]. term) (expected 1)
      stack:
      - term: Match_line(a. b. Value(a)), sort: match_line |}]
    ;;

    let%expect_test _ =
      check_term' "match_line" "Match_line(a. a)";
      [%expect
        {|
      Variable a has unexpected sort (saw: value) (expected: term)
      stack:
      - term: Match_line(a. a), sort: match_line
      - term: a, sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "Lambda(a. b. a)";
      [%expect
        {|
      Wrong number of binders (2) for this valence (value. term) (expected 1)
      stack:
      - term: Lambda(a. b. a), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "Lambda(List(Cons(a; Cons(b; Nil()))). Value(a))";
      [%expect
        {|
      Fixed-valence binders must all be vars (no patterns)
      stack:
      - term: Lambda(List(Cons(a; Cons(b; Nil()))). Value(a)), sort: term |}]
    ;;

    let%expect_test _ =
      check_term' "term" "Match(a. a)";
      [%expect
        {|
      Wrong number of binders (1) for this valence (match_line) (expected 0)
      stack:
      - term: Match(a. a), sort: term |}]
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
