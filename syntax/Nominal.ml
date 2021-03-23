open Base
open Stdio
module Cbor = Lvca_util.Cbor
module Json = Lvca_util.Json
module String = Lvca_util.String
module Tuple2 = Lvca_util.Tuple2

let array_map f args = args |> List.map ~f |> Array.of_list |> Json.array

module rec Term : sig
  type ('info, 'prim) t =
    | Operator of 'info * string * ('info, 'prim) Scope.t list
    | Var of 'info * string
    | Primitive of 'info * 'prim

  val equal
    :  ('info -> 'info -> bool)
    -> ('prim -> 'prim -> bool)
    -> ('info, 'prim) t
    -> ('info, 'prim) t
    -> bool

  val info : ('info, _) t -> 'info

  val pp_generic
    :  open_loc:'info Fmt.t
    -> close_loc:'info Fmt.t
    -> pp_pat:('prim Fmt.t -> ('info, 'prim) Pattern.t Fmt.t)
    -> pp_prim:'prim Fmt.t
    -> ('info, 'prim) t Fmt.t

  val pp : 'prim Fmt.t -> (_, 'prim) t Fmt.t
  val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
  val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t
  val pp_str : 'prim Fmt.t -> (_, 'prim) t -> string
  val jsonify : 'prim Lvca_util.Json.serializer -> (_, 'prim) t Lvca_util.Json.serializer

  val unjsonify
    :  'prim Lvca_util.Json.deserializer
    -> (unit, 'prim) t Lvca_util.Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : 'prim Lvca_util.Json.serializer -> (_, 'prim) t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : 'prim Lvca_util.Json.deserializer -> Bytes.t -> (unit, 'prim) t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : 'prim Lvca_util.Json.serializer -> (_, 'prim) t -> string

  val map_info : f:('a -> 'b) -> ('a, 'prim) t -> ('b, 'prim) t
  val erase : (_, 'prim) t -> (unit, 'prim) t

  (** Attempt to convert a non-binding term to a pattern.

      For example, the term [add(lit(1); a)] is convertible to a pattern, but
      [lambda(a. a)] is not. *)
  val to_pattern
    :  ('info, 'prim) t
    -> (('info, 'prim) Pattern.t, (unit, 'prim) Scope.t) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

      For example [add(lit(1)); a)] (as a pattern) can be converted to a term. *)
  val of_pattern : ('info, 'prim) Pattern.t -> ('info, 'prim) t

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all
    :  ('info, 'prim) t Lvca_util.String.Map.t
    -> ('info, 'prim) t
    -> ('info, 'prim) t

  val select_path
    :  path:int list
    -> ('info, 'prim) t
    -> (('info, 'prim) t, string) Result.t

  val match_pattern
    :  prim_eq:('prim -> 'prim -> bool)
    -> ('info, 'prim) Pattern.t
    -> ('info, 'prim) t
    -> ('info, 'prim) t Lvca_util.String.Map.t option

  val free_vars : (_, _) t -> Lvca_util.String.Set.t

  (** Check that the given term matches the given sort.

      This recursively checks subterms and patterns.

      Checks performed:

      + All used variables must be bound.
      + Variables must have the correct sort at their use site.
      + Primitives must have the correct sort (string / integer).
      + All mentioned operators must appear in the relevant sort.
      + All operators must have the correct number of subterms.
      + Variable-arity terms can have only non-binding terms as children
      + Fixed-valence terms must have the correct number of binders. All must be
        variables.
      + Variable-valence terms must have one binder, a pattern. *)
  val check
    :  'prim Fmt.t
    -> ('info -> 'prim -> 'info Sort.t -> string option) (** Primitive checker *)
    -> 'info AbstractSyntax.t (** Abstract syntax *)
    -> 'info Sort.t (** Sort to check term against *)
    -> ('info, 'prim) t
    -> ('info, (('info, 'prim) Pattern.t, ('info, 'prim) t) Base.Either.t) CheckFailure.t
       option

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
    val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  end

  module Properties : sig
    val json_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
    val string_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val string_round_trip2 : string -> PropertyResult.t
  end

  module Primitive : sig
    (** Hardcoded for the Primitive type *)
    val check
      :  'info AbstractSyntax.t (** Abstract syntax *)
      -> 'info Sort.t (** Sort to check term against *)
      -> ('info, Primitive.t) t
      -> ( 'info
         , (('info, Primitive.t) Pattern.t, ('info, Primitive.t) t) Base.Either.t )
         CheckFailure.t
         option
  end
end = struct
  type ('info, 'prim) t =
    | Operator of 'info * string * ('info, 'prim) Scope.t list
    | Var of 'info * string
    | Primitive of 'info * 'prim

  let equal info_eq prim_eq t1 t2 =
    match t1, t2 with
    | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
      info_eq i1 i2
      && String.(name1 = name2)
      && List.equal (Scope.equal info_eq prim_eq) scopes1 scopes2
    | Primitive (i1, p1), Primitive (i2, p2) -> info_eq i1 i2 && prim_eq p1 p2
    | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
    | _, _ -> false
  ;;

  let info = function
    | Operator (info, _, _) | Var (info, _) | Primitive (info, _) -> info
  ;;

  let pp_generic ~open_loc ~close_loc ~pp_pat ~pp_prim ppf tm =
    let list, string, semi, pf = Fmt.(list, string, semi, pf) in
    open_loc ppf (info tm);
    (match tm with
    | Operator (_, tag, subtms) ->
      pf
        ppf
        "@[<hv>%s(%a)@]"
        tag
        (list ~sep:semi (Scope.pp_generic ~open_loc ~close_loc ~pp_pat ~pp_prim))
        subtms
    | Var (_, v) -> pf ppf "%a" string v
    | Primitive (_, p) -> pf ppf "%a" pp_prim p);
    close_loc ppf (info tm)
  ;;

  let pp pp_prim ppf tm =
    pp_generic
      ~open_loc:(fun _ _ -> ())
      ~close_loc:(fun _ _ -> ())
      ~pp_pat:Pattern.pp
      ~pp_prim
      ppf
      tm
  ;;

  let pp_range pp_prim ppf tm =
    pp_generic
      ~open_loc:OptRange.open_stag
      ~close_loc:OptRange.close_stag
      ~pp_pat:Pattern.pp_range
      ~pp_prim
      ppf
      tm
  ;;

  let pp_ranges pp_prim ppf tm =
    pp_generic
      ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
      ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
      ~pp_pat:Pattern.pp_ranges
      ~pp_prim
      ppf
      tm
  ;;

  let pp_str pp_prim tm = Fmt.to_to_string (pp pp_prim) tm

  let jsonify jsonify_prim tm =
    let array, string = Json.(array, string) in
    match tm with
    | Operator (_, tag, tms) ->
      array [| string "o"; string tag; array_map (Scope.jsonify jsonify_prim) tms |]
    | Var (_, name) -> array [| string "v"; string name |]
    | Primitive (_, p) -> array [| string "p"; jsonify_prim p |]
  ;;

  let unjsonify prim_unjsonify =
    let open Option.Let_syntax in
    Json.(
      function
      | Array [| String "o"; String tag; Array scopes |] ->
        let%map scopes' =
          scopes
          |> Array.to_list
          |> List.map ~f:(Scope.unjsonify prim_unjsonify)
          |> Option.all
        in
        Operator ((), tag, scopes')
      | Array [| String "v"; String name |] -> Some (Var ((), name))
      | Array [| String "p"; prim |] ->
        let%map prim' = prim_unjsonify prim in
        Primitive ((), prim')
      | _ -> None)
  ;;

  let serialize serialize_prim tm = tm |> jsonify serialize_prim |> Cbor.encode

  let deserialize unjsonify_prim buf =
    buf |> Cbor.decode |> Option.bind ~f:(unjsonify unjsonify_prim)
  ;;

  let hash serialize_prim tm = tm |> serialize serialize_prim |> Lvca_util.Sha256.hash

  let map_info ~f = function
    | Operator (info, name, pats) ->
      Operator (f info, name, List.map pats ~f:(Scope.map_info ~f))
    | Var (info, name) -> Var (f info, name)
    | Primitive (info, prim) -> Primitive (f info, prim)
  ;;

  let erase tm = map_info ~f:(fun _ -> ()) tm

  let subst_all ctx tm =
    match tm with
    | Primitive _ -> tm
    | Var (_loc, name) -> (match Map.find ctx name with Some v -> v | None -> tm)
    | Operator (info, name, scopes) ->
      Operator (info, name, List.map scopes ~f:(Scope.subst_all ctx))
  ;;

  let rec match_pattern ~prim_eq pat tm =
    match pat, tm with
    | Pattern.Ignored _, _ -> Some String.Map.empty
    | Var (_, name), tm -> Some (String.Map.singleton name tm)
    | Primitive (_, p1), Primitive (_, p2) ->
      if prim_eq p1 p2 then Some String.Map.empty else None
    | Primitive _, _ -> None
    | Operator (_, name1, pats), Operator (_, name2, scopes) ->
      if String.(name1 = name2)
      then (
        match List.map2 pats scopes ~f:(match_scope ~prim_eq) with
        | Ok zipped ->
          (match String.Map.join_helper zipped with
          | `Ok result -> result
          | `Duplicate_key k ->
            failwith (Printf.sprintf "invariant violation: duplicate key: %s" k))
        | Unequal_lengths -> None)
      else None
    | _ -> None

  and match_scope ~prim_eq pat (Scope (binders, tm)) =
    match binders with [] -> match_pattern ~prim_eq pat tm | _ -> None
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

  let check pp_prim check_prim lang =
    let lookup_operator = AbstractSyntax.lookup_operator lang in
    let check_pattern = Pattern.check pp_prim check_prim lang in
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
        | Primitive (info, p) ->
          (match check_prim info p expected_sort with
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
    | Primitive (info, prim) -> Ok (Primitive (info, prim))

  and scope_to_patterns = function
    | Scope ([], tm) -> to_pattern tm
    | Scope (binders, tm) ->
      let binders' = List.map binders ~f:Pattern.erase in
      let tm = erase tm in
      Error (Scope.Scope (binders', tm))
  ;;

  let rec of_pattern = function
    | Pattern.Operator (info, name, pats) ->
      Operator
        ( info
        , name (* TODO: should the scope really inherit the 'info? *)
        , List.map pats ~f:(fun pat -> Scope.Scope ([], of_pattern pat)) )
    | Primitive (info, prim) -> Primitive (info, prim)
    | Var (info, name) -> Var (info, name)
    | Ignored (info, name) -> Var (info, "_" ^ name)
  ;;

  module Primitive' = Primitive

  module Primitive = struct
    let check lang sort pat = check Primitive.pp Primitive.check lang sort pat
  end

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)

    (* module Primitive = Primitive'.Parse (Comment) *)

    type 'prim tm_or_sep =
      | Tm of (OptRange.t, 'prim) t
      | Sep of char

    type ('info, 'prim) term = ('info, 'prim) t

    let t : 'prim ParseUtil.t -> (OptRange.t, 'prim) term ParseUtil.t =
     fun parse_prim ->
      let open Parsers in
      fix (fun term ->
          let t_or_sep : 'prim tm_or_sep ParseUtil.t =
            choice
              [ (fun c -> Sep c) <$> choice [ char '.'; char ';' ]
              ; (fun tm -> Tm tm) <$> term
              ]
          in
          (* (b11. ... b1n. t11, ... t1n; b21. ... b2n. t21, ... t2n) *)
          let accumulate
              :  OptRange.t -> string -> 'prim tm_or_sep list
              -> (OptRange.t, 'prim) term ParseUtil.t
            =
           fun range tag tokens ->
            (* terms encountered between '.'s, before hitting ',' / ';' *)
            let binding_queue : (OptRange.t, 'prim) term Queue.t = Queue.create () in
            (* scopes encountered *)
            let scope_queue : (OptRange.t, 'prim) Scope.t Queue.t = Queue.create () in
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
            [ (parse_prim
              >>|| fun ~pos prim -> Primitive (OptRange.extend_to pos p1, prim), pos)
            ; (identifier
              >>= fun ident ->
              choice
                [ (parens (many t_or_sep)
                  >>= fun tokens ->
                  pos >>= fun p2 -> accumulate (OptRange.mk p1 p2) ident tokens)
                ; (pos >>| fun p2 -> Var (OptRange.mk p1 p2, ident))
                ])
            ])
      <?> "term"
   ;;

    let whitespace_t parse_prim = Parsers.(junk *> t parse_prim)
  end

  module Properties = struct
    module Parse = Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive'.Parse (ParseUtil.NoComment)
    open PropertyResult
    module Primitive = Primitive'

    let pp = pp Primitive.pp
    let parse = ParseUtil.parse_string (Parse.t ParsePrimitive.t)

    let json_round_trip1 t =
      match t |> jsonify Primitive.jsonify |> unjsonify Primitive.unjsonify with
      | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
      | Some t' -> PropertyResult.check Caml.(t = t') (Fmt.str "%a <> %a" pp t' pp t)
    ;;

    let json_round_trip2 json =
      match json |> unjsonify Primitive.unjsonify with
      | None -> Uninteresting
      | Some t ->
        PropertyResult.check
          Lvca_util.Json.(jsonify Primitive.jsonify t = json)
          "jsonify t <> json (TODO: print)"
    ;;

    let string_round_trip1 t =
      match t |> pp_str Primitive.pp |> parse with
      | Ok t' ->
        let t'' = erase t' in
        PropertyResult.check Caml.(t'' = t) (Fmt.str "%a <> %a" pp t'' pp t)
      | Error msg ->
        Failed (Fmt.str {|parse_string "%s": %s|} (pp_str Primitive.pp t) msg)
    ;;

    let string_round_trip2 str =
      match parse str with
      | Error _ -> Uninteresting
      | Ok t ->
        let str' = pp_str Primitive.pp t in
        if Base.String.(str' = str)
        then Ok
        else (
          match parse str with
          | Error msg -> Failed msg
          | Ok t' ->
            let str'' = pp_str Primitive.pp t' in
            PropertyResult.check
              String.(str'' = str')
              (Fmt.str {|"%s" <> "%s"|} str'' str'))
    ;;

    (* malformed input *)
  end
end

and Scope : sig
  type ('info, 'prim) t = Scope of ('info, 'prim) Pattern.t list * ('info, 'prim) Term.t

  val equal
    :  ('info -> 'info -> bool)
    -> ('prim -> 'prim -> bool)
    -> ('info, 'prim) t
    -> ('info, 'prim) t
    -> bool

  val pp_generic
    :  open_loc:'info Fmt.t
    -> close_loc:'info Fmt.t
    -> pp_pat:('prim Fmt.t -> ('info, 'prim) Pattern.t Fmt.t)
    -> pp_prim:'prim Fmt.t
    -> ('info, 'prim) t Fmt.t

  val pp : 'prim Fmt.t -> (_, 'prim) t Fmt.t
  val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
  val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t
  val pp_str : 'prim Fmt.t -> (_, 'prim) t -> string
  val jsonify : 'prim Lvca_util.Json.serializer -> (_, 'prim) t Lvca_util.Json.serializer

  val unjsonify
    :  'prim Lvca_util.Json.deserializer
    -> (unit, 'prim) t Lvca_util.Json.deserializer

  val map_info : f:('a -> 'b) -> ('a, 'prim) t -> ('b, 'prim) t
  val erase : (_, 'prim) t -> (unit, 'prim) t

  (* TODO?
val to_pattern
  :  ('info, 'prim) t
  -> (('info, 'prim) Pattern.t, (unit, 'prim) scope) Result.t

val of_pattern : ('info, 'prim) Pattern.t -> ('info, 'prim) t
  *)

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all
    :  ('info, 'prim) Term.t Lvca_util.String.Map.t
    -> ('info, 'prim) t
    -> ('info, 'prim) t

  (* TODO
  val free_vars : (_, _) t -> Lvca_util.String.Set.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
    val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  end

  module Properties : sig
    val json_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
    val string_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val string_round_trip2 : string -> PropertyResult.t
  end
  *)
end = struct
  type ('info, 'prim) t = Scope of ('info, 'prim) Pattern.t list * ('info, 'prim) Term.t

  let equal info_eq prim_eq (Scope (pats1, tm1)) (Scope (pats2, tm2)) =
    List.equal (Pattern.equal info_eq prim_eq) pats1 pats2
    && Term.equal info_eq prim_eq tm1 tm2
  ;;

  let pp_generic ~open_loc ~close_loc ~pp_pat ~pp_prim ppf (Scope (bindings, body)) =
    let any, list, pf = Fmt.(any, list, pf) in
    let pp_body = Term.pp_generic ~open_loc ~close_loc ~pp_pat ~pp_prim in
    match bindings with
    | [] -> pp_body ppf body
    | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") (pp_pat pp_prim)) bindings pp_body body
  ;;

  let pp pp_prim ppf tm =
    pp_generic
      ~open_loc:(fun _ _ -> ())
      ~close_loc:(fun _ _ -> ())
      ~pp_pat:Pattern.pp
      ~pp_prim
      ppf
      tm
  ;;

  let pp_range pp_prim ppf tm =
    pp_generic
      ~open_loc:OptRange.open_stag
      ~close_loc:OptRange.close_stag
      ~pp_pat:Pattern.pp_range
      ~pp_prim
      ppf
      tm
  ;;

  let pp_ranges pp_prim ppf tm =
    pp_generic
      ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
      ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
      ~pp_pat:Pattern.pp_ranges
      ~pp_prim
      ppf
      tm
  ;;

  let pp_str pp_prim scope = Fmt.to_to_string (pp pp_prim) scope

  and jsonify jsonify_prim (Scope (pats, body)) : Json.t =
    let body' = Term.jsonify jsonify_prim body in
    Json.array [| array_map (Pattern.jsonify jsonify_prim) pats; body' |]
  ;;

  let unjsonify prim_unjsonify =
    Json.(
      function
      | Array [||] -> None
      | Array arr ->
        let open Option.Let_syntax in
        let binders, body = arr |> Array.to_list |> Lvca_util.List.unsnoc in
        let%bind binders' =
          binders |> List.map ~f:(Pattern.unjsonify prim_unjsonify) |> Option.all
        in
        let%bind body' = Term.unjsonify prim_unjsonify body in
        Some (Scope (binders', body'))
      | _ -> None)
  ;;

  let map_info ~f (Scope (binders, tm)) =
    let binders' = List.map binders ~f:(Pattern.map_info ~f) in
    let tm = Term.map_info ~f tm in
    Scope (binders', tm)
  ;;

  let erase (Scope (pats, tm)) = Scope (List.map pats ~f:Pattern.erase, Term.erase tm)
  let subst_all ctx (Scope (pats, tm)) = Scope (pats, Term.subst_all ctx tm)
end

let%test_module "Nominal" =
  (module struct
    let print_serialize tm =
      let bytes = Term.serialize Primitive.jsonify tm in
      bytes
      |> Bytes.to_array
      |> Array.iter ~f:(fun char -> printf "%02x" (Char.to_int char))
    ;;

    let print_hash tm = printf "%s" (Term.hash Primitive.jsonify tm)
    let ( = ) = Caml.( = )
    let tm = Term.Var ((), "x")
    let j_tm = Json.(Array [| String "v"; String "x" |])

    let%test _ = Term.jsonify Primitive.jsonify tm = j_tm

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 8261766178 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6 |}]
    ;;

    let tm = Term.Operator ((), "Z", [])

    let%test _ =
      Term.jsonify Primitive.jsonify tm
      = Json.(Array [| String "o"; String "Z"; Array [||] |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615a80 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c |}]
    ;;

    let tm = Term.Operator ((), "S", [ Scope ([ Var ((), "x") ], Var ((), "x")) ])

    let%test _ =
      Term.jsonify Primitive.jsonify tm
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

    let tm = Term.Primitive ((), Primitive.PrimInteger (Z.of_string "12345"))

    let%test _ =
      Term.jsonify Primitive.jsonify tm
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

    let parse = ParseUtil.parse_string (ParseNominal.whitespace_t ParsePrimitive.t)

    let print_parse str =
      match parse str with
      | Error msg -> print_string ("failed: " ^ msg)
      | Ok tm ->
        Fmt.pr "%a\n" (Term.pp Primitive.pp) tm;
        Fmt.pr "%a" (Term.pp_range Primitive.pp) tm
    ;;

    let%test _ = parse "x" |> Result.map ~f:Term.erase = Ok (Var ((), "x"))

    let%test _ =
      parse "123"
      |> Result.map ~f:Term.erase
      = Ok (Primitive ((), PrimInteger (Z.of_int 123)))
    ;;

    let%test _ =
      parse "\"abc\"" |> Result.map ~f:Term.erase = Ok (Primitive ((), PrimString "abc"))
    ;;

    let x = Term.Var ((), "x")
    let t = Term.Operator ((), "true", [])

    let%test _ =
      parse "lam(x. x)"
      |> Result.map ~f:Term.erase
      = Ok (Operator ((), "lam", [ Scope ([ Var ((), "x") ], x) ]))
    ;;

    let match_line a b = Term.Operator ((), "match_line", [ Scope ([ a ], b) ])

    let match_lines subtms =
      Term.Operator
        ((), "match_lines", Base.List.map subtms ~f:(fun tm -> Scope.Scope ([], tm)))
    ;;

    let%test _ =
      parse {| match() |} |> Result.map ~f:Term.erase = Ok (Operator ((), "match", []))
    ;;

    let%test _ =
      parse {| match(x; x) |}
      |> Result.map ~f:Term.erase
      = Ok (Operator ((), "match", [ Scope ([], x); Scope ([], x) ]))
    ;;

    let%test _ =
      parse {| match(true(); true()) |}
      |> Result.map ~f:Term.erase
      = Ok (Operator ((), "match", [ Scope ([], t); Scope ([], t) ]))
    ;;

    let%test _ =
      parse {| match(x;) |}
      |> Result.map ~f:Term.erase
      = Ok (Operator ((), "match", [ Scope ([], x) ]))
    ;;

    let%test _ =
      parse
        {|
    match(x; match_lines(
      match_line(foo(). true());
      match_line(bar(_; _x; y). y)
    )) |}
      |> Result.map ~f:Term.erase
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
      ParseUtil.parse_string (Parser.t ParsePrimitive.t) term_str |> Result.ok_or_failwith
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
            Fmt.pf
              ppf
              "- @[pattern: %a,@ sort: %a@]"
              (Pattern.pp Primitive.pp)
              pat
              Sort.pp
              sort
          | Second tm ->
            Fmt.pf ppf "- @[term: %a,@ sort: %a@]" (Term.pp Primitive.pp) tm Sort.pp sort
        in
        (match tm_str |> parse_term |> Term.Primitive.check language sort with
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
