open Base
module Util = Lvca_util
module String = Util.String
module Format = Stdlib.Format
module SMap = Lvca_util.String.Map
open Option.Let_syntax

type ('info, 'prim) t =
  | Operator of 'info * string * ('info, 'prim) scope list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

and ('info, 'prim) scope = Scope of ('info * string) list * ('info, 'prim) t

type 'info capture_type =
  | BoundVar of 'info Sort.t
  | BoundPattern of 'info AbstractSyntax.pattern_sort
  | BoundTerm of 'info Sort.t

type ('info, 'prim) capture =
  | CapturedBinder of ('info, 'prim) Pattern.t
  | CapturedTerm of ('info, 'prim) Nominal.term

let capture_eq ~info_eq ~prim_eq cap1 cap2 =
  match cap1, cap2 with
  | CapturedBinder pat1, CapturedBinder pat2 -> Pattern.equal info_eq prim_eq pat1 pat2
  | CapturedTerm tm1, CapturedTerm tm2 -> Nominal.equal info_eq prim_eq tm1 tm2
  | _, _ -> false
;;

let rec equal info_eq prim_eq t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (scope_eq info_eq prim_eq) scopes1 scopes2
  | Primitive (i1, p1), Primitive (i2, p2) -> info_eq i1 i2 && prim_eq p1 p2
  | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | Ignored (i1, name1), Ignored (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false

and scope_eq info_eq prim_eq (Scope (names1, t1)) (Scope (names2, t2)) =
  List.equal (Lvca_util.Tuple2.equal info_eq String.( = )) names1 names2
  && equal info_eq prim_eq t1 t2
;;

let rec vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats |> List.map ~f:vars_of_scope |> Set.union_list (module String)
  | Primitive _ -> String.Set.empty
  | Var (_, name) -> String.Set.of_list [ name ]
  | Ignored _ -> String.Set.empty

and vars_of_scope (Scope (vars, t)) =
  let vars = vars |> List.map ~f:snd in
  Set.union (String.Set.of_list vars) (vars_of_pattern t)
;;

let rec list_vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats
    (* simpler, morally equivalent: List.concat_map ~f:(List.concat_map
       ~f:list_vars_of_scope) *)
    |> List.map ~f:list_vars_of_scope
    |> List.concat_no_order
  | Primitive _ -> []
  | Var (loc, name) -> [ loc, name ]
  | Ignored _ -> []

and list_vars_of_scope (Scope (names, pat)) =
  let names' = List.map names ~f:snd in
  let pat_vars =
    pat
    |> list_vars_of_pattern
    |> List.filter ~f:(fun (_i, name) -> not (List.mem names' name ~equal:String.( = )))
  in
  List.unordered_append names pat_vars
;;

let rec map_info ~f = function
  | Operator (info, tag, subpats) ->
    Operator (f info, tag, subpats |> List.map ~f:(scope_map_info ~f))
  | Primitive (info, prim) -> Primitive (f info, prim)
  | Var (info, name) -> Var (f info, name)
  | Ignored (info, name) -> Ignored (f info, name)

and scope_map_info ~f (Scope (names, t)) =
  Scope (List.map names ~f:(fun (i, name) -> f i, name), map_info ~f t)
;;

let erase pat = map_info ~f:(fun _ -> ()) pat

let info = function
  | Operator (i, _, _) | Primitive (i, _) | Var (i, _) | Ignored (i, _) -> i
;;

let any, list, string, semi, pf = Fmt.(any, list, string, semi, pf)

let rec pp_generic ~open_loc ~close_loc ~pp_prim ppf tm =
  open_loc ppf (info tm);
  (match tm with
  | Operator (_, tag, subtms) ->
    pf
      ppf
      "@[<hv>%s(%a)@]"
      tag
      (list ~sep:semi (pp_scope_generic ~open_loc ~close_loc ~pp_prim))
      subtms
  | Var (_, v) -> pf ppf "%a" string v
  | Ignored (_, v) -> pf ppf "_%a" string v
  | Primitive (_, p) -> pf ppf "%a" pp_prim p);
  close_loc ppf (info tm)

and pp_scope_generic ~open_loc ~close_loc ~pp_prim ppf (Scope (bindings, body)) =
  let pp_body = pp_generic ~open_loc ~close_loc ~pp_prim in
  let pp_binding ppf (info, name) =
    open_loc ppf info;
    string ppf name;
    close_loc ppf info
  in
  match bindings with
  | [] -> pp_body ppf body
  | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") pp_binding) bindings pp_body body
;;

let pp pp_prim ppf tm =
  pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ~pp_prim ppf tm
;;

let pp_scope pp_prim ppf tm =
  pp_scope_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ~pp_prim ppf tm
;;

let pp_range pp_prim ppf tm =
  pp_generic ~open_loc:OptRange.open_stag ~close_loc:OptRange.close_stag ~pp_prim ppf tm
;;

let pp_scope_range pp_prim ppf tm =
  pp_scope_generic
    ~open_loc:OptRange.open_stag
    ~close_loc:OptRange.close_stag
    ~pp_prim
    ppf
    tm
;;

let pp_ranges pp_prim ppf tm =
  pp_generic
    ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
    ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
    ~pp_prim
    ppf
    tm
;;

let pp_scope_ranges pp_prim ppf tm =
  pp_scope_generic
    ~open_loc:(fun ppf info -> Stdlib.Format.pp_open_stag ppf (SourceRanges.Stag info))
    ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
    ~pp_prim
    ppf
    tm
;;

(*
let to_string pp_prim pat = Fmt.str "%a" (pp pp_prim) pat
let scope_to_string pp_prim scope = Fmt.str "%a" (pp_scope pp_prim) scope
*)

let pp_capture pp_prim ppf = function
  | CapturedBinder pat -> Pattern.pp pp_prim ppf pat
  | CapturedTerm pat -> Nominal.pp_term pp_prim ppf pat
;;

let rec select_path ~path pat =
  match path with
  | [] -> Ok pat
  | i :: path ->
    (match pat with
    | Var _ | Ignored _ | Primitive _ -> Error "TODO: message"
    | Operator (_, _, scopes) ->
      (match List.nth scopes i with
      | None -> Error "TODO: message"
      | Some (Scope (_vars, pat)) -> select_path ~path pat))
;;

let rec match_term ~prim_eq pat tm =
  match pat, tm with
  | Ignored _, _ -> Some SMap.empty
  | Var (_, name), tm -> Some (SMap.singleton name (CapturedTerm tm))
  | Primitive (_, p1), Nominal.Primitive (_, p2) ->
    if prim_eq p1 p2 then Some SMap.empty else None
  | Operator (_, name1, pat_scopes), Operator (_, name2, tm_scopes) ->
    if String.(name1 = name2)
    then (
      match List.map2 pat_scopes tm_scopes ~f:(match_scope ~prim_eq) with
      | Unequal_lengths -> None
      | Ok zipped ->
        (match Option.all zipped with
        | Some list ->
          (match Lvca_util.String.Map.strict_unions list with
          | `Ok result -> Some result
          | `Duplicate_key _k -> failwith "TODO")
        | None -> None))
    else None
  | _, _ -> None

and match_scope ~prim_eq (Scope (binder_pats, body_pat)) (Nominal.Scope (binders, body)) =
  let f (_, name) pat =
    if Char.(name.[0] = '_') then None else Some (name, CapturedBinder pat)
  in
  match List.map2 binder_pats binders ~f with
  | List.Or_unequal_lengths.Unequal_lengths -> None
  | Ok zipped ->
    let%bind binder_captures = Option.all zipped in
    let%bind binder_captures =
      match SMap.of_alist binder_captures with
      | `Duplicate_key _ -> None (* TODO: error *)
      | `Ok binder_captures -> Some binder_captures
    in
    let%bind body_match = match_term ~prim_eq body_pat body in
    (match SMap.strict_union binder_captures body_match with
    | `Duplicate_key _ -> None (* TODO: error *)
    | `Ok result -> Some result)
;;

let handle_dup_error = function
  | `Ok result -> Ok result
  | `Duplicate_key k ->
    Error
      (CheckFailure.err
         (Printf.sprintf
            "Did you mean to bind the same variable (%s) twice in the same pattern? \
             That's not allowed!"
            k))
;;

let check check_prim lang sort =
  let lookup_operator = AbstractSyntax.lookup_operator lang in
  let rec check sort pat =
    let result =
      match pat with
      | Var (_, name) -> Ok (SMap.singleton name (BoundTerm sort))
      | Ignored _ -> Ok SMap.empty
      | Primitive (info, prim) ->
        (match check_prim info prim sort with
        | None -> Ok SMap.empty
        | Some msg -> Error (CheckFailure.err msg))
      | Operator (_, op_name, subpats) ->
        let sort_name, sort_args =
          match sort with
          | Sort.Name (_, sort_name) -> sort_name, []
          | Sort.Ap (_, sort_name, sort_args) -> sort_name, sort_args
        in
        (match lookup_operator sort_name op_name with
        | None ->
          Error
            (CheckFailure.err
               (Printf.sprintf
                  "Pattern.check: failed to find operator %s in sort %s"
                  op_name
                  sort_name))
        | Some (sort_vars, OperatorDef (_, arity)) ->
          let sort_env = SMap.of_alist_exn (List.zip_exn sort_vars sort_args) in
          check_slots (AbstractSyntax.instantiate_arity sort_env arity) subpats)
    in
    Result.map_error result ~f:(fun CheckFailure.{ message; stack } ->
        CheckFailure.{ message; stack = { term = pat; sort } :: stack })
  and check_slots valences scopes =
    match List.zip scopes valences with
    | Unequal_lengths ->
      Error
        (CheckFailure.err
           (Printf.sprintf
              "Wrong number of subterms (%u) for this arity (%s)"
              (List.length scopes)
              (valences
              |> List.map ~f:AbstractSyntax.string_of_valence
              |> String.concat ~sep:", ")))
    | Ok scope_valences ->
      scope_valences
      |> List.map ~f:(fun (scope, valence) -> check_scope valence scope)
      |> Result.all
      |> Result.map ~f:SMap.strict_unions
      |> Result.bind ~f:handle_dup_error
  and check_scope valence (Scope (binders, body)) =
    let (Valence (binder_slots, body_sort)) = valence in
    match List.zip binder_slots binders with
    | Unequal_lengths ->
      Error
        (CheckFailure.err
           (Printf.sprintf
              "Wrong number of binders (%u) for this valence (%s) (expected %u)"
              (List.length binders)
              (AbstractSyntax.string_of_valence valence)
              (List.length binder_slots)))
    | Ok binders ->
      let binders_env =
        binders
        |> List.map ~f:(fun (slot, (_, v)) ->
               let binding_type =
                 match slot with
                 | SortBinding sort -> BoundVar sort
                 | SortPattern pattern_sort -> BoundPattern pattern_sort
               in
               v, binding_type)
      in
      (* Check the body with the new binders environment *)
      (match SMap.of_alist binders_env with
      | `Ok env ->
        (match check body_sort body with
        | Error msg -> Error msg
        | Ok env' ->
          (match SMap.strict_union env env' with
          | `Ok env -> Ok env
          | `Duplicate_key k ->
            failwith (Printf.sprintf "invariant violation -- duplicate key: %s" k)))
      | `Duplicate_key k ->
        Error
          (CheckFailure.err
             (Printf.sprintf
                "Did you mean to bind the same variable (%s) twice in the same set of \
                 patterns? That's not allowed!"
                k)))
  in
  check sort
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  type ('info, 'prim) pattern = ('info, 'prim) t

  module Parsers = ParseUtil.Mk (Comment)
  module Primitive = Primitive.Parse (Comment)

  let to_var = function
    | Var (i, name) -> Ok (i, name)
    (* TODO: we never parse ignored *)
    | Ignored (i, name) -> Ok (i, "_" ^ name)
    | _ -> Error "not a var"
  ;;

  type 'prim tm_or_sep =
    | Tm of (OptRange.t, 'prim) pattern
    | Sep of char

  let t : 'prim ParseUtil.t -> (OptRange.t, 'prim) pattern ParseUtil.t =
   fun parse_prim ->
    let open Parsers in
    fix (fun pat ->
        let t_or_sep : 'prim tm_or_sep ParseUtil.t =
          choice
            [ (fun c -> Sep c) <$> choice [ char '.'; char ';' ]
            ; (fun tm -> Tm tm) <$> pat
            ]
        in
        (* (b11. ... b1n. t11, ... t1n; b21. ... b2n. t21, ... t2n) *)
        let accumulate
            :  OptRange.t -> string -> 'prim tm_or_sep list
            -> (OptRange.t, 'prim) pattern ParseUtil.t
          =
         fun range tag tokens ->
          (* vars encountered between '.'s, before hitting ',' / ';' *)
          let binding_queue : (OptRange.t, 'prim) pattern Queue.t = Queue.create () in
          (* scopes encountered *)
          let scope_queue : (OptRange.t, 'prim) scope Queue.t = Queue.create () in
          let rec go = function
            | [] -> return ~pos:range (Operator (range, tag, Queue.to_list scope_queue))
            | Tm tm :: Sep '.' :: rest ->
              Queue.enqueue binding_queue tm;
              go rest
            | Tm tm :: Sep ';' :: rest (* Note: allow trailing ';' *)
            | Tm tm :: ([] as rest) ->
              (match
                 binding_queue |> Queue.to_list |> List.map ~f:to_var |> Result.all
               with
              | Error _ ->
                fail "Unexpectedly found a variable binding in pattern position"
              | Ok binders ->
                Queue.clear binding_queue;
                Queue.enqueue scope_queue (Scope (binders, tm));
                go rest)
            | _ -> fail "Malformed pattern"
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
              ; (pos
                >>| fun p2 ->
                if Char.(ident.[0] = '_')
                then Ignored (OptRange.mk p1 p2, String.subo ident ~pos:1)
                else Var (OptRange.mk p1 p2, ident))
              ])
          ])
    <?> "binding-aware pattern"
 ;;

  let whitespace_t parse_prim = Parsers.(junk *> t parse_prim)
end

module Properties = struct
  module ParsePattern = Parse (ParseUtil.NoComment)
  module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)
  open PropertyResult

  let parse = ParseUtil.parse_string (ParsePattern.t ParsePrimitive.t)
  let pp = pp Primitive.pp
  let to_string = Fmt.str "%a" pp

  let string_round_trip1 t =
    match t |> to_string |> parse with
    | Ok t' ->
      let t' = erase t' in
      PropertyResult.check Caml.(t' = t) (Fmt.str "%a <> %a" pp t' pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = t |> erase |> to_string in
      if Base.String.(str' = str)
      then Ok
      else (
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = t' |> erase |> to_string in
          PropertyResult.check String.(str'' = str') (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end

let%test_module "Parsing" =
  (module struct
    module Parser = Parse (ParseUtil.NoComment)
    module PrimParser = Primitive.Parse (ParseUtil.NoComment)

    let () =
      Format.set_formatter_stag_functions Range.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let print_parse tm =
      match ParseUtil.parse_string (Parser.t PrimParser.t) tm with
      | Ok pat -> Fmt.pr "%a\n%a" (pp Primitive.pp) pat (pp_range Primitive.pp) pat
      | Error msg -> Fmt.pr "failed: %s\n" msg
    ;;

    let%expect_test _ =
      print_parse {|"str"|};
      [%expect {|
      "str"
      <{0,5}>"str"</{0,5}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      [%expect {|
      a()
      <{0,3}>a()</{0,3}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      [%expect {|
      a(b)
      <{0,4}>a(<{2,3}>b</{2,3}>)</{0,4}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c)|};
      (*0123456*)
      [%expect {|
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
      print_parse {|a(b.c.d;e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b. c. d; e)
      <{0,11}>a(<{2,3}>b</{2,3}>. <{4,5}>c</{4,5}>. <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>)</{0,11}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;;c)|};
      [%expect {| failed: : end_of_input |}]
    ;;
  end)
;;

let%test_module "check" =
  (module struct
    module AbstractSyntaxParse = AbstractSyntax.Parse (ParseUtil.NoComment)
    module Parser = Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)
    module SortParse = Sort.Parse (ParseUtil.NoComment)

    let parse_lang lang_str =
      ParseUtil.parse_string AbstractSyntaxParse.whitespace_t lang_str
      |> Result.ok_or_failwith
    ;;

    let parse_pattern str =
      ParseUtil.parse_string (Parser.t ParsePrimitive.t) str |> Result.ok_or_failwith
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

    let print_check_pattern sort_str pat_str =
      let sort = parse_sort sort_str |> Result.ok_or_failwith in
      let pat = parse_pattern pat_str in
      let pp ppf CheckFailure.{ term = pat; sort } =
        Fmt.pf ppf "- @[pattern: %a,@ sort: %a@]" (pp Primitive.pp) pat Sort.pp sort
      in
      match check Primitive.check language sort pat with
      | Error failure -> Fmt.epr "%a" (CheckFailure.pp pp) failure
      | Ok capture_types ->
        capture_types
        |> Map.iteri ~f:(fun ~key ~data ->
               let rhs =
                 match data with
                 | BoundVar sort -> Sort.to_string sort
                 | BoundPattern { pattern_sort; var_sort } ->
                   (* XXX make pattern_sort printer public *)
                   Printf.sprintf
                     "%s[%s]"
                     (Sort.to_string pattern_sort)
                     (Sort.to_string var_sort)
                 | BoundTerm sort -> Sort.to_string sort
               in
               Stdio.printf "%s: %s\n" key rhs)
    ;;

    let%expect_test _ =
      print_check_pattern "value" "unit()";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "lit_int(1)";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "value" {|lit_str("str")|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "list value" {|nil()|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "list value" {|cons(unit(); nil())|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "term" {|value(list(cons(unit(); nil())))|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "term" {|value(list(cons(a; nil())))|};
      [%expect {| a: value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" {|value(list(cons(a; cons(a; _))))|};
      [%expect
        {|
      Did you mean to bind the same variable (a) twice in the same pattern? That's not allowed!
      stack:
      - pattern: value(list(cons(a; cons(a; _)))), sort: term
      - pattern: list(cons(a; cons(a; _))), sort: value
      - pattern: cons(a; cons(a; _)), sort: list value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "lambda(x. body)";
      [%expect {|
      body: term
      x: value
      |}]
    ;;

    let%expect_test _ =
      print_check_pattern "match_line" "match_line(pat. body)";
      [%expect {|
      body: term
      pat: value[value]
      |}]
    ;;

    let%expect_test _ =
      print_check_pattern "integer" {|"foo"|};
      [%expect
        {|
      Unexpected sort (integer) for a primitive ("foo")
      stack:
      - pattern: "foo", sort: integer |}]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "foo()";
      [%expect
        {|
      Pattern.check: failed to find operator foo in sort value
      stack:
      - pattern: foo(), sort: value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "unit(1)";
      [%expect
        {|
      Wrong number of subterms (1) for this arity ()
      stack:
      - pattern: unit(1), sort: value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "lambda(1)";
      [%expect
        {|
      Wrong number of binders (0) for this valence (value. term) (expected 1)
      stack:
      - pattern: lambda(1), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "match(a; b)";
      [%expect
        {|
      Wrong number of subterms (2) for this arity (match_line)
      stack:
      - pattern: match(a; b), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "integer" "1";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "float" "1.";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "char" "'a'";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "string" {|"str"|};
      [%expect]
    ;;
  end)
;;

let%test_module "check" =
  (module struct
    module Parser = Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)
    module ParseNominal = Nominal.Parse (ParseUtil.NoComment)
    module SortParse = Sort.Parse (ParseUtil.NoComment)

    let parse_pattern str =
      ParseUtil.parse_string (Parser.t ParsePrimitive.t) str |> Result.ok_or_failwith
    ;;

    let parse_term str =
      ParseUtil.parse_string (ParseNominal.t ParsePrimitive.t) str
      |> Result.ok_or_failwith
    ;;

    let print_match pat_str tm_str =
      let pat = parse_pattern pat_str in
      let tm = parse_term tm_str in
      match match_term ~prim_eq:Primitive.( = ) pat tm with
      | None -> ()
      | Some mapping ->
        mapping
        |> Map.iteri ~f:(fun ~key ~data ->
               Fmt.pr "%s -> %a\n" key (pp_capture Primitive.pp) data)
    ;;

    let%expect_test _ =
      print_match "foo()" "foo()";
      [%expect]
    ;;

    let%expect_test _ =
      print_match "lam(a. b)" "lam(x. y)";
      [%expect {|
        a -> x
        b -> y |}]
    ;;

    let%expect_test _ =
      print_match "match(a. b)" "match(foo(bar(); baz()). x)";
      [%expect {|
        a -> foo(bar(); baz())
        b -> x |}]
    ;;
  end)
;;
