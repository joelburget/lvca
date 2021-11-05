open Base
open Lvca_provenance
open Option.Let_syntax
module Util = Lvca_util
module String = Util.String
module Format = Stdlib.Format
module SMap = Util.String.Map
module Tuple2 = Util.Tuple2
module Term = Nominal.Term
module Scope = Nominal.Scope

let ( >> ) = Lvca_util.(( >> ))

module Capture_type = struct
  type t =
    | Bound_var of Sort.t
    | Bound_pattern of Pattern_sort.t
    | Bound_term of Sort.t

  let pp ppf = function
    | Bound_pattern pattern_sort -> Pattern_sort.pp ppf pattern_sort
    | Bound_var sort | Bound_term sort -> Sort.pp ppf sort
  ;;
end

module Capture = struct
  type t =
    | Binder of Pattern.t
    | Term of Term.t

  let equivalent ?(info_eq = fun _ _ -> true) cap1 cap2 =
    match cap1, cap2 with
    | Binder pat1, Binder pat2 -> Pattern.equivalent ~info_eq pat1 pat2
    | Term tm1, Term tm2 -> Term.equivalent ~info_eq tm1 tm2
    | _, _ -> false
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
  let pp ppf = function Binder pat -> Pattern.pp ppf pat | Term tm -> Term.pp ppf tm
end

type t =
  | Operator of Provenance.t * string * scope list
  | Primitive of Primitive.All.t
  | Var of Provenance.t * string

and scope = Scope of (Provenance.t * string) list * t

let rec to_nominal = function
  | Operator (p, name, scopes) ->
    Nominal.Term.Operator
      (Provenance.calculated_here [%here] [ p ], name, List.map ~f:scope_to_nominal scopes)
  | Primitive p -> Primitive p
  | Var (p, name) -> Var (Provenance.calculated_here [%here] [ p ], name)

and scope_to_nominal (Scope (binders, body)) =
  let binders = List.map binders ~f:(fun (p, name) -> Pattern.Var (p, name)) in
  Nominal.Scope.Scope (binders, to_nominal body)
;;

let rec of_nominal tm =
  let open Result.Let_syntax in
  match tm with
  | Nominal.Term.Operator (p, name, scopes) ->
    let%map scopes = scopes |> List.map ~f:scope_of_nominal |> Result.all in
    Operator (Provenance.calculated_here [%here] [ p ], name, scopes)
  | Primitive p -> Ok (Primitive p)
  | Var (p, name) -> Ok (Var (Provenance.calculated_here [%here] [ p ], name))

and scope_of_nominal (Nominal.Scope.Scope (binders, body) as scope) =
  let open Result.Let_syntax in
  let f = function
    | Pattern.Var (p, name) -> Ok (p, name)
    | _ ->
      Error
        (Nominal.Conversion_error.mk_Scope ~provenance:(Provenance.of_here [%here]) scope)
  in
  let%bind binders = binders |> List.map ~f |> Result.all in
  let%map body = of_nominal body in
  Scope (binders, body)
;;

let rec equivalent ?(info_eq = fun _ _ -> true) t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (scope_equivalent ~info_eq) scopes1 scopes2
  | Primitive p1, Primitive p2 -> Primitive.All.equivalent ~info_eq p1 p2
  | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false

and scope_equivalent ~info_eq (Scope (names1, t1)) (Scope (names2, t2)) =
  List.equal (Lvca_util.Tuple2.equal Provenance.( = ) String.( = )) names1 names2
  && equivalent ~info_eq t1 t2
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let rec vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats |> List.map ~f:vars_of_scope |> Set.union_list (module String)
  | Var (_, name) -> String.Set.of_list [ name ]
  | Primitive _ -> String.Set.empty

and vars_of_scope (Scope (vars, t)) =
  vars |> List.map ~f:snd |> String.Set.of_list |> Set.union (vars_of_pattern t)
;;

let rec list_vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats
    (* simpler, morally equivalent: List.concat_map ~f:(List.concat_map
       ~f:list_vars_of_scope) *)
    |> List.map ~f:list_vars_of_scope
    |> List.concat_no_order
  | Var (loc, name) -> [ loc, name ]
  | Primitive _ -> []

and list_vars_of_scope (Scope (names, pat)) =
  let names' = List.map names ~f:snd in
  let pat_vars =
    pat
    |> list_vars_of_pattern
    |> List.filter ~f:(fun (_i, name) -> not (List.mem names' name ~equal:String.( = )))
  in
  List.unordered_append names pat_vars
;;

let info = function
  | Operator (i, _, _) | Var (i, _) -> i
  | Primitive prim -> Primitive.All.info prim
;;

let any, list, string, semi, pf = Fmt.(any, list, string, semi, pf)

let rec pp ppf tm =
  match tm with
  | Operator (_, tag, subtms) ->
    Provenance.open_stag ppf (info tm);
    pf ppf "@[<hv>%s(%a)@]" tag (list ~sep:semi pp_scope) subtms;
    Provenance.close_stag ppf (info tm)
  | Var (_, v) ->
    Provenance.open_stag ppf (info tm);
    string ppf v;
    Provenance.close_stag ppf (info tm)
  | Primitive p -> Primitive.All.pp ppf p

and pp_scope ppf (Scope (bindings, body)) =
  let pp_binding ppf (info, name) =
    Provenance.open_stag ppf info;
    string ppf name;
    Provenance.close_stag ppf info
  in
  match bindings with
  | [] -> pp ppf body
  | _ -> pf ppf "%a.@ %a" (list ~sep:(any ".@ ") pp_binding) bindings pp body
;;

let rec select_path ~path pat =
  match path with
  | [] -> Ok pat
  | i :: path ->
    (match pat with
    | Var _ | Primitive _ -> Error "TODO: message"
    | Operator (_, _, scopes) ->
      (match List.nth scopes i with
      | None -> Error "TODO: message"
      | Some (Scope (_vars, pat)) -> select_path ~path pat))
;;

let rec match_term pat tm =
  match pat, tm with
  | Var (_, name), tm -> Some (SMap.singleton name (Capture.Term tm))
  | Primitive p1, Term.Primitive p2 ->
    if Primitive.All.( = ) p1 p2 then Some SMap.empty else None
  | Operator (_, name1, pat_scopes), Operator (_, name2, tm_scopes) ->
    if String.(name1 = name2)
    then (
      match List.map2 pat_scopes tm_scopes ~f:match_scope with
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

and match_scope (Scope (binder_pats, body_pat)) (Scope.Scope (binders, body)) =
  let f (_, name) pat =
    if Lvca_util.String.is_ignore name then None else Some (name, Capture.Binder pat)
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
    let%bind body_match = match_term body_pat body in
    (match SMap.strict_union binder_captures body_match with
    | `Duplicate_key _ -> None (* TODO: error *)
    | `Ok result -> Some result)
;;

let rec match_all pat tm =
  let tm_info = Nominal.Term.info tm in
  match pat, tm with
  | Var (_, _), _ -> [ tm_info ]
  | Primitive p1, Term.Primitive p2 ->
    if Primitive.All.( = ) p1 p2 then [ tm_info ] else []
  | Operator _, Operator (_, _, scopes) ->
    let f (Scope.Scope (_binders, body)) = match_all pat body in
    let sub_matches = List.concat_map scopes ~f in
    (match match_term pat tm with
    | None -> sub_matches
    | Some _ -> tm_info :: sub_matches)
  | _, _ -> []
;;

let handle_dup_error = function
  | `Ok result -> Ok result
  | `Duplicate_key k ->
    Error
      (Check_failure.err
         (Printf.sprintf
            "Did you mean to bind the same variable (%s) twice in the same pattern? \
             That's not allowed!"
            k))
;;

let check check_prim lang sort =
  let lookup_operator = Abstract_syntax.lookup_operator lang in
  let rec check sort pat =
    let result =
      match pat with
      | Var (_, name) -> Ok (SMap.singleton name (Capture_type.Bound_term sort))
      | Primitive prim ->
        (match check_prim prim sort with
        | None -> Ok SMap.empty
        | Some msg -> Error (Check_failure.err msg))
      | Operator (_, op_name, subpats) ->
        let sort_name, sort_args = Sort.split sort in
        (match lookup_operator sort_name op_name with
        | None ->
          Error
            (Check_failure.err
               (Printf.sprintf
                  "Binding_aware_pattern.check: failed to find operator %s in sort %s"
                  op_name
                  sort_name))
        | Some (sort_vars, Operator_def (_, _, arity)) ->
          (* TODO: kind check *)
          let sort_vars = sort_vars |> List.map ~f:Tuple2.get1 in
          let sort_env = SMap.of_alist_exn (List.zip_exn sort_vars sort_args) in
          check_slots (Abstract_syntax.Arity.instantiate sort_env arity) subpats)
    in
    Result.map_error result ~f:(fun Check_failure.{ message; stack } ->
        Check_failure.{ message; stack = { term = pat; sort } :: stack })
  and check_slots (Arity (_, valences)) scopes =
    match List.zip scopes valences with
    | Unequal_lengths ->
      Error
        (Check_failure.err
           Fmt.(
             str
               "Wrong number of subterms (%u) for this arity (%a)"
               (List.length scopes)
               (list ~sep:comma Abstract_syntax.Valence.pp)
               valences))
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
        (Check_failure.err
           (Fmt.str
              "Wrong number of binders (%u) for this valence (%a) (expected %u)"
              (List.length binders)
              Abstract_syntax.Valence.pp
              valence
              (List.length binder_slots)))
    | Ok binders ->
      let binders_env =
        binders
        |> List.map ~f:(fun (slot, (_, v)) ->
               let binding_type =
                 match slot with
                 | Sort_binding sort -> Capture_type.Bound_var sort
                 | Sort_pattern pattern_sort -> Bound_pattern pattern_sort
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
          (Check_failure.err
             (Printf.sprintf
                "Did you mean to bind the same variable (%s) twice in the same set of \
                 patterns? That's not allowed!"
                k)))
  in
  check sort
;;

let parse =
  let open Lvca_parsing in
  let module Ws = C_comment_parser in
  let pat_to_ident = function Var (info, name) -> Some (info, name) | _ -> None in
  fix (fun pat ->
      let slot =
        sep_by1 (Ws.char '.') pat
        >>= fun pats ->
        let binders_pats, pat = Util.List.unsnoc pats in
        let f = pat_to_ident >> Option.map ~f:(fun (info, name) -> info, name) in
        match binders_pats |> List.map ~f |> Option.all with
        | None ->
          fail
            "found an operator pattern where only a variable is allowed (left of a `.`)"
        | Some binders -> return (Scope (binders, pat))
      in
      choice
        ~failure_msg:"looking for a primitive or identifier (for a var or operator)"
        [ (Primitive.All.parse >>| fun prim -> Primitive prim)
        ; (Ws.identifier
          >>== fun { range; value = ident } ->
          choice
            [ Ws.(
                parens (sep_end_by (char ';') slot)
                >>~ fun range' slots ->
                let range = Opt_range.union range range' in
                Operator (Provenance.of_range range, ident, slots))
            ; return (Var (Provenance.of_range range, ident))
            ])
        ])
  <?> "binding-aware pattern"
;;

module Properties = struct
  open Util
  open Property_result

  let parse = Lvca_parsing.parse_string parse
  let to_string = Fmt.to_to_string pp

  let string_round_trip1 t =
    let str = to_string t in
    match parse str with
    | Ok t' -> Property_result.check (t' = t) (Fmt.str "%a <> %a" pp t' pp t)
    | Error msg -> Failed (Fmt.str {|parse_string %S: %s|} str msg)
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
          Property_result.check String.(str'' = str') (Fmt.str {|%S <> %S|} str'' str'))
  ;;
end

let%test_module "Parsing" =
  (module struct
    let () =
      Format.set_formatter_stag_functions Provenance.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let ( = ) = equivalent
    let here = Provenance.of_here [%here]

    let parse_exn =
      Lvca_parsing.(parse_string (whitespace *> parse)) >> Result.ok_or_failwith
    ;;

    let%test _ = parse_exn "tm " = Var (here, "tm")

    let print_parse tm =
      match Lvca_parsing.parse_string parse tm with
      | Ok pat -> Fmt.pr "%a" pp pat
      | Error msg -> Fmt.pr "failed: %s\n" msg
    ;;

    let%expect_test _ =
      print_parse {|"str"|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,5} }>"str"</{ input = Input_unknown; range = {0,5} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,3} }>a()</{ input = Input_unknown; range = {0,3} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,4} }>a(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>)</{ input = Input_unknown; range = {0,4} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c)|};
      (*0123456*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,6} }>a(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>; <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>)</{ input = Input_unknown; range = {0,6} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;d;e;)|};
      (*012345678901*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,11} }>a(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>; <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>; <{ input = Input_unknown; range = {6,7} }>d</{ input = Input_unknown; range = {6,7} }>; <{ input = Input_unknown; range = {8,9} }>e</{ input = Input_unknown; range = {8,9} }>)</{ input = Input_unknown; range = {0,11} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c.d;e;)|};
      (*012345678901*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,11} }>a(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>. <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>. <{ input = Input_unknown; range = {6,7} }>d</{ input = Input_unknown; range = {6,7} }>; <{ input = Input_unknown; range = {8,9} }>e</{ input = Input_unknown; range = {8,9} }>)</{ input = Input_unknown; range = {0,11} }>
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
    let parse' p str =
      Lvca_parsing.(parse_string (whitespace *> p) str) |> Result.ok_or_failwith
    ;;

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

    let language = parse' Abstract_syntax.parse lang_desc

    let print_check_pattern sort_str pat_str =
      let sort = parse' Sort.parse sort_str in
      let pat = parse' parse pat_str in
      let pp ppf pat = Fmt.pf ppf "pattern: %a" pp pat in
      match check Primitive.All.check language sort pat with
      | Error failure -> Fmt.epr "%a" (Check_failure.pp pp) failure
      | Ok capture_types ->
        capture_types
        |> Map.iteri ~f:(fun ~key ~data -> Fmt.pr "%s: %a\n" key Capture_type.pp data)
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
      [%expect
        {| a: <{ input = Input_unknown; range = {76,81} }>value</{ input = Input_unknown; range = {76,81} }> |}]
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
      [%expect
        {|
      body: <{ input = Input_unknown; range = {201,205} }>term</{ input = Input_unknown; range = {201,205} }>
      x: <{ input = Input_unknown; range = {194,199} }>value</{ input = Input_unknown; range = {194,199} }>
      |}]
    ;;

    let%expect_test _ =
      print_check_pattern "match_line" "match_line(Pattern. body)";
      [%expect
        {|
      Pattern: <{ input = Input_unknown; range = {154,159} }>value</{ input = Input_unknown; range = {154,159} }>[<{ input = Input_unknown; range = {160,165} }>value</{ input = Input_unknown; range = {160,165} }>]
      body: <{ input = Input_unknown; range = {168,172} }>term</{ input = Input_unknown; range = {168,172} }>
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
      Binding_aware_pattern.check: failed to find operator foo in sort value
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
    let parse_pattern str = Lvca_parsing.parse_string parse str |> Result.ok_or_failwith

    let parse_term str =
      Lvca_parsing.parse_string Nominal.Term.parse' str |> Result.ok_or_failwith
    ;;

    let print_match pat_str tm_str =
      let pattern = parse_pattern pat_str in
      let tm = parse_term tm_str in
      match match_term pattern tm with
      | None -> ()
      | Some mapping ->
        mapping
        |> Map.iteri ~f:(fun ~key ~data -> Fmt.pr "%s -> %a\n" key Capture.pp data)
    ;;

    let%expect_test _ =
      print_match "foo()" "foo()";
      [%expect]
    ;;

    let%expect_test _ =
      print_match "lam(a. b)" "lam(x. y)";
      [%expect
        {|
        a -> <syntax/Nominal.ml:18:418>x</syntax/Nominal.ml:18:418>
        b -> <syntax/Nominal.ml:18:418>y</syntax/Nominal.ml:18:418> |}]
    ;;

    let%expect_test _ =
      print_match "match(a. b)" "match(foo(bar(); baz()). x)";
      [%expect
        {|
        a -> <syntax/Nominal.ml:14:302>foo(<syntax/Nominal.ml:14:302>bar()</syntax/Nominal.ml:14:302>; <syntax/Nominal.ml:14:302>baz()</syntax/Nominal.ml:14:302>)</syntax/Nominal.ml:14:302>
        b -> <syntax/Nominal.ml:18:418>x</syntax/Nominal.ml:18:418> |}]
    ;;
  end)
;;
