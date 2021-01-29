open Base
open AbstractSyntax
module SMap = Lvca_util.String.Map

type ('info, 'prim) abstract_syntax_check_failure_frame =
  { term : (('info, 'prim) Pattern.t, ('info, 'prim) Nominal.term) Either.t
  ; sort : 'info Sort.t
  }

type ('info, 'prim) abstract_syntax_check_failure =
  { message : string
  ; stack : ('info, 'prim) abstract_syntax_check_failure_frame list
  }

let err : string -> ('info, 'prim) abstract_syntax_check_failure =
 fun message -> { message; stack = [] }
;;

let pp_failure
    :  'prim Fmt.t -> Stdlib.Format.formatter
    -> ('info, 'prim) abstract_syntax_check_failure -> unit
  =
 fun prim_pp ppf { message; stack } ->
  Fmt.string ppf message;
  if List.length stack > 0
  then (
    Stdlib.Format.pp_force_newline ppf ();
    Fmt.pf ppf "stack:";
    List.iter stack ~f:(fun { term; sort } ->
        Stdlib.Format.pp_force_newline ppf ();
        match term with
        | First pat ->
          Fmt.pf ppf "- @[pattern: %a,@ sort: %a@]" (Pattern.pp prim_pp) pat Sort.pp sort
        | Second tm ->
          Fmt.pf ppf "- @[term: %a,@ sort: %a@]" (Nominal.pp_term prim_pp) tm Sort.pp sort))
;;

let concretize_sort_slot : 'info Sort.t SMap.t -> 'info sort_slot -> 'info sort_slot =
 fun env -> function
  | SortBinding s -> SortBinding (Sort.instantiate env s)
  | SortPattern (s1, s2) -> SortPattern (Sort.instantiate env s1, Sort.instantiate env s2)
;;

let concretize_valence : 'info Sort.t SMap.t -> 'info valence -> 'info valence =
 fun env -> function
  | Valence (binding_sort_slots, body_sort) ->
    Valence
      ( List.map binding_sort_slots ~f:(concretize_sort_slot env)
      , Sort.instantiate env body_sort )
;;

let concretize_arity : 'info Sort.t SMap.t -> 'info arity -> 'info arity =
 fun env -> List.map ~f:(concretize_valence env)
;;

let lookup_operator
    :  'info AbstractSyntax.t -> string (* sort name *) -> string (* operator_name *)
    -> (string list * 'info operator_def) option
  =
 fun sort_defs sort_name op_name ->
  let open Option.Let_syntax in
  let%bind (SortDef (vars, operator_defs)) =
    List.find_map sort_defs ~f:(fun (name, def) ->
        if String.(name = sort_name) then Some def else None)
  in
  let%bind result =
    List.find operator_defs ~f:(fun (OperatorDef (op_def_name, _)) ->
        String.(op_def_name = op_name))
  in
  Some (vars, result)
;;

(* Check that this pattern is valid and return the valence for each variable it binds *)
let check_pattern pp_prim check_prim lang pattern_sort var_sort =
  let lookup_operator' = lookup_operator lang in
  let handle_dup_error = function
    | `Ok result -> Ok result
    | `Duplicate_key k ->
      Error
        (err
           (Printf.sprintf
              "Did you mean to bind the same variable (%s) twice in the same pattern? \
               That's not allowed!"
              k))
  in
  let rec go_pattern sort pat =
    let result =
      match pat with
      | Pattern.Var (_, name) ->
        if Sort.equal Unit.( = ) (Sort.erase_info sort) (Sort.erase_info var_sort)
        then Ok (SMap.singleton name (Valence ([], sort)))
        else
          Error
            (err
               (Printf.sprintf
                  "Pattern %s is of sort `%s`. Expected `%s`."
                  name
                  (Sort.to_string var_sort)
                  (Sort.to_string sort)))
      | Ignored _ -> Ok SMap.empty
      | Primitive (info, prim) ->
        (match check_prim info prim sort with
        | None -> Ok SMap.empty
        | Some msg -> Error (err msg))
      | Operator (_, op_name, subpats) ->
        let lookup_and_go sort_name sort_args =
          match lookup_operator' sort_name op_name with
          | None ->
            Error
              (err
                 (Printf.sprintf
                    "check_pattern: failed to find operator %s in sort %s"
                    op_name
                    sort_name))
          | Some (sort_vars, OperatorDef (_, arity)) ->
            let sort_env = SMap.of_alist_exn (List.zip_exn sort_vars sort_args) in
            go_arity_pat (concretize_arity sort_env arity) subpats
        in
        (match sort with
        | Sort.Name (_, sort_name) -> lookup_and_go sort_name []
        | Sort.Ap (_, sort_name, sort_args) -> lookup_and_go sort_name sort_args)
    in
    Result.map_error result ~f:(fun { message; stack } ->
        { message; stack = { term = First pat; sort } :: stack })
  and go_arity_pat valences pats =
    match List.zip pats valences with
    | Unequal_lengths ->
      Error
        (err
           (Printf.sprintf
              "Wrong number of subterms (%u) for this arity (%s)"
              (List.length pats)
              (valences |> List.map ~f:string_of_valence |> String.concat ~sep:", ")))
    | Ok pat_valences ->
      let open Result.Let_syntax in
      let%bind result =
        pat_valences
        |> List.map ~f:(fun (pat, valence) ->
               match valence with
               (* The pattern is a single var of the given sort *)
               | Valence ([], sort) -> go_pattern sort pat
               | _ ->
                 Error
                   (err
                      (Printf.sprintf
                         "Invalid pattern (%s) binding a non-sort valence (%s)"
                         (Pattern.to_string pp_prim pat)
                         (string_of_valence valence))))
        |> Result.all
        |> Result.map ~f:SMap.strict_unions
      in
      handle_dup_error result
  in
  go_pattern pattern_sort
;;

(* Check that the given term matches the given sort.

   This recursively checks subterms and patterns. *)
let check_term pp_prim check_prim lang =
  let lookup_operator' = lookup_operator lang in
  let check_pattern' = check_pattern pp_prim check_prim lang in
  let rec go var_valences sort tm =
    let result =
      match tm with
      | Nominal.Var (_, v) ->
        (match Map.find var_valences v with
        | None -> Some (err (Printf.sprintf "Unknown variable %s (is it bound?)" v))
        (* Note: We allow patterns to bind scopes, eg we allow the pattern [lambda(x)]
           because this is useful in defining semantics where you would open the scope.
           However, it's not... TODO: clarify *)
        | Some valence ->
          (match valence with
          | Valence ([], sort') ->
            if Sort.equal Unit.( = ) (Sort.erase_info sort') (Sort.erase_info sort)
            then None
            else
              Some
                (err
                   (Printf.sprintf
                      "Variable %s has unexpected valence (saw: %s) (expected: %s)"
                      v
                      (string_of_valence valence)
                      (Sort.to_string sort)))
          | _ -> failwith "TODO"))
      | Primitive (info, p) ->
        (match check_prim info p sort with None -> None | Some msg -> Some (err msg))
      | Operator (_, operator_name, op_scopes) ->
        let lookup_and_go sort_name sort_args =
          match lookup_operator' sort_name operator_name with
          | None ->
            Some
              (err
                 (Printf.sprintf
                    "check_term: failed to find operator %s in sort %s"
                    operator_name
                    sort_name))
          | Some (vars, OperatorDef (_, arity)) ->
            let sort_env = SMap.of_alist_exn (List.zip_exn vars sort_args) in
            let concrete_arity = concretize_arity sort_env arity in
            go_arity var_valences concrete_arity op_scopes
        in
        (match sort with
        | Sort.Name (_, sort_name) -> lookup_and_go sort_name []
        | Sort.Ap (_, sort_name, sort_args) -> lookup_and_go sort_name sort_args)
    in
    Option.map result ~f:(fun { message; stack } ->
        { message; stack = { term = Second tm; sort } :: stack })
  and go_arity var_valences valences scopes =
    match List.zip scopes valences with
    | Unequal_lengths ->
      Some
        (err
           (Printf.sprintf
              "Wrong number of subterms (%u) for this arity (%s)"
              (List.length scopes)
              (valences |> List.map ~f:string_of_valence |> String.concat ~sep:", ")))
    | Ok scope_valences ->
      List.find_map scope_valences (* TODO: go_arity *) ~f:(fun (scope, valence) ->
          go_scope var_valences valence scope)
  and go_scope var_valences valence (Scope (binders, body)) =
    (* Check the body with the new binders environment *)
    let go_body
        body_sort
        binders_env
          (* (binders_env : (valence SMap.t, 'a abstract_syntax_check_failure) Result.t list) *)
      =
      (* XXX: is *, 0-or-more repetition (does this make sense?)? or is it 1-or-more?
       * Does + exist?
       *)
      match Result.all binders_env with
      | Error err -> Some err
      | Ok binders_env' ->
        (match SMap.strict_unions binders_env' with
        | `Ok binders_env'' (* check every term in body for an error *) ->
          go (Lvca_util.Map.union_right_biased var_valences binders_env'') body_sort body
        | `Duplicate_key k ->
          Some
            (err
               (Printf.sprintf
                  "Did you mean to bind the same variable (%s) twice in the same set of \
                   patterns? That's not allowed!"
                  k)))
    in
    let (Valence (binder_sorts, body_sort)) = valence in
    match List.zip binder_sorts binders with
    (* TODO: do we allow binding valence? *)
    | Unequal_lengths ->
      Some
        (err
           (Printf.sprintf
              "Wrong number of binders (%u) for this valence (%s) (expected %u)"
              (List.length binders)
              (string_of_valence valence)
              (List.length binder_sorts)))
    | Ok binders' ->
      binders'
      |> List.map ~f:(fun (slot, pat) ->
             match slot, pat with
             | SortBinding sort, Var (_, _v) -> check_pattern' sort sort pat
             | SortBinding _sort, _ ->
               Error (err "Fixed-valence binders must all be vars (no patterns)")
             | SortPattern (s1, s2), _ -> check_pattern' s1 s2 pat)
      |> go_body body_sort
  in
  go SMap.empty
;;

module Primitive' = struct
  let check_prim _info prim sort =
    match prim, sort with
    | Primitive.PrimString _, Sort.Name (_, "string")
    | PrimFloat _, Sort.Name (_, "float")
    | PrimChar _, Sort.Name (_, "char")
    | PrimInteger _, Sort.Name (_, "integer") ->
      None
    | _, _ ->
      Some
        (Printf.sprintf
           "Unexpected sort (%s) for a primitive (%s)"
           (Sort.to_string sort)
           (Primitive.to_string prim))
  ;;

  let check_pattern lang sort pat = check_pattern Primitive.pp check_prim lang sort pat
  let check_term lang sort pat = check_term Primitive.pp check_prim lang sort pat
end

let%test_module "CheckTerm" =
  (module struct
    module AbstractSyntaxParse = AbstractSyntax.Parse (ParseUtil.NoComment)

    let parse_lang lang_str =
      ParseUtil.parse_string AbstractSyntaxParse.whitespace_t lang_str
      |> Result.ok_or_failwith
    ;;

    module NominalParse = Nominal.Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)

    let parse_term term_str =
      ParseUtil.parse_string (NominalParse.t ParsePrimitive.t) term_str
      |> Result.ok_or_failwith
    ;;

    module SortParse = Sort.Parse (ParseUtil.NoComment)

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

    let print_check_pattern ?var_sort_str sort_str pat_str =
      let sort = parse_sort sort_str |> Result.ok_or_failwith in
      let var_sort =
        match var_sort_str with
        | None -> sort
        | Some str -> parse_sort str |> Result.ok_or_failwith
      in
      let pat =
        match pat_str |> parse_term |> Nominal.to_pattern with
        | Ok pat -> pat
        | Error scope ->
          failwith
            (Printf.sprintf
               "Failed to convert term to pattern (found a scope: %s)"
               (Nominal.pp_scope_str Primitive.pp scope))
      in
      match Primitive'.check_pattern language sort var_sort pat with
      | Error failure -> Fmt.epr "%a" (pp_failure Primitive.pp) failure
      | Ok _ -> ()
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
      print_check_pattern
        ~var_sort_str:"value"
        "term"
        {|value(list(cons(unit(); nil())))|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern ~var_sort_str:"value" "term" {|value(list(cons(a; nil())))|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "term" {|value(list(cons(a; nil())))|};
      [%expect
        {|
        Pattern a is of sort `term`. Expected `value`.
        stack:
        - pattern: value(list(cons(a; nil()))), sort: term
        - pattern: list(cons(a; nil())), sort: value
        - pattern: cons(a; nil()), sort: list value
        - pattern: a, sort: value
      |}]
    ;;

    let%expect_test _ =
      print_check_pattern
        ~var_sort_str:"value"
        "term"
        {|value(list(cons(a; cons(a; _))))|};
      [%expect
        {|
      Did you mean to bind the same variable (a) twice in the same pattern? That's not allowed!
      stack:
      - pattern: value(list(cons(a; cons(a; _)))), sort: term
      - pattern: list(cons(a; cons(a; _))), sort: value
      - pattern: cons(a; cons(a; _)), sort: list value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "lambda(a)";
      [%expect
        {|
        Invalid pattern (a) binding a non-sort valence (value. term)
        stack:
        - pattern: lambda(a), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "match_line" "match_line(a)";
      [%expect
        {|
        Invalid pattern (a) binding a non-sort valence (value[value]. term)
        stack:
        - pattern: match_line(a), sort: match_line |}]
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
      check_pattern: failed to find operator foo in sort value
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
      Invalid pattern (1) binding a non-sort valence (value. term)
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

    let check_term' sort_str tm_str =
      match parse_sort sort_str with
      | Error msg -> Fmt.epr "%s" msg
      | Ok sort ->
        (match tm_str |> parse_term |> Primitive'.check_term language sort with
        | Some failure -> Fmt.epr "%a" (pp_failure Primitive.pp) failure
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
      check_term: failed to find operator unit in sort term
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
      Variable val has unexpected valence (saw: value) (expected: term)
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
      Variable a has unexpected valence (saw: value) (expected: term)
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
      print_check_pattern "integer" "1";
      [%expect]
    ;;

    let%expect_test _ =
      check_term' "float" "1.";
      print_check_pattern "float" "1.";
      [%expect]
    ;;

    let%expect_test _ =
      check_term' "char" "'a'";
      print_check_pattern "char" "'a'";
      [%expect]
    ;;

    let%expect_test _ =
      check_term' "string" {|"str"|};
      print_check_pattern "string" {|"str"|};
      [%expect]
    ;;
  end)
;;

module Primitive = Primitive'
