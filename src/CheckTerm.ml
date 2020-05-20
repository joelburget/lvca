open Base
module Format = Caml.Format

open AbstractSyntax_Types
module Types = AbstractSyntax_Types
module Nominal = Binding.Nominal

type abstract_syntax_check_failure_frame =
  { term : (Pattern.t, Nominal.term) Either.t (** Term that failed to check *)
  ; sort : sort (** Sort it failed to check against *)
  }

type abstract_syntax_check_failure =
  { message : string
  ; stack : abstract_syntax_check_failure_frame list
  }

let pp_failure : Format.formatter -> abstract_syntax_check_failure -> unit
  = fun ppf { message; stack } ->
    Fmt.string ppf message;

    if (List.length stack > 0)
    then (
      Fmt.pf ppf "@\nstack:";
      List.iter stack ~f:(fun { term; sort } ->
        Format.pp_force_newline ppf ();
        match term with
          | First pat ->
            Fmt.pf ppf "pattern: %a, sort: %a" Pattern.pp pat pp_sort sort
          | Second tm ->
            Fmt.pf ppf "term: %a, sort: %a" Nominal.pp_term tm pp_sort sort
      )
    )

let rec concretize_sort : sort String.Map.t -> sort -> sort
  = fun env -> function
    | SortAp (name, sub_sorts) -> SortAp
      ( name
      , List.map sub_sorts ~f:(concretize_sort env)
      )
    | SortVar name
    -> Map.find env name
      |> Util.get_option' (fun () -> "concretize_sort: unknown variable " ^ name)

let concretize_valence : sort String.Map.t -> valence -> valence
  = fun env -> function
    | FixedValence (binding_sorts, body_sort) -> FixedValence
      ( List.map binding_sorts ~f:(concretize_sort env)
      , concretize_sort env body_sort
      )
    | VariableValence (bound_sort, body_sort)
    -> VariableValence
      (concretize_sort env bound_sort, concretize_sort env body_sort)

let concretize_arity : sort String.Map.t -> arity -> arity
  = fun env -> function
    | FixedArity valences
    -> FixedArity (List.map valences ~f:(concretize_valence env))
    | VariableArity sort
    -> VariableArity (concretize_sort env sort)

let lookup_operator
  :  Types.t
  -> sort_name
  -> string (* operator_name *)
  -> (string list * operator_def) option
  = fun { sort_defs = SortDefs sort_defs; _ } sort_name op_name ->
    let open Option.Let_syntax in
    let%bind (SortDef (vars, operator_defs)) = Map.find sort_defs sort_name in
    let%bind it = List.find operator_defs
      ~f:(fun (OperatorDef (op_def_name, _)) -> String.(op_def_name = op_name))
    in
    Some (vars, it)

(** Check that this pattern is valid and return the valence for each variable it binds
 *)
let check_pattern
  :  Types.t
  -> sort
  -> Pattern.t
  -> (valence String.Map.t, abstract_syntax_check_failure) Result.t
  = fun lang ->
    let lookup_operator' = lookup_operator lang in

    let go_primitive
      : sort
      -> Primitive.t
      -> (valence String.Map.t, abstract_syntax_check_failure) Result.t
      = fun sort prim -> match prim, sort with
        | PrimString _, SortAp ("string", [])
        | PrimInteger _, SortAp ("integer", []) -> Ok String.Map.empty
        | _, _ -> Error
          { stack = [{ term = Second (Primitive prim); sort }]
          ; message = Printf.sprintf "Unexpected sort (%s) for a primitive (%s)"
            (string_of_sort sort) (Primitive.to_string prim)
          }
    in

    let rec go_pattern
      :  sort
      -> Pattern.t
      -> (valence String.Map.t, abstract_syntax_check_failure) Result.t
      = fun sort -> function
      | Var name -> Ok (String.Map.singleton name (FixedValence ([], sort)))
      | Ignored _ -> Ok String.Map.empty
      | Primitive prim -> go_primitive sort prim
      | Operator (op_name, subpats) -> match sort with
        | SortVar _ -> Util.invariant_violation "check_pattern: non-concrete sort"
        | SortAp (sort_name, sort_args) -> (match lookup_operator' sort_name op_name with
          | None -> Error
            { stack = []
            ; message = Printf.sprintf
              "check_pattern: failed to find operator %s in sort %s"
              op_name sort_name
            }
          | Some (sort_vars, OperatorDef (_, arity)) ->
            let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
            go_arity_pat (concretize_arity sort_env arity) subpats
        )

    and go_arity_pat
      :  arity
      -> Pattern.t list
      -> (valence String.Map.t, abstract_syntax_check_failure) Result.t
      = fun arity pats -> match arity with
        | FixedArity valences -> go_fixed_arity_pat valences pats
        | VariableArity sort -> go_variable_arity_pat sort pats

    and go_fixed_arity_pat
      :  valence list
      -> Pattern.t list
      -> (valence String.Map.t, abstract_syntax_check_failure) Result.t
      = fun valences pats -> match List.zip pats valences with
        | Unequal_lengths
        -> Error
          { stack = []
          ; message = Printf.sprintf
            "Wrong number of subterms (%n) for this arity (%s)"
            (List.length pats)
            (valences |> List.map ~f:string_of_valence |> String.concat ~sep:", ")
          }
        | Ok pat_valences ->
          let open Result.Let_syntax in
          let%bind result = pat_valences
            |> List.map
              ~f:(fun (pat, valence) -> match pat, valence with
                (* The only thing that can match with a non-sort valence is a var *)
                | Var name, _ -> Ok (String.Map.singleton name valence)
                | Ignored _, _ -> Ok String.Map.empty

                (* For anything else matching a sort we defer to go_pattern *)
                | _, FixedValence ([], sort) -> go_pattern sort pat

                (* Anything else is an error *)
                | Primitive _, _
                | Operator _, _ -> Error
                  { stack = []
                  ; message = Printf.sprintf
                    "Invalid variable-valence (%s) pattern. Only a variable is valid."
                    (string_of_valence valence)
                  }
              )
            |> Result.all
            |> Result.map ~f:Util.String.Map.strict_unions
          in
          match result with
            | `Ok result -> Ok result
            | `Duplicate_key k -> Error
              { stack = []
              ; message = Printf.sprintf
                "Did you mean to bind the same variable (%s) twice in the same pattern? \
                That's not allowed!"
                k
              }

    and go_variable_arity_pat
      :  sort
      -> Pattern.t list
      -> (valence String.Map.t, abstract_syntax_check_failure) Result.t
      = fun sort subterms ->
        let open Result.Let_syntax in
        let%bind result = subterms
          |> List.map ~f:(go_pattern sort)
          |> Result.all
        in
        match Util.String.Map.strict_unions result with
          | `Ok result -> Ok result
          | `Duplicate_key k -> Error
            { stack = []
            ; message = Printf.sprintf
              "Did you mean to bind the same variable (%s) twice in the same pattern? \
              That's not allowed!"
              k
            }
    in

    go_pattern

(** Check that the given term matches the given sort.

 This recursively checks subterms and patterns.
 *)
let check_term
  :  Types.t (** Abstract syntax *)
  -> sort (** Sort to check term against *)
  -> Nominal.term
  -> abstract_syntax_check_failure option
  = fun lang ->
    let invariant_violation = Util.invariant_violation in
    let lookup_operator' = lookup_operator lang in
    let check_pattern' = check_pattern lang in

    let rec go
      :  valence String.Map.t (* mapping from variable name to its sort *)
      -> sort
      -> Nominal.term
      -> abstract_syntax_check_failure option
      = fun var_valences sort tm -> match tm with
        | Var v -> (match Map.find var_valences v with
          | None -> Some
            { stack = [ { term = Second tm; sort } ]
            ; message = Printf.sprintf "Unknown variable %s (is it bound?)" v
            }
          (* XXX this implies we can never successfully check a bound non-sort valence? *)
          | Some sort' -> if Caml.(sort' = FixedValence ([], sort))
            then None
            else Some
              { stack = [ { term = Second tm; sort } ]
              ; message = Printf.sprintf
                "Variable %s has unexpected valence (saw: %s) (expected: %s)"
                v (string_of_valence sort') (string_of_sort sort)
              }
        )
        | Primitive p -> (match p, sort with
          | PrimInteger _, SortAp ("integer", []) -> None
          | PrimString _, SortAp ("string", []) -> None
          | _, _ -> Some
            { stack = [ { term = Second tm; sort } ]
            ; message = "Unexpected primitive sort"
            })

        | Operator (operator_name, op_scopes) -> (match sort with
          | SortVar _ -> invariant_violation "check_term (go): non-concrete sort"
          | SortAp (sort_name, sort_args)
          -> (match lookup_operator' sort_name operator_name with
            | None -> Some
              { stack = []
              ; message = Printf.sprintf
                "check_term: failed to find operator %s in sort %s"
                operator_name sort_name
              }
            | Some (vars, OperatorDef (_, arity)) ->
              let sort_env = String.Map.of_alist_exn (List.zip_exn vars sort_args) in
              let concrete_arity = concretize_arity sort_env arity in
              go_arity var_valences concrete_arity op_scopes
          )
        )

    and go_arity
      :  valence String.Map.t
      -> arity
      -> Nominal.scope list
      -> abstract_syntax_check_failure option
      = fun var_valences arity scopes -> match arity with
        | FixedArity valences -> go_fixed_arity var_valences valences scopes
        | VariableArity sort -> go_variable_arity var_valences sort scopes

    and go_fixed_arity
      :  valence String.Map.t
      -> valence list
      -> Nominal.scope list
      -> abstract_syntax_check_failure option
      = fun var_valences valences scopes -> match List.zip scopes valences with
        | Unequal_lengths -> Some
          { stack = []
          ; message = Printf.sprintf
            "Wrong number of subterms (%n) for this arity (%s)"
            (List.length scopes)
            (valences |> List.map ~f:string_of_valence |> String.concat ~sep:", ")
          }
        | Ok scope_valences -> List.find_map scope_valences (* TODO: go_fixed_arity *)
          ~f:(fun (scope, valence) -> go_scope var_valences valence scope)

    and go_variable_arity
      :  valence String.Map.t
      -> sort
      -> Nominal.scope list
      -> abstract_syntax_check_failure option
      = fun var_valences sort subterms -> subterms
        |> List.find_map ~f:(function
          | Scope ([], body) -> go var_valences sort body
          | _ -> Some
            { stack = []
            ; message = Printf.sprintf
              "Unexpectedly found a binder where terms were expected (variable arity\
              : %s*)"
              (string_of_sort sort)
            }
        )

    and go_scope
      :  valence String.Map.t
      -> valence
      -> Nominal.scope
      -> abstract_syntax_check_failure option
      = fun var_valences valence (Scope (binders, body)) ->

        (* Check the body with the new binders environment *)
        let go_body body_sort
          (binders_env : ((valence String.Map.t, abstract_syntax_check_failure) Result.t) list)
          = match Result.all binders_env with
          | Error err -> Some err
          | Ok binders_env' -> match Util.String.Map.strict_unions binders_env' with
            | `Ok binders_env'' ->
              go (Util.Map.union_right_biased var_valences binders_env'') body_sort body
            | `Duplicate_key k -> Some
              { stack = []
              ; message = Printf.sprintf
                "Did you mean to bind the same variable (%s) twice in the same set of \
                patterns? That's not allowed!"
                k
              }
        in

        match valence with
        | FixedValence (binder_sorts, body_sort) ->
          (match List.zip binder_sorts binders with
            | Unequal_lengths -> Some
              { stack = []
              ; message = Printf.sprintf
                "Wrong number of binders (%n) for this valence (%s)"
                (List.length binders)
                (string_of_valence valence)
              }
            | Ok binders' -> binders'
              |> List.map ~f:(fun (sort, pat) -> match pat with
                | Var _v -> check_pattern' sort pat
                | _ -> Error
                  { stack = []
                  ; message = "Fixed-valence binders must all be vars (no patterns)"
                  })
              |> go_body body_sort)
        | VariableValence (binder_sort, body_sort) -> match binders with
          | [binder] -> go_body body_sort [check_pattern' binder_sort binder]
          | _ -> Some
            { stack = []
            ; message = Printf.sprintf
              "Expected exactly one binder for a variable valence (%s) term (multiple \
              variables are bound in the pattern), saw %n"
              (string_of_valence valence)
              (List.length binders)
            }
    in

    go String.Map.empty

let%test_module "CheckTerm" = (module struct
  let as_parser = AbstractSyntax.Parser.language_def
  let as_lexer = AbstractSyntax.Lexer.read

  let term_parser = Term_Parser.top_term
  let term_lexer = Term_Lexer.read

  let parse_lang lang_str = as_parser as_lexer (Lexing.from_string lang_str)
  let parse_term term_str = term_parser term_lexer (Lexing.from_string term_str)

  let lang_desc =
      {|
import {integer} from "builtins"

value :=
  | unit()
  | lit-int(integer())
  | lit-str(string())
  | list(list(value()))

list(a) :=
  | nil()
  | cons(a; list(a))

match-line :=
  | match-line(value()*. term())

term :=
  | lambda(value(). term())
  | alt-lambda(term(). term())
  | match(match-line()*)
  | value(value())
      |}

  let language = parse_lang lang_desc

  let check_pattern' sort_str pat_str =
    let sort = sort_str
      |> parse_term
      |> sort_of_term_exn
    in
    let pat = pat_str
      |> parse_term
      |> Nominal.to_pattern_exn
    in
    match check_pattern language sort pat with
    | Error failure -> Fmt.epr "%a" pp_failure failure
    | Ok _ -> ()

  let%expect_test _ =
    check_pattern' "value()" "unit()";
    [%expect]

  let%expect_test _ =
    check_pattern' "value()" "lit-int(1)";
    [%expect]

  let%expect_test _ =
    check_pattern' "value()" {|lit-str("str")|};
    [%expect]

  let%expect_test _ =
    check_pattern' "list(value())" {|nil()|};
    [%expect]

  let%expect_test _ =
    check_pattern' "list(value())" {|cons(unit(); nil())|};
    [%expect]

  let%expect_test _ =
    check_pattern' "term()" {|value(list(cons(unit(); nil())))|};
    [%expect]

  let%expect_test _ =
    check_pattern' "term()" {|value(list(cons(a; nil())))|};
    [%expect]

  let%expect_test _ =
    check_pattern' "term()" {|value(list(cons(a; a)))|};
    [%expect{| Did you mean to bind the same variable (a) twice in the same pattern? That's not allowed! |}]

  let%expect_test _ =
    check_pattern' "term()" "lambda(a)";
    [%expect]

  let%expect_test _ =
    check_pattern' "match-line()" "match-line(a)";
    [%expect]

  let%expect_test _ =
    check_pattern' "integer()" {|"foo"|};
    [%expect {|
      Unexpected sort (integer()) for a primitive ("foo")
      stack:
      term: "foo", sort: integer()
    |}]

  let%expect_test _ =
    check_pattern' "value()" "foo()";
    [%expect{| check_pattern: failed to find operator foo in sort value |}]

  let%expect_test _ =
    check_pattern' "value()" "unit(1)";
    [%expect{| Wrong number of subterms (1) for this arity () |}]

  let%expect_test _ =
    check_pattern' "term()" "lambda(1)";
    [%expect{| Invalid variable-valence (value(). term()) pattern. Only a variable is valid. |}]

  let%expect_test _ =
    check_pattern' "term()" "match(a; b)";
    [%expect{||}]

  let check_term' sort_str tm_str =
    let sort = sort_str
      |> parse_term
      |> sort_of_term_exn
    in
    match check_term language sort (parse_term tm_str) with
    | Some failure -> Fmt.epr "%a" pp_failure failure
    | None -> ()

  let%expect_test _ =
    check_term' "term()" "lambda(a. value(a))";
    [%expect]

  let%expect_test _ =
    check_term' "term()"
    {|match(
        match-line(
          list(cons(a; nil())).
          value(list(cons(a; cons(a; nil()))))
        );
        match-line(_. value(list(nil())))
      )
    |};
    [%expect]

  let%expect_test _ =
    check_term' "term()" "unit()";
    [%expect {| check_term: failed to find operator unit in sort term |}]

  let%expect_test _ =
    check_term' "term()" "lambda(a. b)";
    [%expect {|
      Unknown variable b (is it bound?)
      stack:
      term: b, sort: term() |}]

  let%expect_test _ =
    check_term' "term()" "lambda(val. alt-lambda(tm. val))";
    [%expect {|
      Variable val has unexpected valence (saw: value()) (expected: term())
      stack:
      term: val, sort: term() |}]

  let%expect_test _ =
    check_term' "value()" {|lit-int("foo")|};
    [%expect {|
      Unexpected primitive sort
      stack:
      term: "foo", sort: integer() |}]

  let%expect_test _ =
    check_term' "value()" "lit-str(123)";
    [%expect {|
      Unexpected primitive sort
      stack:
      term: 123, sort: string() |}]

  let%expect_test _ =
    check_term' "term()" "lambda(a; b)";
    [%expect {| Wrong number of subterms (2) for this arity (value(). term()) |}]

  let%expect_test _ =
    check_term' "value()" "lit-int(1; 2)";
    [%expect {| Wrong number of subterms (2) for this arity (integer()) |}]

  let%expect_test _ =
    check_term' "match-line()" "match-line(a. b. value(a))";
    [%expect {| Expected exactly one binder for a variable valence (value()*. term()) term (multiple variables are bound in the pattern), saw 2 |}]

  let%expect_test _ =
    check_term' "match-line()" "match-line(a. a)";
    [%expect{|
      Variable a has unexpected valence (saw: value()) (expected: term())
      stack:
      term: a, sort: term() |}]

  let%expect_test _ =
    check_term' "term()" "lambda(a. b. a)";
    [%expect {| Wrong number of binders (2) for this valence (value(). term()) |}]

  let%expect_test _ =
    check_term' "term()" "lambda(list(cons(a; cons(b; nil))). value(a))";
    [%expect {| Fixed-valence binders must all be vars (no patterns) |}]

  let%expect_test _ =
    check_term' "term()" "match(a. a)";
    [%expect {| Unexpectedly found a binder where terms were expected (variable arity: match-line()*) |}]

end);;
