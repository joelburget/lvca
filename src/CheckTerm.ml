open Core_kernel

open AbstractSyntax_Types
module Types = AbstractSyntax_Types
open Binding

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
    List.iter stack ~f:(fun { term; sort } ->
      Format.print_newline ();
      begin
        match term with
        | First pat -> Pattern.pp ppf pat
        | Second tm -> Binding.Nominal.pp_term ppf tm
      end;
      Format.print_space ();
      pp_sort ppf sort
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

let concretize_arity : sort Core_kernel.String.Map.t -> arity -> arity
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
      | Sequence _ -> failwith "TODO: sequence sort 1"
      | Operator (op_name, subpats) -> match sort with
        | SortVar _ -> Util.invariant_violation "check_pattern: non-concrete sort"
        | SortAp (sort_name, sort_args) -> (match lookup_operator' sort_name op_name with
          | None -> failwith "TODO: error"
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
          ; message = "TODO: go_fixed_arity_pat message"
          }
        | Ok pat_valences ->
          pat_valences
            |> List.map
              ~f:(fun (pat, valence) -> match pat, valence with
                (* The only thing that can match with a non-sort valence is a var *)
                | Var name, _ -> Ok (String.Map.singleton name valence)
                | Ignored _, _ -> Ok String.Map.empty

                (* For anything else matching a sort we defer to go_pattern *)
                | _, FixedValence ([], sort) -> go_pattern sort pat

                (* Anything else is an error *)
                | Sequence _, _
                | Primitive _, _
                | Operator _, _ -> Error
                  { stack = []
                  ; message = "TODO: go_fixed_arity_pat message (2)"
                  }
              )
            |> Result.all
            |> Result.map ~f:Util.string_map_unions

    and go_variable_arity_pat
      :  sort
      -> Pattern.t list
      -> (valence String.Map.t, abstract_syntax_check_failure) Result.t
      = fun sort subterms -> match subterms with
        | [body] -> go_pattern sort body
        | _ -> Error
          { stack = []
          ; message = "TODO: go_variable_arity_pat message"
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
        | Sequence _ ->
            Printf.printf "%s\n" (string_of_sort sort);
            Printf.printf "%s\n" (Binding.Nominal.pp_term' tm);
            failwith "TODO: sequence sort 2"
        | Var v -> (match Map.find var_valences v with
          | None -> failwith "TODO"
          | Some sort' -> if Caml.(sort' = FixedValence ([], sort))
            then None
            else Some (failwith "TODO")
        )
        | Primitive p -> (match p, sort with
          | PrimInteger _, SortAp ("integer", []) -> None
          | PrimString _, SortAp ("string", []) -> None
          | _, _ -> Some (failwith "TODO: error"))

        | Operator (operator_name, op_scopes) -> (match sort with
          | SortVar _ -> invariant_violation "check_term (go): non-concrete sort"
          | SortAp (sort_name, sort_args)
          -> (match lookup_operator' sort_name operator_name with
            | None -> failwith "TODO: error"
            | Some (vars, OperatorDef (_, arity)) ->
              Printf.printf "go operator child arity: %s\n" (string_of_arity arity);
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
        | Unequal_lengths -> failwith "TODO: error"
        | Ok scope_valences -> List.find_map scope_valences (* TODO: go_fixed_arity *)
          ~f:(fun (scope, valence) -> go_scope var_valences valence scope)

    and go_variable_arity
      :  valence String.Map.t
      -> sort
      -> Nominal.scope list
      -> abstract_syntax_check_failure option
      = fun var_valences sort subterms -> subterms
        |> List.map ~f:(function
          | Scope ([], body) -> go var_valences sort body
          | _ -> failwith "TODO: error"
        )
        |> List.find_map ~f:Fn.id

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
          | Ok binders_env' ->
            let binders_env'' = Util.string_map_unions binders_env' in
            (* TODO: are we shadowing in right direction? *)
            go (Util.map_union var_valences binders_env'') body_sort body
        in

        match valence with
        | FixedValence (binder_sorts, body_sort) ->
          (match List.zip binder_sorts binders with
            | Unequal_lengths -> failwith "TODO: error"
            | Ok binders' -> binders'
              |> List.map ~f:(fun (sort, pat) -> check_pattern' sort pat)
              |> go_body body_sort)
        | VariableValence (binder_sort, body_sort) -> binders
              |> List.map ~f:(check_pattern' binder_sort)
              |> go_body body_sort
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
      |> Binding.Nominal.to_pattern_exn
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
    check_pattern' "term()" "lambda(a)";
    [%expect]

  let%expect_test _ =
    check_pattern' "match-line()" "match-line(a)";
    [%expect]

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

  (* XXX how do variable-arity matches work? *)

  let%expect_test _ =
    check_term' "term()"
    {|match([
        match-line(cons(a; nil()). cons(a; cons(a; nil()))),
        match-line(_. nil())
      ])
    |};
    [%expect]

end);;
