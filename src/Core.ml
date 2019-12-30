open Types
open Binding

let vars_of_pattern, vars_of_patterns = Pattern.(vars_of_pattern, vars_of_patterns)
let empty_set, set_union = Belt.Set.String.(empty, union)
let empty_map, fromArray = Belt.Map.String.(empty, fromArray)
let every, length, map, toArray, zipBy =
  Belt.List.(every, length, map, toArray, zipBy)
let get_first, map_union, map_unions = Util.(get_first, map_union, map_unions)
;;

(** This type represents the RHS of a denotation rule as parsed. We use
 * denotation_term_to_core to translate it to a core term which can actually be
 * executed.
 * (See produce_denotation_chart : pre_denotation_chart -> denotation_chart).
 * *)
type denotation_term =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * denotation_scope list
  | Var of string
  | Sequence of denotation_term list
  | Primitive of primitive
  (* Also, oxford bracketed var *)
  | Meaning of string

and denotation_scope = Scope of Pattern.t list * denotation_term

type core =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * core_scope list
  | Var of string
  | Sequence of core list
  | Primitive of primitive
  (* plus, core-specific ctors *)
  | Lambda of sort list * core_scope
  | CoreApp of core * core list
  | Case of core * core_scope list
  | Let of core * core_scope
  (** A metavariable refers to a term captured directly from the left-hand-side
  *)
  | Metavar of string
  (** Meaning is very similar to a metavar in that it refers to a capture from
      the left-hand-side. However, a meaning variable is interpreted. *)
  | Meaning of string

and core_scope = Scope of Pattern.t list * core

(* vars is the set of all variables that have been bound by object-language
 * scopes. *)
let rec denotation_term_to_core' vars : denotation_term -> core = function
  | Var s -> if Belt.Set.String.has vars s then Var s else Metavar s
  | Sequence tms -> Sequence (map tms @@ denotation_term_to_core' vars)
  | Primitive p -> Primitive p
  | Meaning v -> Meaning v
  | Operator ("lambda", [Scope (pats, body)])
  ->
    let vars' = set_union vars (vars_of_patterns pats)
    in Lambda ([(* XXX *)], Scope (pats, denotation_term_to_core' vars' body))
  | Operator ("app", [Scope (scope_pats, t); Scope ([], Sequence ts)])
  -> let vars' = set_union vars (vars_of_patterns scope_pats)
     in CoreApp
    (denotation_term_to_core' vars' t, (* XXX scope_pats *)
     map ts @@ denotation_term_to_core' vars)
  | Operator ("case", Scope ([], tm) :: branches) ->
    Case (denotation_term_to_core' vars tm,
      map branches (denotation_scope_to_core vars))
  | Operator ("let", [Scope ([], tm); Scope ([pat], body)]) ->
      Let (denotation_term_to_core' vars tm,
        Scope ([pat], denotation_term_to_core'
          (set_union vars @@ vars_of_pattern pat) body)
        )
  | _ -> failwith "error: unexpected term in denotation_term_to_core'"

and denotation_scope_to_core vars = fun (Scope (pats, body)) ->
  Scope (pats, denotation_term_to_core' vars body)
;;

let denotation_term_to_core : denotation_term -> core
  = denotation_term_to_core' empty_set

let rec sort_from_ast (term : NonBinding.term) : sort =
  match term with
  | Operator (sort_name, subtms) ->
    SortAp (sort_name, subtms
      |. map sort_from_ast
      |. toArray
    )
  | _ -> failwith "TODO: throw"
;;

type pre_denotation_chart =
  DenotationChart of (BindingAwarePattern.t * denotation_term) list
type denotation_chart =
  DenotationChart of (BindingAwarePattern.t * core) list

let produce_denotation_chart : pre_denotation_chart -> denotation_chart =
  fun (DenotationChart lines) ->
  DenotationChart (lines |. map (fun (pat, tm) -> pat, denotation_term_to_core tm))
;;

type located_err = string * DeBruijn.term option
type 'a translation_result = ('a, located_err) Belt.Result.t

(** Raised by to_ast when the presence of metavars, lambdas, and cases make
    the value invalid
*)
exception ToAstConversionErr of core

let rec to_ast : core -> Nominal.term = fun core -> match core with
  | Var name -> Var name
  | Operator (tag, vals) -> Operator (tag, map vals scope_to_ast)
  | Primitive prim -> Primitive prim
  | Sequence tms -> Sequence (map tms to_ast)
  | Lambda _ | Let _ | CoreApp _ | Case _ | Metavar _ | Meaning _
  -> raise @@ ToAstConversionErr core

and scope_to_ast (Scope (pats, body)) = Nominal.Scope (pats, to_ast body)

let rec match_core_pattern
  : core -> Pattern.t -> core Belt.Map.String.t option
  = fun v pat ->
  let isSome, getExn = Belt.Option.(isSome, getExn) in
  match v, pat with
  | Operator (tag1, vals), Operator (tag2, pats) ->
    if tag1 = tag2 && length vals = length pats
    then
      let sub_results = zipBy vals pats (fun core_scope pat ->
        match core_scope with
          | Scope ([], body) -> match_core_pattern body pat
          | _ -> None
      )
      in
      if every sub_results isSome
      then Some (sub_results |. map getExn |. map_unions)
      else None
    else None
  | Sequence s1, Sequence s2 ->
    if length s1 = length s2
    then
      let sub_results = zipBy s1 s2 match_core_pattern in
      if every sub_results isSome
      then Some (sub_results |. map getExn |. map_unions)
      else None
    else None
  | Primitive l1, Primitive l2 -> if prim_eq l1 l2 then Some empty_map else None
  | _, Var "_" -> Some empty_map
  | tm, Var v -> Some (fromArray [| v, tm |])
  | _ -> None
;;

let find_core_match
  : core -> core_scope list -> (core * core Belt.Map.String.t) option
  = fun v branches -> branches
    |> get_first (function
    | Scope ([ pat ], rhs) -> (match match_core_pattern v pat with
      | None -> None
      | Some bindings -> Some (rhs, bindings))
    | _ -> failwith "invariant violation: match binding more than one pattern")
;;

exception EvalError of string

let eval
  : core -> (core, string) Belt.Result.t
  = fun core ->
  let rec go
    : core Belt.Map.String.t -> core -> core
    = fun ctx tm -> match tm with
    | Var v ->
      (match Belt.Map.String.get ctx v with
       | Some result -> result
       | None -> raise @@ EvalError ("Unbound variable " ^ v))
    | CoreApp (Lambda (_tys, Scope (arg_patterns, body)), args) ->
      if length arg_patterns != length args
      then raise @@ EvalError "mismatched application lengths"
      else
        let arg_vals = map args (go ctx) in
        let new_args : core Belt.Map.String.t =
          zipBy arg_patterns arg_vals
            (fun pat arg_val -> match pat with
            | Var name -> name, arg_val
            | _ -> raise @@ EvalError "Unsupported pattern in lambda (only vars allowed)"
            )
          |. toArray
          |. fromArray
        in
        go (map_union ctx new_args) body
    | Case (tm, branches) ->
      (match find_core_match (go ctx tm) branches with
        | None -> raise @@ EvalError "no match found in case"
        | Some (branch, bindings) -> go (map_union ctx bindings) branch)
    | Metavar _v | Meaning _v -> raise @@ EvalError "Found a metavar!"
    (* TODO: or should this be an app? *)
    | Operator ("#add", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
         Primitive (PrimInteger (Bigint.add a' b'))
       | _ -> raise @@ EvalError "TODO")
    | Operator ("#sub", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
         Primitive (PrimInteger (Bigint.sub a' b'))
       | _ -> raise @@ EvalError "TODO")
    | Operator _ | Sequence _ | Primitive _ -> tm
    (* TODO: include the term in error *)
    | _ -> raise @@ EvalError "Found a term we can't evaluate"
  in
  try
    Belt.Result.Ok (go empty_map core)
  with
    EvalError msg -> Error msg
;;
