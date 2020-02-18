open Core_kernel
open Types
open Binding

let (length, map) = List.(length, map)
let get_first, map_union, map_unions = Util.(get_first, map_union, map_unions)
;;

type core =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * core_scope list
  | Var of string
  | Sequence of core list
  | Primitive of primitive
  (* plus, core-specific ctors *)
  | Lambda of sort list * core_scope
  | CoreApp of core * core list
  | Case of core * core_case_scope list
  | Let of core * core_scope

and core_scope = Scope of Pattern.t list * core
and core_case_scope = CaseScope of BindingAwarePattern.t list * core

type denotation_chart = DenotationChart of (string * core) list

(** Raised by to_ast when the presence of lambda, let, app, or case make
    the value invalid
*)
exception ToAstConversionErr of core

let rec to_ast : core -> Nominal.term = function
  | Var name -> Var name
  | Operator (tag, vals) -> Operator (tag, map vals ~f:scope_to_ast)
  | Primitive prim -> Primitive prim
  | Sequence tms -> Sequence (map tms ~f:to_ast)
  | (Lambda _ | Let _ | CoreApp _ | Case _) as core_only_term
  -> raise @@ ToAstConversionErr core_only_term

and scope_to_ast (Scope (pats, body)) = Nominal.Scope (pats, to_ast body)

let rec match_core_pattern
  : core -> BindingAwarePattern.t -> core String.Map.t option
  = fun v pat ->
  match v, pat with
  | Operator (tag1, vals), Operator (tag2, pats) ->
    if String.(tag1 = tag2) && length vals = length pats
    then
      let sub_results = List.map2_exn vals pats
        ~f:(fun core_scope (pat : BindingAwarePattern.scope) ->
        match core_scope, pat with
          | Scope ([], body), Scope ([], pat') -> match_core_pattern body pat' (* XXX *)
          | _ -> None
      )
      in
      if List.for_all sub_results ~f:Option.is_some
      then Some (sub_results
        |> map ~f:(Util.get_option' (fun () -> "we just check all is_some"))
        |> map_unions)
      else None
    else None
  | Sequence s1, Sequence s2 ->
    if length s1 = length s2
    then
      let sub_results = List.map2_exn s1 s2 ~f:match_core_pattern in
      if List.for_all sub_results ~f:Option.is_some
      then Some (sub_results
        |> map ~f:(Util.get_option' (fun () -> "we just check all is_some"))
        |> map_unions)
      else None
    else None
  | Primitive l1, Primitive l2 -> if prim_eq l1 l2 then Some String.Map.empty else None
  | _, Var "_" -> Some String.Map.empty
  | tm, Var v -> Some (String.Map.of_alist_exn [ v, tm ])
  | _ -> None
;;

let find_core_match
  : core -> core_case_scope list -> (core * core String.Map.t) option
  = fun v branches -> branches
    |> get_first (function
      | CaseScope ([ pat ], rhs) -> (match match_core_pattern v pat with
        | None -> None
        | Some bindings -> Some (rhs, bindings))
      | _ -> failwith "invariant violation: match binding more than one pattern"
    )
;;

exception EvalError of string

let eval
  : core -> (core, string) Result.t
  = fun core ->
  let rec go
    : core String.Map.t -> core -> core
    = fun ctx tm -> match tm with
    | Var v ->
      (match String.Map.find ctx v with
       | Some result -> result
       | None -> raise @@ EvalError ("Unbound variable " ^ v))
    | CoreApp (Lambda (_tys, Scope (arg_patterns, body)), args) ->
      if length arg_patterns <> length args
      then raise @@ EvalError "mismatched application lengths"
      else
        let arg_vals = map args ~f:(go ctx) in
        let new_args : core String.Map.t =
          List.map2_exn arg_patterns arg_vals
            ~f:(fun (pat : Pattern.t) arg_val -> match pat with
            | Var name -> name, arg_val
            | _ -> raise @@ EvalError "Unsupported pattern in lambda (only vars allowed)"
            )
          |> String.Map.of_alist
          |> (function
            | `Duplicate_key str
            -> raise @@ EvalError ("Duplicate variable name binding: " ^ str)
            | `Ok result -> result)
        in
        go (map_union ctx new_args) body
    | Case (tm, branches) ->
      (match find_core_match (go ctx tm) branches with
        | None -> raise @@ EvalError "no match found in case"
        | Some (branch, bindings) -> go (map_union ctx bindings) branch)
    (* TODO: or should this be an app? *)
    | Operator ("#add", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
         Primitive (PrimInteger (Bigint.(a' + b')))
       | _ -> raise @@ EvalError "TODO")
    | Operator ("#sub", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
         Primitive (PrimInteger (Bigint.(a' - b')))
       | _ -> raise @@ EvalError "TODO")
    | Operator _ | Sequence _ | Primitive _ -> tm
    (* TODO: include the term in error *)
    | _ -> raise @@ EvalError "Found a term we can't evaluate"
  in
  try
    Ok (go String.Map.empty core)
  with
    EvalError msg -> Error msg
;;
