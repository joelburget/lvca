open Core_kernel
open AbstractSyntax
open Binding

type core =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * core_scope list
  | Var of string
  | Sequence of core list
  | Primitive of Primitive.t
  (* plus, core-specific ctors *)
  | Lambda of sort list * core_scope
  | CoreApp of core * core list
  | Case of core * core_case_scope list
  | Let of core * core_scope

and core_scope = Scope of Pattern.t list * core

and core_case_scope = CaseScope of Pattern.t list * core

type denotation_chart = DenotationChart of (string * core) list

(** Raised by to_ast when the presence of lambda, let, app, or case make the value invalid *)
exception ToAstConversionErr of core

let rec to_ast : core -> Nominal.term = function
  | Var name -> Var name
  | Operator (tag, vals) -> Operator (tag, List.map vals ~f:scope_to_ast)
  | Primitive prim -> Primitive prim
  | Sequence tms -> Sequence (List.map tms ~f:to_ast)
  | (Lambda _ | Let _ | CoreApp _ | Case _) as core_only_term ->
    raise @@ ToAstConversionErr core_only_term

and scope_to_ast (Scope (pats, body)) = Nominal.Scope (pats, to_ast body)

let rec match_core_pattern : core -> Pattern.t -> core String.Map.t option =
  fun v pat -> match v, pat with
  | Operator (tag1, vals), Operator (tag2, pats) ->
    if String.(tag1 = tag2) && List.(length vals = length pats)
    then (
      let sub_results =
        List.map2_exn vals pats ~f:(fun core_scope (pat : Pattern.t) ->
            match core_scope, pat with
            | Scope ([], body), pat' -> match_core_pattern body pat'
            | _ -> None)
      in
      if List.for_all sub_results ~f:Option.is_some
      then
        Some
          (sub_results
          |> List.map ~f:(Util.get_option' (fun () -> "we just check all is_some"))
          |> Util.string_map_unions)
      else None)
    else None
  | Sequence s1, Sequence s2 ->
    if List.(length s1 = length s2)
    then (
      let sub_results = List.map2_exn s1 s2 ~f:match_core_pattern in
      if List.for_all sub_results ~f:Option.is_some
      then
        Some
          (sub_results
          |> List.map ~f:(Util.get_option' (fun () -> "we just check all is_some"))
          |> Util.string_map_unions)
      else None)
    else None
  | Primitive l1, Primitive l2
  -> if Primitive.(l1 = l2) then Some String.Map.empty else None
  | _, Var "_" -> Some String.Map.empty
  | tm, Var v -> Some (String.Map.of_alist_exn [ v, tm ])
  | _ -> None
;;

let find_core_match : core -> core_case_scope list -> (core * core String.Map.t) option =
 fun v branches ->
  branches
  |> List.find_map ~f:(function
         | CaseScope ([ pat ], rhs) ->
           (match match_core_pattern v pat with
           | None -> None
           | Some bindings -> Some (rhs, bindings))
         | _ -> failwith "invariant violation: match binding more than one pattern")
;;

exception EvalError of string

let eval : core -> (core, string) Result.t =
 fun core ->
  let rec go : core String.Map.t -> core -> core =
   fun ctx tm ->
    match tm with
    | Var v ->
      (match Map.find ctx v with
      | Some result -> result
      | None -> raise @@ EvalError ("Unbound variable " ^ v))
    | CoreApp (Lambda (_tys, Scope (arg_patterns, body)), args) ->
      if List.(length arg_patterns <> length args)
      then raise @@ EvalError "mismatched application lengths"
      else (
        let arg_vals = List.map args ~f:(go ctx) in
        let new_args : core String.Map.t =
          List.map2_exn arg_patterns arg_vals ~f:(fun (pat : Pattern.t) arg_val ->
              match pat with
              | Var name -> name, arg_val
              | _ ->
                raise @@ EvalError "Unsupported pattern in lambda (only vars allowed)")
          |> String.Map.of_alist
          |> function
          | `Duplicate_key str ->
            raise @@ EvalError ("Duplicate variable name binding: " ^ str)
          | `Ok result -> result
        in
        go (Util.map_union ctx new_args) body)
    | Case (tm, branches) ->
      (match find_core_match (go ctx tm) branches with
      | None -> raise @@ EvalError "no match found in case"
      | Some (branch, bindings) -> go (Util.map_union ctx bindings) branch)
    (* TODO: or should this be an app? *)
    | Operator ("#add", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
      | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
        Primitive (PrimInteger Bigint.(a' + b'))
      | _ -> raise @@ EvalError "TODO")
    | Operator ("#sub", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
      | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
        Primitive (PrimInteger Bigint.(a' - b'))
      | _ -> raise @@ EvalError "TODO")
    | Operator _ | Sequence _ | Primitive _ -> tm
    (* TODO: include the term in error *)
    | _ -> raise @@ EvalError "Found a term we can't evaluate"
  in
  try Ok (go String.Map.empty core) with EvalError msg -> Error msg
;;

(* to_term *)

let rec term_of_core : core -> Nominal.term
  = function
  | Operator (name, _scopes) -> Operator ("operator",
    [ Scope ([], Primitive (PrimString name))
    ])
  | Var name -> Var name
  | Sequence cores -> Sequence (List.map cores ~f:term_of_core)
  | Primitive p -> Primitive p
  (* plus, core-specific ctors *)
  | Lambda (sorts, scope) -> Operator ("lambda",
    [ Scope ([], Operator ("sorts",
      [ Scope ([], Sequence (sorts
        |> List.map ~f:(fun sort -> sort
          |> AbstractSyntax.term_of_sort
          |> NonBinding.to_nominal)
      ))
      ]))
    ; scope_of_core_scope scope
    ])
  | CoreApp (f, args) -> Operator ("core_app",
    [ Scope ([], term_of_core f)
    ; Scope ([], Sequence (List.map args ~f:term_of_core))
    ])
  | Case (tm, _branches) -> Operator ("case",
    [ Scope ([], term_of_core tm)
    (* TODO *)
    (* ; Scope ([], Sequence (List.map branches ~f:scope_of_core_case_scope)) *)
    ])
  | Let (tm, body) -> Operator ("let",
    [ Scope ([], term_of_core tm)
    ; scope_of_core_scope body
    ])

and scope_of_core_scope : core_scope -> Nominal.scope
  = fun (Scope (pats, body)) -> Scope (pats, term_of_core body)

  (*
and scope_of_core_case_scope : core_case_scope -> Nominal.scope
  = fun (CaseScope (baw_pat, body)) -> failwith "TODO"
  *)

let to_term : denotation_chart -> Nominal.term
  = fun (DenotationChart lines) -> Sequence (List.map lines
    ~f:(fun (name, core) -> Nominal.Operator ("pair",
      [ Scope ([], Primitive (PrimString name))
      ; Scope ([], term_of_core core)
      ])))
