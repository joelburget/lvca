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

and core_case_scope = CaseScope of Pattern.t * core

module PP = struct
  open Format

  (* TODO: add parse <-> pretty tests *)

  let rec pp_core
    = fun ppf -> function
      | Operator (tag, subtms) -> fprintf ppf "@[%s(%a)@]" tag pp_core_scope_list subtms
      | Var v -> fprintf ppf "%s" v
      | Sequence tms -> fprintf ppf "@[[%a]@]" pp_core_list tms
      | Primitive p -> Primitive.pp ppf p
      | Lambda (_sorts, Scope(_pats, body)) ->
        fprintf ppf "\\%s -> %a"
        "TODO"
        pp_core body
        (*
        (List.zip_exn pats sorts
          |> List.map ~f:
            *)
      (* TODO: parens*)
      | CoreApp (f, args) -> fprintf ppf "%a %a" pp_core f pp_core_list args
      (* XXX newines *)
      | Case (arg, case_scopes) -> fprintf ppf "match %a with { %a }"
        pp_core arg pp_case_scopes case_scopes
      | Let (tm, Scope([pat], body)) -> fprintf ppf "let %a = %a in %a"
        Pattern.pp pat pp_core tm pp_core body
      | _ -> failwith "TODO"

  and pp_core_list ppf = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a" pp_core x
    | x :: xs -> fprintf ppf "%a, %a" pp_core x pp_core_list xs

  and pp_core_scope_list ppf = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a" pp_core_scope x
    | x :: xs -> fprintf ppf "%a; %a" pp_core_scope x pp_core_scope_list xs

  and pp_core_scope ppf = fun (Scope (bindings, body)) -> match bindings with
    | [] -> pp_core ppf body
    | _ -> fprintf ppf "%a %a" pp_bindings bindings pp_core body

  and pp_case_scopes ppf = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a" pp_core_case_scope x
    (* XXX newlines *)
    | x :: xs -> fprintf ppf "%a | %a" pp_core_case_scope x pp_case_scopes xs

  and pp_core_case_scope : Format.formatter -> core_case_scope -> unit
    = fun ppf (CaseScope (pat, body))
    -> fprintf ppf "%a -> %a" Pattern.pp pat pp_core body

  (* TODO: remove duplication *)
  and pp_bindings ppf = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a." Pattern.pp x
    | x :: xs -> fprintf ppf "%a. %a" Pattern.pp x pp_bindings xs
end

let pp_core : Format.formatter -> core -> unit
  = PP.pp_core

let pp_core_str : core -> string
  = Format.asprintf "%a" pp_core

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
         | CaseScope (pat, rhs) ->
           (match match_core_pattern v pat with
           | None -> None
           | Some bindings -> Some (rhs, bindings)))
;;

type eval_error = string * core

exception EvalExn of string * core

let eval : core -> (core, eval_error) Result.t =
 fun core ->
  let rec go : core String.Map.t -> core -> core =
   fun ctx tm ->
    match tm with
    | Var v ->
      (match Map.find ctx v with
      | Some result -> result
      | None -> raise @@ EvalExn ("Unbound variable " ^ v, tm))
    | CoreApp (Lambda (_tys, Scope (arg_patterns, body)), args) ->
      if List.(length arg_patterns <> length args)
      then raise @@ EvalExn ("mismatched application lengths", tm)
      else (
        let arg_vals = List.map args ~f:(go ctx) in
        let new_args : core String.Map.t =
          List.map2_exn arg_patterns arg_vals ~f:(fun (pat : Pattern.t) arg_val ->
              match pat with
              | Var name -> name, arg_val
              | _ ->
                raise @@ EvalExn ("Unsupported pattern in lambda (only vars allowed)", tm))
          |> String.Map.of_alist
          |> function
          | `Duplicate_key str ->
            raise @@ EvalExn ("Duplicate variable name binding: " ^ str, tm)
          | `Ok result -> result
        in
        go (Util.map_union ctx new_args) body)
    | Case (tm, branches) ->
      (match find_core_match (go ctx tm) branches with
      | None -> raise @@ EvalExn ("no match found in case", tm)
      | Some (branch, bindings) -> go (Util.map_union ctx bindings) branch)

    (* primitives *)
    (* TODO: or should this be an app? *)
    | Operator ("#add", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
      | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
        Primitive (PrimInteger Bigint.(a' + b'))
      | _ -> raise @@ EvalExn ("Invalid arguments to #add", tm))
    | Operator ("#sub", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
      | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
        Primitive (PrimInteger Bigint.(a' - b'))
      | _ -> raise @@ EvalExn ("Invalid arguments to #sub", tm))

    | Operator _ | Sequence _ | Primitive _ -> tm
    | _ -> raise @@ EvalExn ("Found a term we can't evaluate", tm)
  in
  try Ok (go String.Map.empty core) with EvalExn (msg, tm) -> Error (msg, tm)
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
