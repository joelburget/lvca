open Belt
open Types
module DeBruijn = Binding.DeBruijn
module Nominal = Binding.Nominal
let (fold_right, get_first, traverse_list_result, union, sequence_list_result)
  = Util.(fold_right, get_first, traverse_list_result, union,
    sequence_list_result)

module M = Belt.Map.String
module O = Belt.Option

type denotation_scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * denotation_scope_pat list
  | DVar       of string option

type core_pat =
  | PatternTerm     of string * core_binding_pat list
  | PatternVar      of string option
  | PatternSequence of core_pat list
  | PatternPrim     of primitive
  | PatternDefault

and core_binding_pat = CoreBindingPat of string list * core_pat

type core =
  (* first four constructors correspond to regular term constructors *)
  | Operator  of string * core_scope list
  | Var       of string
  | Sequence  of core list
  | Primitive of primitive

  (* plus, core-specific ctors *)
  | Lambda   of core_scope (*  string list * core *)
  | CoreApp  of core * core list
  | Case     of core * sort * (core_pat * core_scope) list
  (** A metavariable refers to a term captured directly from the left-hand-side
   *)
  | Metavar  of string
  (** Meaning is very similar to a metavar in that it refers to a capture from
   the left-hand-side. However, a meaning variable is interpreted. *)
  | Meaning  of string

and core_scope = CoreScope of string list * core

type assoc =
  { pattern_name : string;
    term_name    : string;
  }

type denotation_chart
  = DenotationChart of (denotation_pat * core) list

type located_err = string * DeBruijn.term option
type 'a translation_result = ('a, located_err) Result.t

(** Raised by to_ast when the presence of metavars, lambdas, and cases make
   the value invalid
 *)
exception AstConversionErr of core

let rec to_ast (core : core) : Nominal.term = match core with
  | Var name
  -> Var name
  | Operator (tag, vals)
  -> Operator (tag, List.map vals scope_to_ast)
  | Primitive prim
  -> Primitive prim
  | Sequence tms
  -> Sequence (List.map tms to_ast)
  | Lambda _ as v -> raise @@ AstConversionErr v
  | CoreApp _
  | Case _
  | Metavar _
  | Meaning _ -> raise @@ AstConversionErr core

and scope_to_ast (CoreScope (names, body)) = Nominal.Scope (names, to_ast body)

(* val match_branch : core -> core_pat -> core M.t option *)
let rec match_branch v pat = match (v, pat) with

  | (Operator (tag1, vals), PatternTerm (tag2, pats)) -> List.(
    let sub_results = zipBy vals pats match_binding_branch in
    if tag1 = tag2 && length vals = length pats && every sub_results O.isSome
    then Some (reduce (map sub_results O.getExn) M.empty union)
    else None
  )
  | (Sequence s1, PatternSequence s2)
  -> List.(
    let sub_results = zipBy s1 s2 match_branch in
    if length s1 = length s2 && every sub_results O.isSome
    then Some (reduce (map sub_results O.getExn) M.empty union)
    else None
  )
  | (Primitive l1, PatternPrim l2)
  -> if prim_eq l1 l2 then Some M.empty else None
  | (tm, PatternVar (Some v))
  -> Some (M.fromArray [|v,tm|])
  | (_, PatternVar None)
  | (_, PatternDefault)
  -> Some M.empty
  | _
  -> None

and match_binding_branch (CoreScope (names, tm)) (CoreBindingPat (names', pat))
  = match_branch tm pat (* XXX names? *)

let rec find_core_match v
  : (core_pat * core_scope) list -> (core * core M.t) option
  = function
  | []
  -> None
  | (pat, CoreScope (_, rhs)) :: pats
  -> (match match_branch v pat with
    | None          -> find_core_match v pats
    | Some bindings -> Some (rhs, bindings)
    )

(* val matches
  : DeBruijn.term
  -> denotation_pat
  -> (assoc list * DeBruijn.term M.t) option
*)
let rec matches tm pat = match tm, pat with
  | DeBruijn.Operator (tag1, subtms), DPatternTm (tag2, subpats)
  -> if tag1 == tag2 && List.(length subtms == length subpats)
     then fold_right
       (fun ((scope, subpat), b_opt) ->
         match matches_scope scope subpat, b_opt with
         | Some (assocs, bindings), Some (assocs', bindings')
         -> Some (assocs @ assocs', union bindings bindings')
         | _ -> None)
       (List.zip subtms subpats)
       (Some ([], M.empty))
     else None
  | _, DPatternTm _   -> None
  | _, DVar None      -> Some ([], M.empty)
  | tm, DVar (Some v) -> Some ([], M.fromArray [|v,tm|])

(* val matches_scope
  : DeBruijn.scope
  -> denotation_scope_pat
  -> (assoc list * DeBruijn.term M.t) option
*)
and matches_scope (Scope (binders, tm)) (DenotationScopePat (patBinders, pat))
  = if List.(length patBinders == length binders)
    then O.map
      (matches tm pat)
      (fun (assocs, tmMatches)
      -> List.zipBy patBinders binders
           (fun pattern_name term_name -> {pattern_name; term_name})
         @ assocs
         , tmMatches
      )
    else None

(* val find_match
  : denotation_chart
  -> DeBruijn.term
  -> (assoc list * DeBruijn.term M.t * core) option
*)
let find_match (DenotationChart denotations) term = get_first
  (fun (pat, core) -> O.map
    (matches term pat)
    (fun (assocs, bindings) -> (assocs, bindings, core)))
  denotations

(* val fill_in_core
  : denotation_chart
  -> assoc list * DeBruijn.term M.t
  -> core
  -> core translation_result
*)
(** Fill in a core representation of a value with metavars (the raw
  right-hand-side of a denotation chart) with the core terms that go there.
  *)
let rec fill_in_core (dynamics : denotation_chart) (vars : string list)
  ((assocs, assignments) as mr : assoc list * DeBruijn.term M.t) = function
  | Metavar name -> (match M.get assignments name with
    | Some tm -> term_to_core [] tm
    | None    -> Result.Error ("Metavariable " ^ name ^ " not found", None)
    )
  | Meaning name -> (match M.get assignments name with
    | Some tm -> term_denotation dynamics vars tm
    | None    -> Result.Error ("Metavariable " ^ name ^ " not found", None)
    )
  | Var _ as c -> Ok c
  | Operator (tag, vals) -> Result.map
    (traverse_list_result (fill_in_core_scope dynamics vars mr) vals)
    (fun vals' -> Operator (tag, vals'))
  | Primitive _  as v -> Ok v
  | Sequence tms -> Result.map
    (traverse_list_result (fill_in_core dynamics vars mr) tms)
    (fun tms' -> Sequence tms')
  | Lambda body -> Result.map
    (fill_in_core_scope dynamics vars mr body)
    (fun body' -> Lambda body')
  | CoreApp (f, args) -> (match
    fill_in_core dynamics vars mr f,
    mr
      |> fill_in_core dynamics vars
      |> List.map args
      |> sequence_list_result
     with
    | Ok f', Ok args'             -> Ok (CoreApp (f', args'))
    | Error msg, _ | _, Error msg -> Error msg
    )
  (* | Lam (binders, core) -> Result.map
    (fill_in_core dynamics vars mr core)
    (fun core' -> Lam (binders, core')) *)
  | Case (scrutinee, ty, branches) ->
      let mBranches : ((core_pat * core_scope) list) translation_result =
        traverse_list_result
          (fun (pat, scope) -> Result.map
            (fill_in_core_scope dynamics vars mr scope)
            (fun scope' -> (pat, scope'))
          )
          branches
      in
      match fill_in_core dynamics vars mr scrutinee, mBranches with
        | Ok scrutinee', Ok branches'
        -> Ok (Case (scrutinee', ty, branches'))
        | Error msg, _ | _, Error msg
        -> Error msg

(* XXX use assocs *)
and fill_in_core_scope dynamics vars mr (CoreScope (names, body)) =
  Result.map
  (fill_in_core dynamics (names @ vars) mr body)
  (fun body' -> CoreScope (names, body'))

(** Translate a term directly to core, with no interpretation *)
and term_to_core env tm = match tm with
  | Operator (tag, subtms) -> Result.map
    (traverse_list_result (scope_to_core env) subtms)
    (fun subtms' -> Operator (tag, subtms'))
    (* XXX
  | Var ix -> (match List.get env ix with
    | None -> Error ("failed to look up variable", Some tm)
    | Some name -> Ok (Var name)
    )
    *)
  | Sequence tms -> Result.map
    (traverse_list_result (term_to_core env) tms)
    (fun tms' -> Sequence tms')
  | Primitive prim -> Ok (Primitive prim)

and scope_to_core env (Scope (names, body)) = Result.map
  (term_to_core env body)
  (fun body' -> CoreScope (names, body')) (* XXX change names (assocs)? *)

(* val term_denotation
  : denotation_chart -> DeBruijn.term -> core translation_result
*)
(** Match a term in the denotation chart, and return its core term. *)
and term_denotation dynamics vars tm = match tm with
  | DeBruijn.Var i ->
      (match List.get vars i with
    | Some var_name -> Ok (Var var_name)
    | None -> Error ("couldn't find variable " ^ string_of_int i ^
      " in variable context of size " ^ string_of_int (List.length vars),
      Some tm)
  )
  | _ -> (match find_match dynamics tm with
    | None
    -> Error ("no match found", Some tm)
    | Some (assocs, bindings, protoCore)
    -> fill_in_core dynamics vars (assocs, bindings) protoCore
  )

(* val eval : core -> (core, string) Result.t *)
let eval core =
  let open Belt.Result in

  let rec go ctx tm = match tm with
        | Var v -> (match M.get ctx v with
          | Some result -> Ok result
          | None        -> Error ("Unbound variable " ^ v)
        )
        | CoreApp (Lambda (CoreScope (argNames, body)), args) ->
            if List.(length argNames != length args)
            then Error "mismatched application lengths"
            else Result.flatMap
              (sequence_list_result (List.map args (go ctx)))
              (fun arg_vals ->
                 let new_args = List.(arg_vals
                   |> zip argNames
                   |> toArray
                   |> M.fromArray
                 ) in
                 go (union ctx new_args) body
              )
        | Case (tm, _ty, branches) -> Result.flatMap (go ctx tm)
          (fun v -> (match find_core_match v branches with
            | None
            -> Error "no match found in case"
            | Some (branch, bindings)
            -> go (union ctx bindings) branch
          ))
        | Metavar _v | Meaning _v -> Error "Found a metavar!"

        | Operator _  | Sequence _ | Primitive _
        -> Ok tm

        (* TODO: include the term in error *)
        | _ -> Error "Found a term we can't evaluate"

  in go M.empty core
