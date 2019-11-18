open Belt
open Types
module DeBruijn = Binding.DeBruijn
module Nominal = Binding.Nominal
let (fold_right, get_first, traverse_list_result, union)
  = Util.(fold_right, get_first, traverse_list_result, union)

module M = Belt.Map.String
module O = Belt.Option
module S = Belt.Set.String

type denotation_scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * denotation_scope_pat list
  | DVar       of string

type core =
  (* first four constructors correspond to regular term constructors *)
  | Operator  of string * core_scope list
  | Var       of string
  | Sequence  of core list
  | Primitive of primitive

  (* plus, core-specific ctors *)
  | Lambda   of sort list * core_scope
  | CoreApp  of core * core list
  | Case     of core * core_scope list
  (** A metavariable refers to a term captured directly from the left-hand-side
   *)
  | Metavar  of string
  (** Meaning is very similar to a metavar in that it refers to a capture from
   the left-hand-side. However, a meaning variable is interpreted. *)
  | Meaning  of string

and core_scope = CoreScope of string list * core

(** An association of variable names between pattern and term. IE the named
 * pattern variable and term variable are the same. *)
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
exception ToAstConversionErr of core

let rec to_ast (core : core) : Nominal.term = match core with
  | Var name
  -> Var name
  | Operator (tag, vals)
  -> Operator (tag, List.map vals scope_to_ast)
  | Primitive prim
  -> Primitive prim
  | Sequence tms
  -> Sequence (List.map tms to_ast)
  | Lambda _ as v -> raise @@ ToAstConversionErr v
  | CoreApp _
  | Case _
  | Metavar _
  | Meaning _ -> raise @@ ToAstConversionErr core

and scope_to_ast (CoreScope (names, body)) = Nominal.Scope (names, to_ast body)

(* val match_branch : core -> core_pat -> core M.t option *)
let rec match_branch v pat = match v, pat with

  | Operator (tag1, vals), PatternTerm (tag2, pats) -> List.(
    let sub_results = zipBy vals pats match_binding_branch in
    if tag1 = tag2 && length vals = length pats && every sub_results O.isSome
    then Some (reduce (map sub_results O.getExn) M.empty union)
    else None
  )
  | Sequence s1, PatternSequence s2
  -> List.(
    let sub_results = zipBy s1 s2 match_branch in
    if length s1 = length s2 && every sub_results O.isSome
    then Some (reduce (map sub_results O.getExn) M.empty union)
    else None
  )
  | Primitive l1, PatternPrim l2
  -> if prim_eq l1 l2 then Some M.empty else None
  | _, PatternVar "_"
  -> Some M.empty
  | (tm, PatternVar v)
  -> Some (M.fromArray [|v,tm|])
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
  | _, DPatternTm _ -> None
  | _, DVar "_"     -> Some ([], M.empty)
  | tm, DVar v      -> Some ([], M.fromArray [|v,tm|])

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

exception TranslationError of located_err

type fill_in_args =
  { dynamics    : denotation_chart;
    vars        : string list;
    assocs      : assoc list;
    assignments : DeBruijn.term M.t;
  }

let associate_name (assocs : assoc list) (pat_name : string) =
  match Util.find (fun { pattern_name } -> pattern_name = pat_name) assocs with
    | None -> raise
      (TranslationError ("Pattern name " ^ pat_name ^ " not found", None))
    | Some { term_name } -> term_name

(* val fill_in_core
  : denotation_chart
  -> assoc list * DeBruijn.term M.t
  -> core
  -> core translation_result
*)
(** Fill in a core representation of a value with metavars (the raw
  right-hand-side of a denotation chart) with the core terms that go there.
  *)
let rec fill_in_core ({ dynamics; vars; assignments } as args) = function
  | Metavar name ->
    (match M.get assignments name with
    | Some tm -> term_to_core [] tm
    | None
    -> raise (TranslationError ("Metavariable " ^ name ^ " not found", None))
    )
  | Meaning name -> (match M.get assignments name with
    | Some tm -> (match term_denotation dynamics vars tm with
      | Result.Ok tm' -> tm'
      | Error err     -> raise (TranslationError err)
    )
    | None
    -> raise (TranslationError ("Metavariable " ^ name ^ " not found", None))
    )
  | Var _ as v -> v
  | Operator (tag, vals)
  -> Operator (tag, vals |. List.map (fill_in_core_scope args))
  | Primitive _  as v -> v
  | Sequence tms -> Sequence (tms |. List.map (fill_in_core args))
  | Lambda (tys, body) -> Lambda (tys, fill_in_core_scope args body)
  | CoreApp (f, app_args) -> CoreApp
    ( fill_in_core args f
    , app_args |. List.map (fill_in_core args)
    )
  | Case (scrutinee, branches) -> Case
      ( fill_in_core args scrutinee
      , branches |. List.map (fun (pat, scope) ->
         (pat, fill_in_core_scope args scope))
      )

and fill_in_core_scope
  ({ assocs; vars } as args)
  (CoreScope (names, body))
  = let names' = names |. List.map (associate_name assocs)
    in CoreScope (names', fill_in_core {args with vars = names' @ vars} body)

(** Translate a term directly to core, with no interpretation. In other words,
  this term is supposed to directly represent a term in the codomain. *)
and term_to_core env tm = match tm with
  | Operator (tag, subtms) ->
      Operator (tag, subtms |. List.map (scope_to_core env))
  | Var ix -> (match List.get env ix with
    | None -> raise (TranslationError ("failed to look up variable", Some tm))
    | Some name -> Var name
    )
  | Sequence tms -> Sequence (tms |. List.map (term_to_core env))
  | Primitive prim -> Primitive prim

(* XXX change names (assocs)? *)
and scope_to_core env (Scope (names, body)) =
  CoreScope (names, term_to_core env body)

(* val term_denotation
  : denotation_chart -> DeBruijn.term -> core translation_result
*)
(** Match a term in the denotation chart, and return its core term. *)
and term_denotation dynamics vars tm : core translation_result = match tm with
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
    | Some (assocs, assignments, protoCore)
    -> try
        Ok (fill_in_core {dynamics; vars; assocs; assignments} protoCore)
       with
         TranslationError err -> Error err
  )

(* val eval : core -> (core, string) Result.t *)
let eval core =
  let open Belt.Result in

  let rec go ctx tm = match tm with
        | Var v -> (match M.get ctx v with
          | Some result -> Ok result
          | None        -> Error ("Unbound variable " ^ v)
        )
        | CoreApp (Lambda (_tys, CoreScope (argNames, body)), args) ->
            if List.(length argNames != length args)
            then Error "mismatched application lengths"
            else Result.flatMap
              (traverse_list_result (go ctx) args)
              (fun arg_vals ->
                 let new_args = List.(arg_vals
                   |> zip argNames
                   |> toArray
                   |> M.fromArray
                 ) in
                 go (union ctx new_args) body
              )
        | Case (tm, branches) -> Result.flatMap (go ctx tm)
          (fun v -> match find_core_match v branches with
            | None
            -> Error "no match found in case"
            | Some (branch, bindings)
            -> go (union ctx bindings) branch
          )
        | Metavar _v | Meaning _v -> Error "Found a metavar!"

        | Operator _  | Sequence _ | Primitive _
        -> Ok tm

        (* TODO: include the term in error *)
        | _ -> Error "Found a term we can't evaluate"

  in go M.empty core
