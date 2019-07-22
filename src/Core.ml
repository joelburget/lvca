open Belt
open Types
open Binding
let (fold_right, get_first, traverse_list_result) =
  Util.(fold_right, get_first, traverse_list_result)

module M = Belt.Map.String
module O = Belt.Option

type denotation_scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * denotation_scope_pat list
  | DVar       of string option

type core_pat =
  | PatternTerm     of string * core_pat list
  | PatternVar      of string option
  | PatternSequence of core_pat list
  | PatternPrim     of primitive
  | PatternDefault

type core =
  (* first four constructors correspond to regular term constructors *)
  | Operator  of string * core list
  | Var       of string
  | Sequence  of core list
  | Primitive of primitive

  (* plus, core-specific ctors *)
  | Lambda   of string list * core
  | CoreApp  of core * core list
  | Case     of core * sort * (core_pat * core) list
  | Metavar  of string
  | Meaning  of string

type denotation_chart =
  | DenotationChart of (denotation_pat * core) list

type located_err = string * DeBruijn.term option
type 'a translation_result = ('a, located_err) Result.t

(** Raised by to_ast when the presence of metavars, lambdas, and cases make
   the value invalid
 *)
exception AstConversionErr of core

let rec to_ast (core : core) : Nominal.term = match core with
  | Var name -> Var name
  | Operator (name, vals)
  -> Operator
    ( name
    , List.map vals (fun value -> Nominal.Scope([], to_ast value))
    )
  | Primitive prim
  -> Primitive prim
  | Sequence tms
  -> Sequence (List.map tms to_ast)
  | Lambda _ as v -> raise @@ AstConversionErr v
  | CoreApp _
  | Case _
  | Metavar _
  | Meaning _ -> raise @@ AstConversionErr core

(* val match_branch : core -> core_pat -> core M.t option *)
let rec match_branch v pat = match (v, pat) with

  | (Operator (tag1, vals), PatternTerm (tag2, pats)) -> List.(
    let sub_results = zipBy vals pats match_branch in
    if tag1 = tag2 &&
       length vals = length pats && every sub_results O.isSome
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

(* val find_core_match : core -> (core_pat * core) list *)
let rec find_core_match v = function
  | []                 -> None
  | (pat, rhs) :: pats -> (match match_branch v pat with
    | None          -> find_core_match v pats
    | Some bindings -> Some (rhs, bindings))

(* val matches
  : DeBruijn.term
  -> denotation_pat
  -> ((string * string) list * DeBruijn.term M.t) option
*)
let rec matches tm pat = match (tm, pat) with
  | (DeBruijn.Operator(tag1, subtms), DPatternTm(tag2, subpats))
  -> if tag1 == tag2 && List.(length subtms == length subpats)
     then fold_right
       (fun ((scope, subpat), b_opt) ->
         match (matches_scope scope subpat, b_opt) with
         | (Some (assocs, bindings), Some (assocs', bindings'))
         -> Some (assocs @ assocs', union bindings bindings')
         | _ -> None)
       (List.zip subtms subpats)
       (Some ([], M.empty))
     else None
  | (_, DPatternTm _)   -> None
  | (_, DVar None)      -> Some ([], M.empty)
  | (tm, DVar (Some v)) -> Some ([], M.fromArray [|v,tm|])

(* val matches_scope
  : DeBruijn.scope
  -> denotation_scope_pat
  -> ((string * string) list * DeBruijn.term M.t) option
*)
and matches_scope (Scope (binders, tm)) (DenotationScopePat (patBinders, pat))
  = if List.(length patBinders == length binders)
    then O.map
      (matches tm pat)
      (fun (assocs, tmMatches)
        -> (List.zip patBinders binders @ assocs, tmMatches))
    else None

(* val find_match
  : denotation_chart
  -> DeBruijn.term
  -> ((string * string) list * DeBruijn.term M.t * core) option
*)
let find_match (DenotationChart denotations) term = get_first
  (fun (pat, core) -> O.map
    (matches term pat)
    (fun (assocs, bindings) -> (assocs, bindings, core)))
  denotations

(* val fill_in_core
  : denotation_chart
  -> (string * string) list * DeBruijn.term M.t
  -> core
  -> core translation_result
*)
(** Fill in a core representation of a value with metavars (the raw
  right-hand-side of a denotation chart) with the core terms that go there.
  *)
let rec fill_in_core dynamics ((assocs, assignments) as mr) = function
  | Metavar name -> (match M.get assignments name with
    | Some tm -> term_to_core dynamics tm
    | None    -> Result.Error ("Metavariable " ^ name ^ " not found", None)
    )
  (* XXX same as Metavar? *)
  | Meaning name -> (match M.get assignments name with
    | Some tm -> term_to_core dynamics tm
    | None    -> Result.Error ("Metavariable " ^ name ^ " not found", None)
    )
  | Var _ as c -> Ok c
  | Operator (tag, vals) -> Result.map
    (traverse_list_result (fill_in_core dynamics mr) vals)
    (fun vals' -> Operator (tag, vals'))
  | Primitive _  as v -> Ok v
  | Sequence tms -> Result.map
    (traverse_list_result (fill_in_core dynamics mr) tms)
    (fun tms' -> Sequence tms')
  | Lambda (binders, core) -> Result.map
    (fill_in_core dynamics mr core)
    (fun core' -> Lambda (binders, core'))
  | CoreApp (f, args) -> (match
    ( fill_in_core dynamics mr f
    , mr
      |> fill_in_core dynamics
      |> List.map args
      |> sequence_list_result
    ) with
    | (Ok f', Ok args')               -> Ok (CoreApp (f', args'))
    | (Error msg, _) | (_, Error msg) -> Error msg
    )
  (* | Lam (binders, core) -> Result.map
    (fill_in_core dynamics mr core)
    (fun core' -> Lam (binders, core')) *)
  | Case (scrutinee, ty, branches) ->
      let mBranches : ((core_pat * core) list) translation_result
            = sequence_list_result (List.map branches
            (fun (pat, core) -> Result.map
              (fill_in_core dynamics mr core)
              (fun core' -> (pat, core'))
            )) in
      match (fill_in_core dynamics mr scrutinee, mBranches) with
        | (Ok scrutinee', Ok branches')
        -> Ok (Case (scrutinee', ty, branches'))
        | (Error msg, _) | (_, Error msg)
        -> Error msg

(* val term_is_core_val : string list -> DeBruijn.term -> core translation_result *)
and term_is_core_val env = function
  | DeBruijn.Operator ("lam", [Scope (names, body)])
  -> let env' = List.concat names env in
     Result.map
       (term_is_core env' body)
       (fun body' -> Lambda (names, body'))
  | Operator (tag, subtms) -> Result.map
    (traverse_list_result (scope_is_core_val env) subtms)
    (fun subtms' -> Operator (tag, subtms'))
  | Primitive prim -> Ok (Primitive prim)
  | Sequence tms -> Result.map
    (traverse_list_result (term_is_core env) tms)
    (fun tms' -> Sequence tms')
  | Var _ as tm -> Error ("A variable cannot be a value", Some tm)
  | tm          -> Error ("This term cannot be a value", Some tm)

(* val scope_is_core_val : string list -> DeBruijn.scope -> core translation_result *)
and scope_is_core_val env (Scope (names, body)) = match names with
  | [] -> term_is_core_val env body
  | _  -> Error ("Unexpected binding TODO", None)

(* val term_is_core : string list -> DeBruijn.term -> core translation_result *)
and term_is_core env tm = match tm with
  | DeBruijn.Var ix -> (match List.get env ix with
    | None -> Error ("failed to look up variable", Some tm)
    | Some name -> Ok (Var name)
    )
  (*
    | Operator  of string * scope list
    | Var       of int
    | Sequence  of term list
    | Primitive of primitive
    *)
  (* TODO *)

(* val term_to_core : denotation_chart -> DeBruijn.term -> core translation_result *)
(** Match a term in the denotation chart, and return its core term. *)
and term_to_core dynamics tm = match find_match dynamics tm with
  | None
  -> Error ("no match found", Some tm)
  | Some (assocs, bindings, protoCore)
  -> fill_in_core dynamics (assocs, bindings) protoCore

(* val eval : core -> (core, string) Result.t *)
let eval core =
  let open Belt.Result in

  let rec go ctx = function
        | Var v -> (match M.get ctx v with
          | Some result -> Ok result
          | None        -> Error ("Unbound variable " ^ v)
        )
        | CoreApp (Lambda (argNames, body), args) ->
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
            | None                    -> Error "no match found in case"
            | Some (branch, bindings) -> go (union ctx bindings) branch)
          )
        | Metavar _v | Meaning _v -> Error "Found a metavar!"

        | _ -> Error "TODO 7"

  in go M.empty core
