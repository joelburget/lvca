open Belt
open Types
open Util

module M = Belt.Map.String
module O = Belt.Option

type scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * scope_pat list
  | DVar       of string option

type ty = | Ty (* TODO *)

type core_pat =
  | PatternTerm of string * core_pat list
  | PatternVar  of string option
  | PatternLit  of primitive
  | PatternDefault

type core_val =
  | ValTm     of string * core_val list
  | ValPrim   of primitive
  | ValLam    of string list * core

and core =
  | CoreVar of string
  | CoreVal of core_val
  | CoreApp of core * core list
  | Case    of core * ty * (core_pat * core) list
  | Meaning of string

type denotation_chart =
  | DenotationChart of (denotation_pat * core) list

let rec val_to_ast (core_val : core_val) : Ast.term
  = match core_val with
  | ValTm (name, vals)
  -> Ast.Term
    ( name
    , List.map vals (fun value -> Ast.Scope([], val_to_ast value))
    )
  | ValPrim prim
  -> Ast.Primitive prim
  | ValLam (args, body)
  -> Ast.Term ("lam", [ Ast.Scope (args, to_ast body) ])

and pat_to_ast (pat : core_pat) : Ast.term
  = failwith "TODO"

and to_ast (core : core) : Ast.term = match core with
  | CoreVar name
  -> Ast.Var name
  (* -> Ast.Term ("CoreVar", [Ast.Scope ([], Ast.Primitive (PrimString name))]) *)
  | CoreVal core_val
  -> Ast.Term ("CoreVal", [Ast.Scope ([], val_to_ast core_val)])
  (* TODO *)

let rec match_branch (v : core_val) (pat : core_pat)
  : core_val M.t option = match (v, pat) with

  | (ValTm (tag1, vals), PatternTerm (tag2, pats)) -> List.(
    let sub_results = zipBy vals pats match_branch in
    if tag1 = tag2 &&
       length vals = length pats && every sub_results O.isSome
    then Some (reduce (map sub_results O.getExn) M.empty union)
    else None
  )
  | (ValPrim l1, PatternLit l2)
  -> if prim_eq l1 l2 then Some M.empty else None
  | (tm, PatternVar (Some v)) -> Some (M.fromArray [|v,tm|])
  | (tm, PatternVar None)     -> Some M.empty
  | (_val, PatternDefault)    -> Some M.empty
  | _ -> None

let rec find_core_match (v : core_val) (pats : (core_pat * core) list)
  : (core * core_val M.t) option = match pats with
    | []                 -> None
    | (pat, rhs) :: pats -> (match match_branch v pat with
      | None          -> find_core_match v pats
      | Some bindings -> Some (rhs, bindings))

let rec matches (tm : Abt.term) (pat : denotation_pat)
  : ((string * string) list * Abt.term M.t) option
  = match (tm, pat) with
    | (Term(tag1, subtms), DPatternTm(tag2, subpats))
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

and matches_scope
  (Scope (binders, tm) : Abt.scope)
  (DenotationScopePat (patBinders, pat) : scope_pat)
  : ((string * string) list * Abt.term M.t) option
  = if List.(length patBinders == length binders)
    then O.map
      (matches tm pat)
      (fun (assocs, tmMatches)
        -> (List.zip patBinders binders @ assocs, tmMatches))
    else None

let find_match
  (DenotationChart denotations : denotation_chart)
  (term : Abt.term)
  : ((string * string) list * Abt.term M.t * core) option
  = get_first
      (fun (pat, core) -> O.map
        (matches term pat)
        (fun (assocs, bindings) -> (assocs, bindings, core)))
      denotations

type located_err = (string * Abt.term option)
type 'a translation_result = ('a, located_err) Result.t

let rec fill_in_core
  (dynamics : denotation_chart)
  ((assocs, assignments) as mr : (string * string) list * Abt.term M.t)
  (c : core)
  : core translation_result
  = match c with
    | Meaning name -> (match M.get assignments name with
      | Some tm -> term_to_core dynamics tm
      | None    -> Error ("TODO 3", None)
      )
    (* XXX same as Meaning *)
    | CoreVar name -> (match M.get assignments name with
      | Some tm -> Result.map (term_is_core_val [] tm) (fun cv -> CoreVal cv)
      | None    -> Ok c
      )
    | CoreVal v -> Result.map
      (fill_in_val dynamics mr v)
      (fun v' -> CoreVal v')
    | CoreApp(f, args) -> (match
      ( fill_in_core dynamics mr f
      , sequence_list_result (List.map args (fill_in_core dynamics mr))
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

and fill_in_val
    (dynamics : denotation_chart)
    (mr : (string * string) list * Abt.term M.t)
    (v : core_val)
  : core_val translation_result
  = match v with
    | ValTm (tag, vals) -> Result.map
      (traverse_list_result (fill_in_val dynamics mr) vals)
      (fun vals' -> ValTm (tag, vals'))
    | ValPrim _   -> Ok v
    | ValLam (binders, core) -> Result.map
      (fill_in_core dynamics mr core)
      (fun core' -> ValLam (binders, core'))

and term_is_core_val (env : string list) (tm : Abt.term)
  : core_val translation_result
  = match tm with
  | Term ("lam", [Scope (names, body)])
  -> let env' = List.concat names env
     in Result.map
       (term_is_core env' body)
       (fun body' -> ValLam (names, body'))
  | Term (tag, subtms) -> Result.map
    (traverse_list_result (scope_is_core_val env) subtms)
    (fun subtms' -> ValTm (tag, subtms'))
  | Abt.Primitive prim -> Ok (ValPrim prim)
  | Abt.Var _          -> Error ("TODO 4", Some tm)
  | _                  -> Error ("TODO 5", Some tm)

and scope_is_core_val
  (env : string list)
  (Scope (names, body) : Abt.scope)
  : core_val translation_result
  = match names with
  | [] -> term_is_core_val env body
  | _  -> Error ("Unexpected binding TODO", None)

and term_is_core (env : string list) (tm : Abt.term) : core translation_result
  = match tm with
  | Abt.Var ix -> (match List.get env ix with
    | None -> Error ("failed to look up variable", Some tm)
    | Some name -> Ok (CoreVar name)
    )

and term_to_core (dynamics : denotation_chart) (tm : Abt.term)
  : core translation_result
  = match find_match dynamics tm with
    | None
    -> Error ("no match found", Some tm)
    | Some (assocs, bindings, protoCore)
    -> fill_in_core dynamics (assocs, bindings) protoCore

let eval (core : core) : (core_val, string) Result.t =

  let open Belt.Result in

  let rec go ctx = function
        | CoreVar v -> (match M.get ctx v with
          | Some result -> Ok result
          | None        -> Error ("Unbound variable " ^ v)
        )
        | CoreVal v -> Ok v
        | CoreApp (CoreVal (ValLam (argNames, body)), args) ->
            if List.(length argNames != length args)
            then Error "mismatched application lengths"
            else Result.flatMap
              (sequence_list_result (List.map args (go ctx)))
              (fun arg_vals ->
                 let new_args = M.fromArray
                   (List.(toArray (zip argNames arg_vals))) in
                 go (union ctx new_args) body)
        | Case (tm, _ty, branches) -> Result.flatMap (go ctx tm)
          (fun v -> (match find_core_match v branches with
            | None                    -> Error "no match found in case"
            | Some (branch, bindings) -> go (union ctx bindings) branch)
          )
        | Meaning _v -> Error "Found a metavar!"

        | _ -> Error "TODO 7"

  in go M.empty core
