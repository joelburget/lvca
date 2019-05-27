open Belt

type sort =
  | SortAp   of sort * sort
  | SortName of string
;;

type valence =
  | FixedValence    of sort list * sort
  | VariableValence of string * sort
;;

type arity =
  | Arity of string list * valence list
;;

type operatorDef =
  | OperatorDef of string * arity
;;

type sortDef =
  | SortDef of string list * operatorDef list
;;

type language =
  | Language of sortDef Belt.Map.String.t
;;

type primitive =
  | PrimInteger of Bigint.t
  | PrimString  of string
  | PrimBool    of bool
  ;;

let prim_eq p1 p2 = match (p1, p2) with
  | (PrimInteger i1, PrimInteger i2) -> Bigint.(i1 = i2)
  | (PrimString  s1, PrimString  s2) -> s1 = s2
  | (PrimBool    b1, PrimBool    b2) -> b1 = b2
  | _                                -> false

module Abt = struct

  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of string
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive

end

module Ast = struct

  type term =
    | Term      of string * term list
    | Var       of string
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive
    ;;

end

module Core = struct

  type ty = | Ty;; (* TODO *)

  type core_pat =
    | PatternTerm of string * core_pat list
    | PatternVar  of string option
    | PatternLit  of primitive
    | PatternDefault
  ;;

  type core_val =
    | ValTm  of string * core_val list
    | ValLit of primitive
    | ValPrimop of string
    | ValLam of string list * core

  and core =
    | CoreVar of string
    | CoreVal of core_val
    | CoreApp of core * core list
    | Lam     of string list * core
    | Case    of core * ty * (core_pat * core) list
    | Metavar of string
  ;;

  module M = Belt.Map.String
  module O = Belt.Option

  let rec matchBranch (v : core_val) (pat : core_pat)
    : core_val M.t option = match (v, pat) with

    | (ValTm (tag1, vals), PatternTerm (tag2, pats)) ->
        let subResults = List.map
          (List.zip vals pats)
          (fun (v', pat') -> matchBranch v' pat') in
        if tag1 = tag2 &&
           List.length vals = List.length pats &&
           List.every subResults O.isSome
        then Some (List.reduce subResults M.empty
          (fun m m' -> M.merge m (O.getExn m')
             (fun _k v1 v2 -> match (v1, v2) with
               | (_,      Some v) -> Some v (* TODO: except if both Some *)
               | (Some v, None  ) -> Some v
               | (None,   None  ) -> None)
          ))
        else None
    | (ValLit l1, PatternLit l2) ->
        if prim_eq l1 l2
        then Some M.empty
        else None
    | (tm, PatternVar (Some v)) -> Some (M.fromArray [|v,tm|])
    | (tm, PatternVar None)     -> Some M.empty
    | (_val, PatternDefault)    -> Some M.empty
    | _ -> None

  let rec traverse_list_result (lst : (('a, 'b) Result.t) list)
    : ('a list, 'b) Result.t = match lst with
    | []             -> Ok []
    | Ok a :: rest   -> (match traverse_list_result rest with
      | Ok rest'  -> Ok (a :: rest')
      | Error msg -> Error msg
    )
    | Error msg :: _ -> Error msg

  let rec from_term (term : Abt.term) : (core, string) Result.t = match term with
    | Abt.Var name -> Ok (CoreVar name)
    | Abt.Sequence _tms ->  Error "TODO: conversion of sequences"
    | Abt.Primitive prim -> Ok (CoreVal (ValLit prim))
    | Abt.Term (name, children) -> match name with
      | "app"  -> (match children with
        | f :: args -> (match
          (from_scope f, traverse_list_result (List.map args from_scope)) with
          | (Ok f', Ok args') -> Ok (CoreApp(f', args'))
          | (Error msg, _)
          | (_, Error msg) -> Error msg
        )
        | []        -> Error "App must have a function"
      )
      | "lam"  -> Error "TODO 1"
      | "case" -> Error "TODO 2"
      | _      -> (match traverse_list_result (List.map children from_scope) with
        | Ok children' -> Ok (CoreApp(CoreVar name, children'))
        | Error msg    -> Error msg
      )

    and from_scope (scope : Abt.scope) : (core, string) Result.t = failwith "TODO"

  (* val eval : core -> (core_val, string) result;; *)
  let eval (core : core) : (core_val, string) Result.t =

    let open Belt.Result in

    let rec go ctx core = match core with
          | CoreVar v -> (match M.get ctx v with
            | Some result -> Ok result
            | None        -> Error ("Unbound variable " ^ v))
          | CoreVal v -> Ok v
          | CoreApp (Lam (argNames, body), args) ->
              if List.length argNames != List.length args
              then Error "mismatched application lengths"
              else let args' = [] (* List.map (go ctx) args *) in
                   let ctx' = M.merge
                         ctx
                         (M.fromArray (List.toArray (List.zip argNames args')))
                         (fun _k v1 v2 -> match (v1, v2) with
                           | (_,      Some v) -> Some v
                           | (Some v, None  ) -> Some v
                           | (None,   None  ) -> None)
                   in go ctx' body
          (* | Case tm _ty branches ->
            let v = go ctx tm *)
          | Metavar _v -> Error "Found a metavar!"

          | _ -> Error "TODO 3"

    in go M.empty core

end

module Denotation = struct

  type scope_pat =
    | DenotationScopePat of string list * pat

  and pat =
    | DPatternTm of string * scope_pat list
    | DVar       of string option
  ;;

  type chart =
    | DenotationChart of (pat * Core.core) list
  ;;

end

module Statics = struct

  module M = Belt.Map.String

  type scope = Scope of string list * term

  and term =
          | Term      of string * scope list
          | Bound     of int
          | Free      of string
          | Sequence  of term list
          | Primitive of primitive

  type inferenceRule = InferenceRule of term * term
  type checkingRule  = CheckingRule  of term * term

  type typingClause =
    | InferenceRule of inferenceRule
    | CheckingRule  of checkingRule

  type hypothesis = term M.t * typingClause

  (* TODO: the conclusion type differs from LVCA *)
  type rule = Rule of hypothesis list * string option * hypothesis

end

let rec intersperse list el =
  match list with
  | [] | [_]     -> list
  | x :: y :: tl -> x :: el :: intersperse (y::tl) el

let rec intersperse_after list el =
  match list with
  | []           -> []
  | [ list_el ]  -> [ list_el; el ]
  | x :: y :: tl -> x :: el :: intersperse_after (y::tl) el
