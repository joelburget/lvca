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

module Abt = struct

  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of string
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive
    ;;
end

module Ast = struct

  type term =
    | Term      of string * term list
    | Var       of string
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive
    ;;

end

type literal =
  | LitText of string
  | LitInteger of Bigint.t
;;

module Core = struct

  type ty = | Ty;; (* TODO *)

  type var =
    | Var of string
  ;;

  type core_pat =
    | PatternTerm of string * core_pat list
    | PatternVar  of string option
    | PatternLit  of literal
    | PatternDefault
  ;;

  type core_val =
    | ValTm  of string * core_val list
    | ValLit of literal
    | ValPrimop of string
    | ValLam of var list * core

  and core =
    | CoreVar of var
    | CoreVal of core_val
    | CoreApp of core * core list
    | Lam     of string list * core
    | Case    of core * ty * (core_pat * core) list
    | Metavar of string
  ;;

  module M = Belt.Map.String

  let matchBranch (v : core_val) (pat : core_pat) (core : core)
    : (core_val M.t * core) option = match (v, pat) with

    | (ValTm (tag1, vals), PatternTerm (tag2, pats)) ->
        if tag1 == tag2
        then failwith "TODO"
        else failwith "TODO"


  (* val eval : core -> (core_val, string) result;; *)
  let eval (core : core) : (core_val, string) Result.t =

    let open Belt.Result in

    let rec go ctx core = match core with
          | CoreVar (Var v) -> (match M.get ctx v with
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

          | _ -> Error "TODO"

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

let rec intersperse list el =
  match list with
  | [] | [_]     -> list
  | x :: y :: tl -> x :: el :: intersperse (y::tl) el

let rec intersperse_after list el =
  match list with
  | []           -> []
  | [ list_el ]  -> [ list_el; el ]
  | x :: y :: tl -> x :: el :: intersperse_after (y::tl) el
