open Belt

type valence =
  | FixedValence    of string list * string
  | VariableValence of string * string
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
    | Scope of string list * t

  and t =
    | Term      of string * scope list
    | Var       of string
    | Sequence  of t list (* TODO: list vs array? *)
    | Primitive of primitive
    ;;
end

module Ast = struct

  type t =
    | Term      of string * t list
    | Var       of string
    | Sequence  of t list (* TODO: list vs array? *)
    | Primitive of primitive
    ;;

end

type var =
  | Var of string
;;

type literal =
  | LitText of string
  | LitInteger of Bigint.t
;;

module Core = struct

type ty = | Ty;; (* TODO *)

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

end

type denotation_scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * denotation_scope_pat list
  | DVar       of string option
;;

type denotation_chart =
  | DenotationChart of (denotation_pat * Core.core) list
;;

(* val eval_core : core -> (core_val, string) result;; *)
let eval_core (core : Core.core) : (Core.core_val, string) Result.t =

  let open Belt.Result in
  let open Core in

  let rec go ctx core = match core with
        | CoreVar (Var v) -> (match Belt.Map.String.get ctx v with
          | Some result -> Ok result
          | None        -> Error ("Unbound variable " ^ v))
        | CoreVal v -> Ok v
        | CoreApp (Lam (argNames, body), args) ->
            if List.length argNames != List.length args
            then Error "mismatched application lengths"
            else let args' = [] (* List.map (go ctx) args *) in
                 let ctx' = Belt.Map.String.merge
                       ctx
                       (Belt.Map.String.fromArray (List.toArray
                         (List.zip argNames args')))
                       (fun _k v1 v2 -> match (v1, v2) with
                         | (_,      Some v) -> Some v
                         | (Some v, None  ) -> Some v
                         | (None,   None  ) -> None)
                 in go ctx' body
        (* | Case tm _ty branches ->
          let v = go ctx tm *)
        | Metavar _v -> Error "Found a metavar!"

        | _ -> Error "TODO"

  in go Belt.Map.String.empty core

let rec intersperse_after list el =
  match list with
  | []           -> []
  | [ list_el ]  -> [ list_el; el ]
  | x :: y :: tl -> x :: el :: intersperse_after (y::tl) el
