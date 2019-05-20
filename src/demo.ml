open Belt

type test = | Test;;

type primitive =
  | PrimInteger of Bigint.t
  | PrimString  of string
  | PrimBool    of bool
  ;;

type scope =
  | Scope of string list * term

and term =
  | Term      of string * scope list
  | Var       of string
  | Sequence  of term list (* TODO: list vs array? *)
  | Primitive of primitive
  ;;

type var =
  | Var of string
;;

type literal =
  | LitText of string
  | LitInteger of Bigint.t
;;

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

type denotation_scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * denotation_scope_pat list
  | DVar       of string option
;;

type denotation_chart =
  | DenotationChart of (denotation_pat * core) list
;;

(* val eval_core : core -> (core_val, string) result;; *)
let eval_core (core : core) : (core_val, string) Result.t = match core with
  | CoreVar v -> Error "TODO";;

let rec intersperse_after list el =
  match list with
  | []           -> []
  | [ list_el ]  -> [ list_el; el ]
  | x :: y :: tl -> x :: el :: intersperse_after (y::tl) el
