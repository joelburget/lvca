open Belt
open Types
open Binding

type scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * scope_pat list
  | DVar       of string option

type ty = CoreTy of sort

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

type located_err = (string * DeBruijn.term option)
type 'a translation_result = ('a, located_err) Result.t

val val_to_ast   : core_val -> Nominal.term
val eval         : core -> (core_val, string) Result.t
val term_to_core : denotation_chart -> Binding.DeBruijn.term -> core translation_result
