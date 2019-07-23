open Types
open Binding

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

and core =
  | Operator  of string * core_scope list
  | Var       of string
  | Sequence  of core list
  | Primitive of primitive
  | Lambda    of core_scope
  | CoreApp   of core * core list
  | Case      of core * sort * (core_pat * core_scope) list
  | Metavar   of string
  | Meaning   of string

and core_scope = CoreScope of string list * core

type denotation_chart =
  | DenotationChart of (denotation_pat * core) list

type located_err = (string * DeBruijn.term option)
type 'a translation_result = ('a, located_err) Result.t

val to_ast : core -> Nominal.term
val eval   : core -> (core, string) Result.t
val term_denotation
  : denotation_chart -> string list -> DeBruijn.term -> core translation_result
