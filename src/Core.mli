(** Tools for dealing with the core language in LVCA.
 - denotation_chart is the data type for declaring a mapping from some language to core
 - core, core_scope, core_pat, core_binding_pat, denotation_pat, and denotation_scope_pat define the core language
 - term_denotation is used to map some language to core
 - eval is then used to evaluate the core term
 - finally, to_ast is used to give the resulting term
 *)

open Types
open Binding

type denotation_scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * denotation_scope_pat list
  | DVar       of string

type core_pat =
  | PatternTerm     of string * core_binding_pat list
  | PatternVar      of string
  | PatternSequence of core_pat list
  | PatternPrim     of primitive

and core_binding_pat = CoreBindingPat of string list * core_pat

and core =
  | Operator  of string * core_scope list
  | Var       of string
  | Sequence  of core list
  | Primitive of primitive
  | Lambda    of sort list * core_scope
  | CoreApp   of core * core list
  | Case      of core * (core_pat * core_scope) list
  | Metavar   of string
  | Meaning   of string

and core_scope = CoreScope of string list * core

type denotation_chart =
  | DenotationChart of (denotation_pat * core) list

type located_err = (string * DeBruijn.term option)
type 'a translation_result = ('a, located_err) Result.t

val term_denotation
  : denotation_chart -> string list -> DeBruijn.term -> core translation_result

val eval   : core -> (core, string) Result.t

val to_ast : core -> Nominal.term
