(** Tools for dealing with the core language in LVCA.
 - denotation_chart is the data type for declaring a mapping from some language to core
 - core, core_scope, denotation_pat, and denotation_scope_pat define the core language
 - term_denotation is used to map some language to core
 - eval is then used to evaluate the core term
 - finally, to_ast is used to give the resulting term
 *)

open Types
open Binding

(** Represents the LHS of a denotation rule. Why is this not just `Pattern.t`?
 Because patterns can't match binders. For example, we want to be able to write
 this on the LHS of a denotation rule:

     [[ lam(x. x) ]] = ...

 This is not allowed by regular patterns.
 *)
type denotation_pat =
  | Operator  of string * denotation_pat_scope list
  | Sequence  of denotation_pat list
  | Primitive of primitive
  | Var       of string

(** A scope within the LHS of a denotation rule. Note that it's not currently
  allowed to match on specific patterns -- you can only match on an entire
  slot at once.
*)
and denotation_pat_scope = Scope of string list * denotation_pat

(** Represents the RHS of a denotation rule *)
type denotation_term =
  (* first four constructors correspond to regular term constructors *)
  | Operator  of string * denotation_scope list
  | Var       of string
  | Sequence  of denotation_term list
  | Primitive of primitive

  (* Also, oxford bracketed var *)
  | Meaning of string

and denotation_scope =
  Scope of denotation_scope_pat list * denotation_term

and denotation_scope_pat =
  | PatOperator of string * denotation_scope_pat list
  | PatVar      of string
  | Sequence    of denotation_scope_pat list
  | Primitive   of primitive

and core =
  | Operator  of string * core_scope list
  | Var       of string
  | Sequence  of core list
  | Primitive of primitive
  | Lambda    of sort list * core_scope
  | CoreApp   of core * core list
  | Case      of core * core_scope list
  | Metavar   of string
  | Meaning   of string

and core_scope = CoreScope of Pattern.t list * core

type pre_denotation_chart =
  | DenotationChart of (denotation_pat * denotation_term) list

type denotation_chart =
  | DenotationChart of (denotation_pat * core) list

val produce_denotation_chart : pre_denotation_chart -> denotation_chart

type located_err = (string * DeBruijn.term option)
type 'a translation_result = ('a, located_err) Result.t

val term_denotation
  : denotation_chart -> string list -> DeBruijn.term -> core translation_result

val eval   : core -> (core, string) Result.t

(** Convert a core term to a nominal term, ensuring that it contains no core
 operators (note this is not the inverse of from_ast) *)
val to_ast : core -> Nominal.term

(** Convert a nominal term (with core operators) to a core term (note this is
 not the inverse of to_ast) *)
val from_ast : Nominal.term -> core
