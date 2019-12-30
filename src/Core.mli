(** Tools for dealing with the core language in LVCA.
    - denotation_chart is the data type for declaring a mapping from some language to core
    - core and core_scope (and BindingAwarePattern.t) define the core language
    - term_denotation is used to map some language to core
    - eval is then used to evaluate the core term
    - finally, to_ast is used to give the resulting term
*)

open Types
open Binding


(** Represents the RHS of a denotation rule *)
type denotation_term =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * denotation_scope list
  | Var of string
  | Sequence of denotation_term list
  | Primitive of primitive
  (* Also, oxford bracketed var *)
  | Meaning of string

and denotation_scope = Scope of Pattern.t list * denotation_term

and core =
  | Operator of string * core_scope list
  | Var of string
  | Sequence of core list
  | Primitive of primitive
  | Lambda of sort list * core_scope
  | CoreApp of core * core list
  | Case of core * core_scope list
  | Let of core * core_scope
  | Metavar of string
  | Meaning of string

and core_scope = Scope of Pattern.t list * core

type pre_denotation_chart =
  DenotationChart of (BindingAwarePattern.t * denotation_term) list
type denotation_chart =
  DenotationChart of (BindingAwarePattern.t * core) list

val produce_denotation_chart : pre_denotation_chart -> denotation_chart

type located_err = string * DeBruijn.term option
type 'a translation_result = ('a, located_err) Result.t

val eval : core -> (core, string) Result.t

(** Convert a core term to a nominal term, ensuring that it contains no core
    operators. *)
val to_ast : core -> Nominal.term
