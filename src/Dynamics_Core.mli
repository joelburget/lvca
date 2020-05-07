(** Tools for dealing with the core language in LVCA.

    - [denotation_chart] is the data type for declaring a mapping from some language to
      [core]
    - [core] and [core_scope] (and [Pattern.t]) define the core language
    - [term_denotation] is used to map some language to [core]
    - [eval] is then used to evaluate the core term
    - finally, [to_ast] is used to give the resulting term *)

open AbstractSyntax
open Binding

(* Questions:
  - give declarations types?
 *)

type is_rec = Rec | NoRec

type core =
  | Operator of string * core_scope list
  | Var of string
  | Sequence of core list
  | Primitive of Primitive.t
  | Lambda of sort list * core_scope
  | CoreApp of core * core list
  | Case of core * core_case_scope list
  | Let of is_rec * core * core_scope

and core_scope = Scope of Pattern.t list * core

and core_case_scope = CaseScope of Pattern.t * core

val pp_core : Format.formatter -> core -> unit
val pp_core_str : core -> string

type denotation_chart = DenotationChart of (string * core) list

type eval_error = string * core

val eval : core -> (core, eval_error) Core_kernel.Result.t

(** Convert a core term to a nominal term, ensuring that it contains no core operators. *)
val to_ast : core -> Nominal.term

(** Convert a denotation chart to a nominal term, for storage. *)
val to_term : denotation_chart -> Nominal.term
