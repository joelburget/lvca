(** Tools for dealing with the core language in LVCA.

    - [denotation_chart] is the data type for declaring a mapping from some language to
      [term]
    - [term] and [core_scope] (and [Pattern.t]) define the core language
    - [term_denotation] is used to map some language to [term]
    - [eval] is then used to evaluate the core term
    - finally, [to_ast] is used to give the resulting term *)

open AbstractSyntax
open Binding

(* Questions:
  - give declarations types?
 *)

type is_rec = Rec | NoRec

type term =
  | Term of Nominal.term
  | CoreApp of term * term
  | Case of term * core_case_scope list
  (** Cases match patterns *)
  | Lambda of sort * core_scope
  (** Lambdas bind variables. Patterns not allowed. *)
  | Let of is_rec * term * core_scope
  (** Lets bind variables. Patterns not allowed. *)

and core_scope = Scope of string * term

and core_case_scope = CaseScope of Pattern.t * term

val pp_core : Format.formatter -> term -> unit
val pp_core_str : term -> string

type denotation_chart = DenotationChart of (string * term) list

val pp_chart : Format.formatter -> denotation_chart -> unit
val pp_chart_str : denotation_chart -> string

type eval_error = string * term

val eval : term -> (Nominal.term, eval_error) Core_kernel.Result.t

(** Convert a core term to a nominal term, ensuring that it contains no core operators. *)
val to_ast : term -> Nominal.term

(** Convert a denotation chart to a nominal term, for storage. *)
val to_term : denotation_chart -> Nominal.term
