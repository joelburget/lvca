(** Statics describe the rules for stating whether an expression is well-formed. This
    implementation is for expressing bidirectional typing rules. *)

open Lvca_provenance
open Lvca_syntax
open Lvca_util

(** Both typing and inference rules share this shape.

    There is an important distinction in terms of the typechecking algorithm, though.
    Inference rules assert that some type can be synthesized from the given term. Checking
    rules assert that given both a term and a type we can check if the term is of that
    type. *)
module Typing_rule : sig
  type 'info t =
    { tm : 'info Binding_aware_pattern.t
    ; ty : 'info Binding_aware_pattern.t
    }

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val erase : _ t -> unit t
end

module Typing_clause : sig
  type 'info inference_rule = 'info Typing_rule.t
  type 'info checking_rule = 'info Typing_rule.t

  type 'info t =
    | Inference_rule of 'info inference_rule
    | Checking_rule of 'info checking_rule

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val erase : _ t -> unit t
  val parse : Opt_range.t t Lvca_parsing.t
end

(** A hypothesis contains a set of variables (and their types) that must appear in the
    context, as well as an inference or checking clause. *)
module Hypothesis : sig
  type 'info t = 'info Binding_aware_pattern.t String.Map.t * 'info Typing_clause.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val erase : _ t -> unit t

  module Parse : sig
    val pattern : Opt_range.t Binding_aware_pattern.t Lvca_parsing.t
    val typed_term : (string * Opt_range.t Binding_aware_pattern.t) Lvca_parsing.t
    val context : Opt_range.t Binding_aware_pattern.t String.Map.t Lvca_parsing.t
    val t : Opt_range.t t Lvca_parsing.t
  end
end

(** A rule contains a set of hypotheses, an optional name, and a conclusion *)
module Rule : sig
  type 'info t =
    { hypotheses : 'info Hypothesis.t list
    ; name : string option
    ; conclusion : 'info Hypothesis.t
    }

  val erase : _ t -> unit t

  module Parse : sig
    val line : string option Lvca_parsing.t
    val t : Opt_range.t t Lvca_parsing.t
  end
end

module Typing : sig
  type 'info t = Typing of 'info Nominal.Term.t * 'info Nominal.Term.t

  val erase : _ t -> unit t
end

type 'info t = 'info Rule.t list

val erase : _ t -> unit t
val parse : Opt_range.t t Lvca_parsing.t
