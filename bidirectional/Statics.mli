(** Statics describe the rules for stating whether an expression is well-formed. This
    implementation is for expressing bidirectional typing rules. *)

open Lvca_syntax
open Lvca_util

(** Both typing and inference rules share this shape.

    There is an important distinction in terms of the typechecking algorithm, though.
    Inference rules assert that some type can be synthesized from the given term. Checking
    rules assert that given both a term and a type we can check if the term is of that
    type. *)
module Typing_rule : sig
  type t =
    { tm : Binding_aware_pattern.t
    ; ty : Binding_aware_pattern.t
    }

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
end

module Typing_clause : sig
  type inference_rule = Typing_rule.t
  type checking_rule = Typing_rule.t

  type t =
    | Inference_rule of inference_rule
    | Checking_rule of checking_rule

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val parse : t Lvca_parsing.Parser.t
end

(** A hypothesis contains a set of variables (and their types) that must appear in the
    context, as well as an inference or checking clause. *)
module Hypothesis : sig
  type t = Binding_aware_pattern.t String.Map.t * Typing_clause.t

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool

  module Parse : sig
    val typed_term : (string * Binding_aware_pattern.t) Lvca_parsing.Parser.t
    val context : Binding_aware_pattern.t String.Map.t Lvca_parsing.Parser.t
    val t : t Lvca_parsing.Parser.t
  end
end

(** A rule contains a set of hypotheses, an optional name, and a conclusion *)
module Rule : sig
  type t =
    { hypotheses : Hypothesis.t list
    ; name : string option
    ; conclusion : Hypothesis.t
    }

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool

  module Parse : sig
    val line : string option Lvca_parsing.Parser.t
    val t : t Lvca_parsing.Parser.t
  end
end

module Typing : sig
  type t = Typing of Nominal.Term.t * Nominal.Term.t

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
end

type t = Rule.t list

val parse : t Lvca_parsing.Parser.t
