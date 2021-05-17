(** Statics describe the rules for stating whether an expression is well-formed. This
    implementation is for expressing bidirectional typing rules. *)

open Lvca_provenance
open Lvca_syntax

(** Both typing and inference rules share this shape.

    There is an important distinction in terms of the typechecking algorithm, though.
    Inference rules assert that some type can be synthesized from the given term. Checking
    rules assert that given both a term and a type we can check if the term is of that
    type. *)
module TypingRule : sig
  type 'info t =
    { tm : 'info BindingAwarePattern.t
    ; ty : 'info BindingAwarePattern.t
    }

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val erase : _ t -> unit t
end

module TypingClause : sig
  type 'info inference_rule = 'info TypingRule.t
  type 'info checking_rule = 'info TypingRule.t

  type 'info t =
    | InferenceRule of 'info inference_rule
    | CheckingRule of 'info checking_rule

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val erase : _ t -> unit t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

exception StaticsParseError of string

(** A hypothesis contains a set of variables (and their types) that must appear in the
    context, as well as an inference or checking clause. *)
module Hypothesis : sig
  type 'info t = 'info BindingAwarePattern.t Lvca_util.String.Map.t * 'info TypingClause.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val erase : _ t -> unit t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val pattern : OptRange.t BindingAwarePattern.t ParseUtil.t
    val typed_term : (string * OptRange.t BindingAwarePattern.t) ParseUtil.t

    (** @raise StaticsParseError *)
    val context : OptRange.t BindingAwarePattern.t Lvca_util.String.Map.t ParseUtil.t

    (** @raise StaticsParseError *)
    val t : OptRange.t t ParseUtil.t
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

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val line : string option ParseUtil.t

    (** @raise StaticsParseError *)
    val t : OptRange.t t ParseUtil.t
  end
end

module Typing : sig
  type 'info t = Typing of 'info Nominal.Term.t * 'info Nominal.Term.t

  val erase : _ t -> unit t
end

type 'info t = 'info Rule.t list

val erase : _ t -> unit t

module Parse (Comment : ParseUtil.Comment_int) : sig
  (** @raise StaticsParseError *)
  val t : OptRange.t t ParseUtil.t

  (** @raise StaticsParseError *)
  val whitespace_t : OptRange.t t ParseUtil.t
end
