(** Statics describe the rules for stating whether an expression is well-formed. This
    implementation is for expressing bidirectional typing rules. *)

open Lvca_syntax

type 'info term = ('info, Primitive.t) BindingAwarePattern.t

(** Both typing and inference rules share this shape.

    There is an important distinction in terms of the typechecking algorithm, though.
    Inference rules assert that some type can be synthesized from the given term. Checking
    rules assert that given both a term and a type we can check if the term is of that
    type. *)
type 'info typing_rule =
  { tm : 'info term
  ; ty : 'info term
  }

type 'info inference_rule = 'info typing_rule
type 'info checking_rule = 'info typing_rule

type 'info typing_clause =
  | InferenceRule of 'info inference_rule
  | CheckingRule of 'info checking_rule

(** A hypothesis contains a set of variables (and their types) that must appear in the
    context, as well as an inference or checking clause. *)
type 'info hypothesis = 'info term Lvca_util.String.Map.t * 'info typing_clause

(** A rule contains a set of hypotheses, an optional name, and a conclusion *)
type 'info rule =
  { hypotheses : 'info hypothesis list
  ; name : string option
  ; conclusion : 'info hypothesis
  }

type 'info typing = Typing of 'info term * 'info term
type 'info t = 'info rule list

val erase_typing_rule : _ typing_rule -> unit typing_rule
val erase_typing_clause : _ typing_clause -> unit typing_clause
val erase_hypothesis : _ hypothesis -> unit hypothesis
val erase_rule : _ rule -> unit rule
val erase_typing : _ typing -> unit typing
val erase : _ t -> unit t

module Parse (Comment : ParseUtil.Comment_int) : sig
  exception StaticsParseError of string

  val term : OptRange.t term ParseUtil.t
  val typing_clause : OptRange.t typing_clause ParseUtil.t
  val typed_term : (string * OptRange.t term) ParseUtil.t

  (** @raise StaticsParseError *)
  val context : OptRange.t term Lvca_util.String.Map.t ParseUtil.t

  (** @raise StaticsParseError *)
  val hypothesis : OptRange.t hypothesis ParseUtil.t

  val line : string option ParseUtil.t

  (** @raise StaticsParseError *)
  val rule : OptRange.t rule ParseUtil.t

  (** @raise StaticsParseError *)
  val t : OptRange.t t ParseUtil.t

  (** @raise StaticsParseError *)
  val whitespace_t : OptRange.t t ParseUtil.t
end
