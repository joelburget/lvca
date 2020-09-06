(** Statics describe the rules for stating whether an expression is well-formed.
 This implementation is for expressing bidirectional typing rules.
 *)

open Lvca_syntax

(** A term is the same as Binding.DeBruijn.term, but allows for free variables. *)
type 'a term =
  | Operator of 'a * string * 'a scope list
  | Bound of 'a * int * int
  (** Bound vars come via conversion of de Bruijn terms. *)
  | Free of 'a * string
  (** Free vars are used during typechecking. *)
  | Primitive of 'a * Primitive.t

and 'a scope = Scope of 'a Pattern.t list * 'a term list

val location : 'a term -> 'a

val string_of_term : 'a term -> string
val string_of_scope : 'a scope -> string

(** Both typing and inference rules share this shape.

 There is an important distinction in terms of the typechecking algorithm, though.
 Inference rules assert that some type can be synthesized from the given term. Checking
 rules assert that given both a term and a type we can check if the term is of that type.
 *)
type 'a typing_rule =
  { tm : 'a term
  ; ty : 'a term
  }

type 'a inference_rule = 'a typing_rule
type 'a checking_rule = 'a typing_rule

type 'a typing_clause =
  | InferenceRule of 'a inference_rule
  | CheckingRule of 'a checking_rule

(** A hypothesis contains a set of variables (and their types) that must appear in the
  context, as well as an inference or checking clause.
 *)
type 'a hypothesis = 'a term Lvca_util.String.Map.t * 'a typing_clause

(** A rule contains a set of hypotheses, an optional name, and a conclusion *)
type 'a rule =
  { hypotheses : 'a hypothesis list
  ; name : string option
  ; conclusion : 'a hypothesis
  }

type 'a typing = Typing of 'a term * 'a term

type 'a t = 'a rule list

val erase_term : 'a term -> unit term
val erase_scope : 'a scope -> unit scope
val erase_typing_rule : 'a typing_rule -> unit typing_rule
val erase_typing_clause : 'a typing_clause -> unit typing_clause
val erase_hypothesis : 'a hypothesis -> unit hypothesis
val erase_rule : 'a rule -> unit rule
val erase_typing : 'a typing -> unit typing
val erase : 'a t -> unit t

val of_de_bruijn : 'a Binding.DeBruijn.term -> 'a term

(** Raised by [to_de_bruijn_exn] when it encounters a free variable. *)
exception FreeVar of string

(** Convert a [term] to a de Bruijn representation. See also [of_de_bruijn].
 @raise FreeVar *)
val to_de_bruijn_exn : 'a term -> 'a Binding.DeBruijn.term

module Parse (Comment : ParseUtil.Comment_int) : sig
  exception StaticsParseError of string

  val term : Range.t term Angstrom.t
  val typing_clause : Range.t typing_clause Angstrom.t
  val typed_term : (string * Range.t term) Angstrom.t

  (** @raise StaticsParseError *)
  val context : Range.t term Lvca_util.String.Map.t Angstrom.t

  (** @raise StaticsParseError *)
  val hypothesis : Range.t hypothesis Angstrom.t

  val line : string option Angstrom.t

  (** @raise StaticsParseError *)
  val rule : Range.t rule Angstrom.t

  (** @raise StaticsParseError *)
  val t : Range.t t Angstrom.t
end
