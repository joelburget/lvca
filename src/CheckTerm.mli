module Types = AbstractSyntax.Types
module Nominal = Binding.Nominal

type abstract_syntax_check_failure_frame =
  { term : (Pattern.t, Nominal.term) Base.Either.t
  (** Term that failed to check *)
  ; sort : Types.sort
  (** Sort it failed to check against *)
  }

(** A check failure includes both an error message and the stack of terms / patterns
  leading to the problematic term / pattern.
  *)
type abstract_syntax_check_failure =
  { message : string
  ; stack : abstract_syntax_check_failure_frame list
  }

val pp_failure : Format.formatter -> abstract_syntax_check_failure -> unit

(** Check that this pattern is valid and return the valence for each variable it binds.

 Checks performed:
 + Primitives: string and integer literals coincide with the string and integer sorts.
   XXX what if they're aliased?
 + All used operators are found (in the sort corresponding to the pattern type).
 + All operators have the correct number of subterms for their arity.
   - Fixed arity patterns must have the exact number of subterms.
   - Variable arity patterns may have any number.
 + Patterns can't see valence: they can only bind subterms with some given valence.
 *)
val check_pattern
  :  Types.t
  -> Types.sort
  -> Pattern.t
  -> (Types.valence Util.String.Map.t, abstract_syntax_check_failure) Result.t

(** Check that the given term matches the given sort.

 This recursively checks subterms and patterns.

 Checks performed:
   + All used variables must be bound.
   + Variables must have the correct sort at their use site.
   + Primitives must have the correct sort (string / integer).
   + All mentioned operators must appear in the relevant sort.
   + All operators must have the correct number of subterms.
   + Variable-arity terms can have only non-binding terms as children
   + Fixed-valence terms must have the correct number of binders. All must be variables.
   + Variable-valence terms must have one binder, a pattern.
 *)
val check_term
  :  Types.t (** Abstract syntax *)
  -> Types.sort (** Sort to check term against *)
  -> Nominal.term
  -> abstract_syntax_check_failure option
