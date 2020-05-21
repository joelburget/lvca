(** Statics describe the rules for stating whether an expression is well-formed.
 This implementation is for expressing bidirectional typing rules.
 *)

module Lexer = Statics_Lexer
module Parser = Statics_Parser
module ParseErrors = Statics_ParseErrors
module Types = Statics_Types

(** A term is the same as Binding.DeBruijn.term, but allows for free variables. *)
type term = Types.term =
  | Operator of string * scope list
  | Bound of int * int
  (** Bound vars come via conversion of de Bruijn terms. *)
  | Free of string
  (** Free vars are used during typechecking. *)
  | Primitive of Primitive.t

and scope = Types.scope = Scope of Pattern.t list * term

val string_of_term : term -> string
val string_of_scope : scope -> string

(** Both typing and inference rules share this shape.

 There is an important distinction in terms of the typechecking algorithm, though.
 Inference rules assert that some type can be synthesized from the given term. Checking
 rules assert that given both a term and a type we can check if the term is of that type.
 *)
type typing_rule = Types.typing_rule =
  { tm : term
  ; ty : term
  }

type inference_rule = typing_rule
type checking_rule = typing_rule

type typing_clause = Types.typing_clause =
  | InferenceRule of inference_rule
  | CheckingRule of checking_rule

(** A hypothesis contains a set of variables (and their types) that must appear in the
  context, as well as an inference or checking clause.
 *)
type hypothesis = term Util.String.Map.t * typing_clause

(** A rule contains a set of hypotheses, an optional name, and a conclusion *)
type rule = Types.rule =
  { hypotheses : hypothesis list
  ; name : string option
  ; conclusion : hypothesis
  }

type typing = Types.typing = Typing of term * term

val of_de_bruijn : Binding.DeBruijn.term -> term

(** Raised by [to_de_bruijn_exn] when it encounters a free variable. *)
exception FreeVar of string

(** Convert a [term] to a de Bruijn representation. See also [of_de_bruijn].
 @raise FreeVar *)
val to_de_bruijn_exn : term -> Binding.DeBruijn.term
