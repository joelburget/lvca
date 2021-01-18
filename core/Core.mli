(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [core_scope],
      [core_case_scope], (and [Pattern.t]).
    - [eval] is used to evaluate a core term *)

open Lvca_syntax

type is_rec =
  | Rec
  | NoRec

type 'a term =
  | Term of ('a, Primitive.t) Nominal.term
  | CoreApp of 'a * 'a term * 'a term
  | Case of 'a * 'a term * 'a core_case_scope list (** Cases match patterns *)
  | Lambda of 'a * Sort.t * 'a core_scope
      (** Lambdas bind variables. Patterns not allowed. *)
  | Let of 'a * is_rec * 'a term * 'a core_scope
      (** Lets bind variables. Patterns not allowed. *)

and 'a core_scope = Scope of string * 'a term

and 'a core_case_scope = CaseScope of ('a, Primitive.t) Pattern.t * 'a term

val map_loc : f:('a -> 'b) -> 'a term -> 'b term
val erase : _ term -> unit term
val location : 'a term -> 'a
val pp : Format.formatter -> 'a term -> unit
val to_string : 'a term -> string

type 'a eval_error = string * 'a term

val eval_ctx
  :  ('a, Primitive.t) Nominal.term Lvca_util.String.Map.t
  -> 'a term
  -> (('a, Primitive.t) Nominal.term, 'a eval_error) Base.Result.t

val eval : 'a term -> (('a, Primitive.t) Nominal.term, 'a eval_error) Base.Result.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val term : OptRange.t term ParseUtil.t
end
