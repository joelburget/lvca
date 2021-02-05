(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [scope], [case_scope], (and
      [Pattern.t]).
    - [eval] is used to evaluate a core term *)

type is_rec =
  | Rec
  | NoRec

type 'a term =
  | Term of ('a, Lvca_syntax.Primitive.t) Lvca_syntax.Nominal.term
  | CoreApp of 'a * 'a term * 'a term
  | Case of 'a * 'a term * 'a cases (** Cases match patterns *)
  | Lambda of 'a * 'a Lvca_syntax.Sort.t * 'a scope
      (** Lambdas bind variables. Patterns not allowed. *)
  | Let of 'a * is_rec * 'a term * 'a scope
      (** Lets bind variables. Patterns not allowed. *)

and 'a scope = Scope of string * 'a term

and 'a cases = 'a case_scope list

and 'a case_scope =
  | CaseScope of ('a, Lvca_syntax.Primitive.t) Lvca_syntax.Pattern.t * 'a term

val map_info : f:('a -> 'b) -> 'a term -> 'b term
val erase : _ term -> unit term
val info : 'a term -> 'a
val pp : Format.formatter -> 'a term -> unit
val to_string : 'a term -> string

type 'a eval_error = string * 'a term

val eval_ctx
  :  ('a, Lvca_syntax.Primitive.t) Lvca_syntax.Nominal.term Lvca_util.String.Map.t
  -> 'a term
  -> (('a, Lvca_syntax.Primitive.t) Lvca_syntax.Nominal.term, 'a eval_error) Base.Result.t

val eval
  :  'a term
  -> (('a, Lvca_syntax.Primitive.t) Lvca_syntax.Nominal.term, 'a eval_error) Base.Result.t

module Parse (Comment : Lvca_syntax.ParseUtil.Comment_int) : sig
  val term : Lvca_syntax.OptRange.t term Lvca_syntax.ParseUtil.t
end
