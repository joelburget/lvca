(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [scope], [case_scope], (and
      [Pattern.t]).
    - [eval] is used to evaluate a core term *)

open Lvca_provenance

(** {1 Types} *)
type 'info n_term = 'info Lvca_syntax.Nominal.Term.t

type 'info pattern = 'info Lvca_syntax.BindingAwarePattern.t

type is_rec =
  | Rec
  | NoRec

type 'info term =
  | Term of 'info n_term
  | CoreApp of 'info * 'info term * 'info term list
  | Case of 'info * 'info term * 'info cases (** Cases match patterns *)
  | Lambda of 'info * 'info Lvca_syntax.Sort.t * 'info scope
      (** Lambdas bind variables. Patterns not allowed. *)
  | Let of 'info let_ (** Lets bind variables. Patterns not allowed. *)
  | Var of 'info * string

and 'info let_ =
  { info : 'info
  ; is_rec : is_rec
  ; tm : 'info term
  ; ty : 'info n_term option
  ; scope : 'info scope
  }

and 'info scope = Scope of string * 'info term

and 'info cases = 'info case_scope list

and 'info case_scope = CaseScope of 'info pattern * 'info term

val equal : info_eq:('info -> 'info -> bool) -> 'info term -> 'info term -> bool
val map_info : f:('a -> 'b) -> 'a term -> 'b term
val erase : _ term -> unit term
val info : 'info term -> 'info
val pp : Format.formatter -> _ term -> unit
val to_string : _ term -> string

(** {1 Checking} *)

type 'info check_env = 'info Lvca_syntax.Sort.t Lvca_util.String.Map.t
type 'info check_error

val check : 'info check_env -> 'info term -> 'info check_error option

(** {1 Evaluation} *)

type 'info env = 'info n_term Lvca_util.String.Map.t
type 'info eval_error = string * 'info term

type 'info primitive_eval =
  ('info env -> 'info term -> ('info n_term, 'info eval_error) Result.t)
  -> ('info env -> 'info n_term -> ('info n_term, 'info eval_error) Result.t)
  -> 'info env
  -> 'info term
  -> string
  -> 'info term list
  -> ('info n_term, 'info eval_error) Result.t

val eval_ctx
  :  'info primitive_eval
  -> 'info env
  -> 'info term
  -> ('info n_term, 'info eval_error) Base.Result.t

val eval
  :  'info primitive_eval
  -> 'info term
  -> ('info n_term, 'info eval_error) Base.Result.t

(** {1 Patterns} *)
val match_pattern : 'info n_term -> 'info pattern -> 'info env option

val find_match : 'info n_term -> 'b cases -> ('b term * 'info env) option

(* val coverage_check : 'info cases -> *)
val preimage : 'info cases -> 'info pattern list
val reverse : 'info n_term -> 'info cases -> 'info env option

(** {1 Parsing} *)
module Parse : sig
  val term : OptRange.t term Lvca_parsing.t
end
