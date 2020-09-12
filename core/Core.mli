(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [core_scope],
      [core_case_scope], (and [Pattern.t]).
    - [defn] contains imports and a declaration.
    - [eval] is used to evaluate a core term *)

open Lvca_syntax
open AbstractSyntax

type is_rec =
  | Rec
  | NoRec

type 'a term =
  | Term of 'a Nominal.term
  | CoreApp of 'a term * 'a term
  | Case of 'a term * 'a core_case_scope list (** Cases match patterns *)
  | Lambda of sort * 'a core_scope (** Lambdas bind variables. Patterns not allowed. *)
  | Let of is_rec * 'a term * 'a core_scope
      (** Lets bind variables. Patterns not allowed. *)

and 'a core_scope = Scope of string * 'a term

and 'a core_case_scope = CaseScope of 'a Pattern.t * 'a term

val erase : 'a term -> unit term
val pp : Format.formatter -> 'a term -> unit
val to_string : 'a term -> string

type import = AbstractSyntax.import
type 'a defn = Defn of import list * 'a term

val pp_defn : Format.formatter -> 'a defn -> unit
val defn_to_string : 'a defn -> string
val erase_defn : 'a defn -> unit defn

type eval_error = string * unit (* TODO: 'a *) term

(** @raise eval_error *)
val eval_ctx_exn : 'a Nominal.term Lvca_util.String.Map.t -> 'a term -> 'a Nominal.term

(** @raise eval_error *)
val eval_exn : 'a term -> 'a Nominal.term

val eval : 'a term -> ('a Nominal.term, eval_error) Base.Result.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val term : OptRange.t term ParseUtil.t
  val defn : OptRange.t defn ParseUtil.t
end

(** Convert a module to a nominal term, for storage. *)

(* val module_to_term : defn -> Nominal.term *)
