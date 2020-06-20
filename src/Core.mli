(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [core_scope],
      [core_case_scope], (and [Pattern.t]).
    - [defn] contains imports and a declaration.
    - [eval] is used to evaluate a core term

*)

open AbstractSyntax
open Binding

type is_rec = Rec | NoRec

type term =
  | Term of Nominal.term
  | CoreApp of term * term
  | Case of term * core_case_scope list
  (** Cases match patterns *)
  | Lambda of sort * core_scope
  (** Lambdas bind variables. Patterns not allowed. *)
  | Let of is_rec * term * core_scope
  (** Lets bind variables. Patterns not allowed. *)

and core_scope = Scope of string * term

and core_case_scope = CaseScope of Pattern.t * term

val pp : Format.formatter -> term -> unit
val to_string : term -> string

type import = AbstractSyntax.import

type defn = Defn of import list * term

val pp_defn : Format.formatter -> defn -> unit
val defn_to_string : defn -> string

type eval_error = string * term

(** @raise eval_error *)
val eval_ctx_exn : Nominal.term Util.String.Map.t -> term -> Nominal.term

(** @raise eval_error *)
val eval_exn : term -> Nominal.term

val eval : term -> (Nominal.term, eval_error) Base.Result.t

module Parse (Comment : Util.Angstrom.Comment_int) : sig
  val term : term Angstrom.t
  val defn : defn Angstrom.t
end

(** Convert a module to a nominal term, for storage. *)
(* val module_to_term : defn -> Nominal.term *)
