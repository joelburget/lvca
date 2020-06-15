(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [core_scope],
      [core_case_scope], (and [Pattern.t]).
    - [core_module] contains imports and a sequence of declarations.
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

val pp_core : Format.formatter -> term -> unit
val pp_core_str : term -> string

type core_defn =
  { name : string
  ; ty : sort
  ; defn : term
  }

type import = AbstractSyntax.import

type core_module = CoreModule of import list * core_defn list

val pp_module : Format.formatter -> core_module -> unit
val pp_module_str : core_module -> string

type eval_error = string * term

(** @raise [eval_error] *)
val eval_exn : term -> Nominal.term
val eval : term -> (Nominal.term, eval_error) Base.Result.t

module Parse (Lex : Util.Angstrom.Lexical_int) : sig
  val term : term Angstrom.t
end

(** Convert a module to a nominal term, for storage. *)
(* val module_to_term : core_module -> Nominal.term *)
