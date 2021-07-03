(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [scope], [case_scope], (and
      [Binding_aware_pattern.t.t]).
    - [eval] is used to evaluate a core term *)

open Lvca_provenance
open Lvca_syntax
open Lvca_util

(** {1 Types} *)

module Is_rec : sig
  module Types : sig
    type 'info is_rec =
      | Rec of 'info
      | No_rec of 'info
  end

  module Is_rec : sig
    type 'info t = 'info Types.is_rec

    val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  end
end

module Type : sig
  type 'info t =
    | Arrow of 'info t list
    | Sort of 'info Sort.t

  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
  val parse : Opt_range.t t Lvca_parsing.t
end

module Types : sig
  type 'info term =
    | Term of 'info Nominal.Term.t
    | Core_app of 'info * 'info term * 'info term list
    | Case of 'info * 'info term * 'info case_scope list (** Cases match patterns *)
    | Lambda of 'info * 'info Type.t * 'info scope
        (** Lambdas bind variables. Patterns not allowed. *)
    | Let of 'info let_ (** Lets bind variables. Patterns not allowed. *)
    | Var of 'info * string

  and 'info let_ =
    { info : 'info
    ; is_rec : 'info Is_rec.Is_rec.t
    ; tm : 'info term
    ; ty : 'info Type.t option
    ; scope : 'info scope
    }

  and 'info scope = Scope of string * 'info term

  and 'info case_scope = Case_scope of 'info Binding_aware_pattern.t * 'info term
end

module Term : sig
  type 'info t = 'info Types.term =
    | Term of 'info Nominal.Term.t
    | Core_app of 'info * 'info t * 'info t list
    | Case of 'info * 'info t * 'info Types.case_scope list
    | Lambda of 'info * 'info Type.t * 'info Types.scope
    | Let of 'info Types.let_
    | Var of 'info * string

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val info : 'info t -> 'info
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
  val pp : _ t Fmt.t
  val parse : Opt_range.t t Lvca_parsing.t
end

module Let : sig
  type 'info t = 'info Types.let_ =
    { info : 'info
    ; is_rec : 'info Is_rec.Is_rec.t
    ; tm : 'info Types.term
    ; ty : 'info Type.t option
    ; scope : 'info Types.scope
    }

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val info : 'info t -> 'info
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
end

module Scope : sig
  type 'info t = 'info Types.scope = Scope of string * 'info Types.term

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
end

module Case_scope : sig
  type 'info t = 'info Types.case_scope =
    | Case_scope of 'info Binding_aware_pattern.t * 'info Types.term

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
end

(** {1 Core type} *)
module Module : sig
  type 'info t =
    { externals : (string * 'info Type.t) list
    ; defs : (string * 'info Term.t) list
    }

  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
  val pp : _ t Fmt.t
  val parse : Opt_range.t t Lvca_parsing.t
end

(** {1 Checking} *)

type 'info check_env =
  { type_env : 'info Type.t String.Map.t
  ; syntax : 'info Abstract_syntax.t
  }

module Check_error' : sig
  type term_failure_inference_reason =
    | Term_var_not_found of string
    | Operator_not_supported

  type 'info t =
    | Cant_infer_case
    | Cant_infer_lambda
    | Var_not_found
    | Operator_not_found
    | Mismatch of 'info Type.t
    | Term_isnt_arrow
    | Failed_term_inference of term_failure_inference_reason
    | Failed_check_term of
        ('info, ('info Pattern.t, 'info Nominal.Term.t) Base.Either.t) Check_failure.t
end

module Check_error : sig
  type 'info t =
    { env : 'info check_env
    ; tm : 'info Term.t
    ; ty : 'info Type.t
    ; error : 'info Check_error'.t
    }
end

module Infer_error : sig
  type 'info t =
    { env : 'info check_env
    ; tm : 'info Term.t
    ; error : 'info Check_error'.t
    }
end

(** Typecheck a term in an environment. *)
val infer
  :  'info option check_env
  -> 'info option Term.t
  -> ('info option Type.t, 'info option Infer_error.t) Result.t

val check
  :  'info option check_env
  -> 'info option Term.t
  -> 'info option Type.t
  -> 'info option Check_error.t option

(** {1 Evaluation} *)

type 'info env = 'info Nominal.Term.t String.Map.t
type 'info eval_error = string * 'info Term.t

val eval_in_ctx
  :  'info env
  -> 'info Term.t
  -> ('info Nominal.Term.t, 'info eval_error) Base.Result.t

val eval : 'info Term.t -> ('info Nominal.Term.t, 'info eval_error) Base.Result.t

(** {1 Patterns} *)
val match_pattern
  :  'info Nominal.Term.t
  -> 'info Binding_aware_pattern.t
  -> 'info env option

val find_match
  :  'info Nominal.Term.t
  -> 'b Case_scope.t list
  -> ('b Term.t * 'info env) option

(* val coverage_check : 'info Cases.t -> *)
val preimage : 'info Case_scope.t list -> 'info Binding_aware_pattern.t list
val reverse : 'info Nominal.Term.t -> 'info Case_scope.t list -> 'info env option
