(** Tools for dealing with the core language in LVCA.

    - [term] defines expressions in the core language. It uses [scope], [case_scope], (and
      [Binding_aware_pattern.t.t]).
    - [eval] is used to evaluate a core term *)

open Lvca_provenance
open Lvca_syntax
open Lvca_util

(** {1 Types} *)

module Is_rec : sig
  type t =
    | Rec
    | No_rec

  val ( = ) : t -> t -> bool
end

module Type : sig
  type 'info t =
    | Arrow of 'info t list
    | Sort of 'info Sort.t

  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

  module Parse : sig
    val t : Opt_range.t t Lvca_parsing.t
  end
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
    ; is_rec : Is_rec.t
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

  module Parse : sig
    val t : Opt_range.t t Lvca_parsing.t
  end
end

module Let : sig
  type 'info t = 'info Types.let_ =
    { info : 'info
    ; is_rec : Is_rec.t
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
type 'info t =
  { externals : (string * 'info Type.t) list
  ; defs : (string * 'info Term.t) list
  }

val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val pp : _ t Fmt.t

(** {2 Parsing} *)
module Parse : sig
  val t : Opt_range.t t Lvca_parsing.t
end

(** {1 Checking} *)

type 'info check_env = 'info Type.t String.Map.t
type 'info check_error

(** Typecheck a term in an environment. *)
val check : 'info check_env -> 'info Term.t -> 'info check_error option

(** {1 Evaluation} *)

type 'info env = 'info Nominal.Term.t String.Map.t
type 'info eval_error = string * 'info Term.t

val eval_ctx
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
