(** An implementation of bidirectional typechecking. *)

open Lvca_provenance
open Lvca_syntax
open Statics

module Env : sig
  type 'info t =
    { rules : 'info Rule.t list (** The (checking / inference) rules we can apply *)
    ; var_types : 'info Nominal.Term.t Lvca_util.String.Map.t
          (** The types of all known free variables *)
    }

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
end

module Capture : sig
  type 'info t = 'info Binding_aware_pattern.Capture.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
end

module Check_error : sig
  type 'info t =
    | Check_error of string
    | Bad_merge of 'info Capture.t * 'info Capture.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
end

module Trace_entry : sig
  type 'info t =
    | Check_trace of 'info Env.t * 'info Typing.t
    | Check_success
    | Check_failure of 'info Check_error.t
    | Infer_trace of 'info Env.t * 'info Nominal.Term.t
    | Inferred of 'info Nominal.Term.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
end

module Trace_step : sig
  type 'info t = 'info Trace_entry.t list

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
end

(*
val check_trace : ('info Trace_step.t -> unit) -> 'info Env.t -> 'info Typing.t -> 'info Check_error.t option

val infer_trace
  :  ('info Trace_step.t -> unit)
  -> 'info Env.t
  -> 'info Nominal.term
  -> ('info Nominal.term, 'a Check_error.t) Result.t

val check : 'info Env.t -> 'info Typing.t -> 'info Check_error.t option
val infer : 'info Env.t -> 'info Nominal.term -> ('info Nominal.term, 'info Check_error.t) Result.t
*)

val check_trace
  :  (Opt_range.t Trace_step.t -> unit)
  -> Opt_range.t Env.t
  -> Opt_range.t Typing.t
  -> Opt_range.t Check_error.t option

val infer_trace
  :  (Opt_range.t Trace_step.t -> unit)
  -> Opt_range.t Env.t
  -> Opt_range.t Nominal.Term.t
  -> (Opt_range.t Nominal.Term.t, Opt_range.t Check_error.t) Result.t

val check : Opt_range.t Env.t -> Opt_range.t Typing.t -> Opt_range.t Check_error.t option

val infer
  :  Opt_range.t Env.t
  -> Opt_range.t Nominal.Term.t
  -> (Opt_range.t Nominal.Term.t, Opt_range.t Check_error.t) Result.t
