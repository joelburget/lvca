(** An implementation of bidirectional typechecking. *)

open Lvca_syntax
open Statics

module Env : sig
  type t =
    { rules : Rule.t list (** The (checking / inference) rules we can apply *)
    ; var_types : Nominal.Term.t Lvca_util.String.Map.t
          (** The types of all known free variables *)
    }

  val ( = ) : t -> t -> bool
end

module Capture : sig
  type t = Binding_aware_pattern.Capture.t

  val ( = ) : t -> t -> bool
end

module Check_error : sig
  type t =
    | Check_error of string
    | Bad_merge of Capture.t * Capture.t

  val ( = ) : t -> t -> bool
end

module Trace_entry : sig
  type t =
    | Check_trace of Env.t * Typing.t
    | Check_success
    | Check_failure of Check_error.t
    | Infer_trace of Env.t * Nominal.Term.t
    | Inferred of Nominal.Term.t

  val ( = ) : t -> t -> bool
end

module Trace_step : sig
  type t = Trace_entry.t list

  val ( = ) : t -> t -> bool
end

(*
val check_trace : ( Trace_step.t -> unit) ->  Env.t ->  Typing.t ->  Check_error.t option

val infer_trace
  :  ( Trace_step.t -> unit)
  ->  Env.t
  ->  Nominal.term
  -> ( Nominal.term, 'a Check_error.t) Result.t

val check :  Env.t ->  Typing.t ->  Check_error.t option
val infer :  Env.t ->  Nominal.term -> ( Nominal.term,  Check_error.t) Result.t
*)

val check_trace : (Trace_step.t -> unit) -> Env.t -> Typing.t -> Check_error.t option

val infer_trace
  :  (Trace_step.t -> unit)
  -> Env.t
  -> Nominal.Term.t
  -> (Nominal.Term.t, Check_error.t) Result.t

val check : Env.t -> Typing.t -> Check_error.t option
val infer : Env.t -> Nominal.Term.t -> (Nominal.Term.t, Check_error.t) Result.t
