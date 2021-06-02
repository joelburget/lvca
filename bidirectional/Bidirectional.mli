(** An implementation of bidirectional typechecking. *)

open Lvca_provenance
open Lvca_syntax
open Statics

type 'info capture = 'info Binding_aware_pattern.Capture.t

type 'info env =
  { rules : 'info Rule.t list (** The (checking / inference) rules we can apply *)
  ; var_types : 'info Nominal.Term.t Lvca_util.String.Map.t
        (** The types of all known free variables *)
  }

type 'info check_error =
  | Check_error of string
  | Bad_merge of 'info capture * 'info capture

type 'info trace_entry =
  | Check_trace of 'info env * 'info Typing.t
  | Check_success
  | Check_failure of 'info check_error
  | Infer_trace of 'info env * 'info Nominal.Term.t
  | Inferred of 'info Nominal.Term.t

type 'info trace_step = 'info trace_entry list

(*
val check_trace : ('info trace_step -> unit) -> 'info env -> 'info Typing.t -> 'info check_error option

val infer_trace
  :  ('info trace_step -> unit)
  -> 'info env
  -> 'info Nominal.term
  -> ('info Nominal.term, 'a check_error) Result.t

val check : 'info env -> 'info Typing.t -> 'info check_error option
val infer : 'info env -> 'info Nominal.term -> ('info Nominal.term, 'info check_error) Result.t
*)

val check_trace
  :  (Opt_range.t trace_step -> unit)
  -> Opt_range.t env
  -> Opt_range.t Typing.t
  -> Opt_range.t check_error option

val infer_trace
  :  (Opt_range.t trace_step -> unit)
  -> Opt_range.t env
  -> Opt_range.t Nominal.Term.t
  -> (Opt_range.t Nominal.Term.t, Opt_range.t check_error) Result.t

val check : Opt_range.t env -> Opt_range.t Typing.t -> Opt_range.t check_error option

val infer
  :  Opt_range.t env
  -> Opt_range.t Nominal.Term.t
  -> (Opt_range.t Nominal.Term.t, Opt_range.t check_error) Result.t
