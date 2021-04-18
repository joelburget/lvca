(** An implementation of bidirectional typechecking. *)

open Lvca_syntax
open Statics

type 'info capture = 'info BindingAwarePattern.capture

type 'info env =
  { rules : 'info Rule.t list (** The (checking / inference) rules we can apply *)
  ; var_types : 'info Nominal.term Lvca_util.String.Map.t
        (** The types of all known free variables *)
  }

type 'info check_error =
  | CheckError of string
  | BadMerge of 'info capture * 'info capture

type 'info trace_entry =
  | CheckTrace of 'info env * 'info Typing.t
  | CheckSuccess
  | CheckFailure of 'info check_error
  | InferTrace of 'info env * 'info Nominal.term
  | Inferred of 'info Nominal.term

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
  :  (OptRange.t trace_step -> unit)
  -> OptRange.t env
  -> OptRange.t Typing.t
  -> OptRange.t check_error option

val infer_trace
  :  (OptRange.t trace_step -> unit)
  -> OptRange.t env
  -> OptRange.t Nominal.term
  -> (OptRange.t Nominal.term, OptRange.t check_error) Result.t

val check : OptRange.t env -> OptRange.t Typing.t -> OptRange.t check_error option

val infer
  :  OptRange.t env
  -> OptRange.t Nominal.term
  -> (OptRange.t Nominal.term, OptRange.t check_error) Result.t
