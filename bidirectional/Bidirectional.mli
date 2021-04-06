(** An implementation of bidirectional typechecking. *)

open Lvca_syntax
open Statics

type 'a env =
  { rules : 'a Rule.t list (** The (checking / inference) rules we can apply *)
  ; var_types : 'a term Lvca_util.String.Map.t
        (** The types of all known free variables *)
  }

type 'info check_error =
  | CheckError of string
  | BadMerge of
      ('info, 'info Primitive.t) BindingAwarePattern.capture
      * ('info, 'info Primitive.t) BindingAwarePattern.capture

type 'a trace_entry =
  | CheckTrace of 'a env * 'a Typing.t
  | CheckSuccess
  | CheckFailure of 'a check_error
  | InferTrace of 'a env * 'a term
  | Inferred of 'a term

type 'a trace_step = 'a trace_entry list

(*
val check_trace : ('a trace_step -> unit) -> 'a env -> 'a Typing.t -> 'a check_error option

val infer_trace
  :  ('a trace_step -> unit)
  -> 'a env
  -> 'a term
  -> ('a term, 'a check_error) Result.t

val check : 'a env -> 'a Typing.t -> 'a check_error option
val infer : 'a env -> 'a term -> ('a term, 'a check_error) Result.t
*)

val check_trace
  :  (OptRange.t trace_step -> unit)
  -> OptRange.t env
  -> OptRange.t Typing.t
  -> OptRange.t check_error option

val infer_trace
  :  (OptRange.t trace_step -> unit)
  -> OptRange.t env
  -> OptRange.t term
  -> (OptRange.t term, OptRange.t check_error) Result.t

val check
  :  OptRange.t env
  -> OptRange.t Typing.t
  -> OptRange.t check_error option

val infer
  :  OptRange.t env
  -> OptRange.t term
  -> (OptRange.t term, OptRange.t check_error) Result.t
