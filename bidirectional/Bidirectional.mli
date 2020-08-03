(** An implementation of bidirectional typechecking. *)

open Statics

type 'a env =
  { rules : 'a rule list (** The (checking / inference) rules we can apply *)
  ; var_types : 'a term Lvca_util.String.Map.t (** The types of all known free variables *)
  }

(* TODO: add docs *)
exception BadTermMerge of unit term * unit term
exception BadScopeMerge of unit scope * unit scope

(* TODO: remove from public interface? *)

(** Raised by check *)
exception CheckError of string

type 'a trace_entry =
  | CheckTrace of 'a env * 'a typing
  | CheckSuccess
  | CheckFailure of string
  | InferTrace of 'a env * 'a term
  | Inferred of 'a term

type 'a trace_step = 'a trace_entry list

(* module type CHECKINFER = sig val check : env -> typing -> unit val infer : env -> term
   -> term end

   module type TRACER = sig val emit_trace : trace_entry -> unit end

   module CheckInfer (Tracer : TRACER) = struct val check : env -> typing -> unit val
   infer : env -> term -> term end *)

val check_trace : ('a trace_step -> unit) -> 'a env -> 'a typing -> unit
val infer_trace : ('a trace_step -> unit) -> 'a env -> 'a term -> 'a term
val check : 'a env -> 'a typing -> unit
val infer : 'a env -> 'a term -> 'a term
