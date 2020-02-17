open Statics

type env = {
  (** The (checking / inference) rules we can apply *)
  rules     : rule list;
  (** The types of all known free variables *)
  var_types : term Core_kernel.String.Map.t;
}

(* TODO: add docs *)
exception BadTermMerge of term * term
exception BadScopeMerge of scope * scope

(* TODO: remove from public interface? *)
(** Raised by check *)
exception CheckError of string

type trace_entry =
  | CheckTrace of env * typing
  | CheckSuccess
  | CheckFailure of string
  | InferTrace of env * term
  | Inferred   of term

type trace_step = trace_entry list

  (*
   module type CHECKINFER = sig
   val check : env -> typing -> unit
   val infer : env -> term -> term
   end

   module type TRACER = sig
   val emit_trace : trace_entry -> unit
   end

   module CheckInfer (Tracer : TRACER) = struct
   val check : env -> typing -> unit
   val infer : env -> term -> term
   end
*)

val check_trace : (trace_step -> unit) -> env -> typing -> unit
val infer_trace : (trace_step -> unit) -> env -> term -> term
val check : env -> typing -> unit
val infer : env -> term -> term
