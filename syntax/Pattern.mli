(** Patterns for matching non-binding terms. *)

module Make : Pattern_intf.SF

(*
module Primitive : sig
  (** Hardcoded for the Primitive type *)

  val check
    :  'info AbstractSyntax.t (** Abstract syntax *)
    -> pattern_sort:'info Sort.t (** Sort to check pattern against *)
    -> var_sort:'info Sort.t (** Sort pattern must yield as variables *)
    -> ('info, 'info Primitive.t) t
    -> ( 'info Sort.t Lvca_util.String.Map.t
       , ('info, ('info, 'info Primitive.t) t) CheckFailure.t )
       Result.t
end
*)
