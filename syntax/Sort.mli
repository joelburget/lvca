(** Sorts divide ASTs into syntactic categories.

    We don't allow higher-order sorts. In other words, no functions at the sort level. In
    other words, the head of an application is always concrete. *)
open Lvca_util

type 'info t =
  | Ap of 'info * string * 'info t list (** A higher-kinded sort can be applied *)
  | Name of 'info * string

module Plain : sig
  type t =
    | Ap of string * t list
    | Name of string
end

val of_plain : Plain.t -> unit t
val to_plain : _ t -> Plain.t
val to_nominal : 'info t -> 'info t
val of_nominal : 'info t -> ('info t, 'info t) Result.t
val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
val info : 'info t -> 'info
val pp : _ t Fmt.t
val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val instantiate : 'info t String.Map.t -> 'info t -> 'info t
val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase_info : _ t -> unit t
val split : 'info t -> string * 'info t list
val kind_check : Int.Set.t String.Map.t -> 'info t -> Int.Set.t String.Map.t

(* TODO: remove? *)
(*
val of_term
  :  ('info, 'prim) Nominal.term
  -> ('info t, ('info, 'prim) Nominal.term) Result.t
  *)

val parse : comment:'a Lvca_parsing.t -> 'a Lvca_provenance.Commented.t t Lvca_parsing.t
