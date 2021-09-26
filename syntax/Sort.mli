(** Sorts divide ASTs into syntactic categories.

    We don't allow higher-order sorts. In other words, no functions at the sort level. In
    other words, the head of an application is always concrete. *)
open Lvca_util

type 'info t =
  | Ap of 'info * string * 'info ap_list (** A higher-kinded sort can be applied *)
  | Name of 'info * string

and 'info ap_list =
  | Nil of 'info
  | Cons of 'info * 'info t * 'info ap_list

module Ap_list : sig
  val to_list : 'info ap_list -> 'info t list
  val of_list : default_info:'info -> 'info t list -> 'info ap_list
  val map : f:('info t -> 'info t) -> 'info ap_list -> 'info ap_list
end

val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
val info : 'info t -> 'info
val pp : _ t Fmt.t
val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val instantiate : 'info t String.Map.t -> 'info t -> 'info t
val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase_info : _ t -> unit t
val split : 'info t -> string * 'info t list
val kind_check : Int.Set.t String.Map.t -> 'info t -> Int.Set.t String.Map.t
val parse : comment:'a Lvca_parsing.t -> 'a Lvca_provenance.Commented.t t Lvca_parsing.t
