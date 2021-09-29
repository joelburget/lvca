(** Sorts divide ASTs into syntactic categories.

    We don't allow higher-order sorts. In other words, no functions at the sort level. In
    other words, the head of an application is always concrete. *)
open Lvca_util

type t =
  | Ap of Provenance.t * string * ap_list (** A higher-kinded sort can be applied *)
  | Name of Provenance.t * string

and ap_list =
  | Nil of Provenance.t
  | Cons of Provenance.t * t * ap_list

val mk_Ap : ?provenance:Provenance.t -> string -> ap_list -> t
val mk_Name : ?provenance:Provenance.t -> string -> t
val mk_Nil : ?provenance:Provenance.t -> unit -> ap_list
val mk_Cons : ?provenance:Provenance.t -> t -> ap_list -> ap_list

module Ap_list : sig
  val to_list : ap_list -> t list
  val of_list : t list -> ap_list
  val map : f:(t -> t) -> ap_list -> ap_list
end

val equivalent : info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val info : t -> Provenance.t
val pp : t Fmt.t
val instantiate : t String.Map.t -> t -> t
val split : t -> string * t list
val kind_check : Int.Set.t String.Map.t -> t -> Int.Set.t String.Map.t
val parse : t Lvca_parsing.t
