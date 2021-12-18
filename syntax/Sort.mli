(** Sorts divide ASTs into syntactic categories.

    We don't allow higher-order sorts. In other words, no functions at the sort level. In
    other words, the head of an application is always concrete. *)
open Lvca_util

type t =
  | Ap of Provenance.t * string * t list (** A higher-kinded sort can be applied *)
  | Name of Provenance.t * string

val mk_Ap : ?provenance:Provenance.t -> string -> t list -> t
val mk_Name : ?provenance:Provenance.t -> string -> t
val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val info : t -> Provenance.t
val pp : t Fmt.t
val instantiate : t String.Map.t -> t -> t
val split : t -> string * t list
val name : t -> string
val kind_check : Int.Set.t String.Map.t -> t -> Int.Set.t String.Map.t
val parse : String.Set.t -> t Lvca_parsing.t
