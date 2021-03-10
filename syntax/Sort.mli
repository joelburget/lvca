(** Sorts divide ASTs into syntactic categories.

    We don't allow higher-order sorts. In other words, no functions at the sort level. In
    other words, the head of an application is always concrete. *)

type 'info t =
  | Ap of 'info * string * 'info t list (** A higher-kinded sort can be applied *)
  | Name of 'info * string

val equal : ('info -> 'info -> bool) -> 'info t -> 'info t -> bool
val pp : Format.formatter -> _ t -> unit
val instantiate : 'info t Lvca_util.String.Map.t -> 'info t -> 'info t
val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase_info : _ t -> unit t

(* TODO: remove? *)
(*
val of_term
  :  ('info, 'prim) Nominal.term
  -> ('info t, ('info, 'prim) Nominal.term) Result.t
  *)

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : OptRange.t t ParseUtil.t
end
