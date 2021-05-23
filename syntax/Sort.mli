(** Sorts divide ASTs into syntactic categories.

    We don't allow higher-order sorts. In other words, no functions at the sort level. In
    other words, the head of an application is always concrete. *)

type 'info t =
  | Ap of 'info * string * 'info t list (** A higher-kinded sort can be applied *)
  | Name of 'info * string

module Plain : sig
  type t =
    | Ap of string * t list
    | Name of string
end

val of_plain : Plain.t -> Plain.t t
val to_plain : _ t -> Plain.t
val equal : ('info -> 'info -> bool) -> 'info t -> 'info t -> bool
val info : 'info t -> 'info
val pp : _ t Fmt.t
val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val instantiate : 'info t Lvca_util.String.Map.t -> 'info t -> 'info t
val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase_info : _ t -> unit t
val split : 'info t -> string * 'info t list

val kind_check
  :  Lvca_util.Int.Set.t Lvca_util.String.Map.t
  -> 'info t
  -> Lvca_util.Int.Set.t Lvca_util.String.Map.t

(* TODO: remove? *)
(*
val of_term
  :  ('info, 'prim) Nominal.term
  -> ('info t, ('info, 'prim) Nominal.term) Result.t
  *)

module Parse (Comment : ParseUtil_intf.Comment_s) : sig
  val t : Lvca_provenance.OptRange.t t ParseUtil.t
end
