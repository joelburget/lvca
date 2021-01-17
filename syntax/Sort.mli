(** Sorts divide ASTs into syntactic categories.

    We don't allow higher-order sorts. In other words, no functions at the sort level. In
    other words, the head of an application is always concrete. *)

type t =
  | Ap of string * t list (** A higher-kinded sort can be applied *)
  | Name of string

val ( = ) : t -> t -> bool
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val instantiate : t Lvca_util.String.Map.t -> t -> t

(* TODO: remove? *)
val of_term : ('loc, 'prim) Nominal.term -> (t, ('loc, 'prim) Nominal.term) Result.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : t ParseUtil.t
end
