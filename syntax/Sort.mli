(** Sorts divide ASTs into syntactic categories.

    Concrete sorts are always represented by a [SortAp], even if not applied to anything.
    For example, [integer] is represented as [SortAp ("integer", \[\])]. *)

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
