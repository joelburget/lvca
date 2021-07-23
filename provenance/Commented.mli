(** Metadata with both an optional comment and a range. *)
type 'comment t =
  { comment : 'comment option
  ; range : Opt_range.t
  }

val none : _ t
val get_comment : 'comment t -> 'comment option
val get_range : _ t -> Opt_range.t
val equal : ('comment -> 'comment -> bool) -> 'comment t -> 'comment t -> bool
val pp : 'comment Fmt.t -> 'comment t Fmt.t
val of_opt_range : Opt_range.t -> _ t

module Properties : sig
  val round_trip : Opt_range.t -> Lvca_util.Property_result.t
end
