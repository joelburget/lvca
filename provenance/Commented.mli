(** Metadata with both an optional comment and a range. *)
type 'comment t =
  { comment : 'comment option
  ; range : Opt_range.t
  }

val none : _ t
val get_comment : 'comment t -> 'comment option
val get_range : _ t -> Opt_range.t