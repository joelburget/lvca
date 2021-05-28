type 'a t =
  { latest_pos : int
        (** The first position *after* the last non-whitespace character read. This may be
            used as the endpoint (non-inclusive) of the range for enclosing parsers *)
  ; value : 'a
  ; range : Lvca_provenance.OptRange.t
  }

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val pp : 'a Fmt.t -> 'a t Fmt.t
