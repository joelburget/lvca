(** The kind of a sort is the number of arguments it takes. Invariant: must be a natural
    number. *)
type t = Kind of Provenance.t * int

val mk : ?provenance:Provenance.t -> int -> t
val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val info : t -> Provenance.t
val pp : t Fmt.t

module Parse : sig
  val t : t Lvca_parsing.Parser.t
  val decl : (string * t) Lvca_parsing.Parser.t
end
