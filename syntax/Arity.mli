(** An arity specifies the arguments to an operator. *)
type t = Arity of Provenance.t * Valence.t list

val mk : ?provenance:Provenance.t -> Valence.t list -> t
val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val pp : t Fmt.t

(** Instantiate concrete vars in an arity *)
val instantiate : Sort.t Lvca_util.String.Map.t -> t -> t

val parse : t Lvca_parsing.t
