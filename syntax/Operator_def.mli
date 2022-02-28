open Lvca_util

type t =
  | Operator_def of Provenance.t * string * Arity.t
      (** An operator is defined by its tag and arity. *)

val mk : ?provenance:Provenance.t -> string -> Arity.t -> t
val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val pp : t Fmt.t
val parse : t Lvca_parsing.Parser.t
val kind_check : Int.Set.t String.Map.t -> t -> Int.Set.t String.Map.t
