open Lvca_util

type t =
  | Sort_def of (string * Kind.t option) list * Operator_def.t list
      (** A sort is defined by a set of variables and a set of operators. *)

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val pp : name:string -> t Fmt.t
val kind_check : Int.Set.t String.Map.t -> string -> t -> Int.Set.t String.Map.t
val parse : (string * t) Lvca_parsing.t