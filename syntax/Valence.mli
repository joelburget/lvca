open Lvca_util

(** A valence represents a sort, as well as the number and sorts of the variables bound
    within it. Valences are most often used to represent slots in an operator. *)
type t = Valence of Sort_slot.t list * Sort.t

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val pp : t Fmt.t

(** Instantiate concrete vars in a valence *)
val instantiate : Sort.t String.Map.t -> t -> t

val parse : t Lvca_parsing.t
val kind_check : Int.Set.t String.Map.t -> t -> Int.Set.t String.Map.t
