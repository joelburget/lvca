open Lvca_util

(** Represents a place where a sort can go in a valence. *)
type t =
  | Sort_binding of Sort.t
  | Sort_pattern of Pattern_sort.t

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val pp : t Fmt.t
val kind_check : Int.Set.t String.Map.t -> t -> Int.Set.t String.Map.t

(** Instantiate concrete vars in a sort *)
val instantiate : Sort.t String.Map.t -> t -> t

val parse : t Lvca_parsing.Parser.t
