(** A pattern sort represents the sort of a pattern with variables of some sort. This is
    written as [pattern_sort\[var_sort\]]. *)
type t =
  { pattern_sort : Sort.t
  ; var_sort : Sort.t
  }

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val pp : t Fmt.t
val instantiate : Sort.t Lvca_util.String.Map.t -> t -> t
