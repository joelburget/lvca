type t =
  { source : string
  ; range : Range.t
  }

type Stdlib.Format.stag += Stag of t

val mk : string -> int -> int -> t

(** Pretty-print this range. *)
val pp : t Fmt.t

(** Extend this range to include the given position *)
val extend_to : t -> int -> t

(** Append two ranges. This creates a new range spanning from the earlier start to the
    later finish. *)
val union : t -> t -> t option

(** Are the two ranges equal *)
val ( = ) : t -> t -> bool

(** Does one range occur entirely before the other *)
val is_before : t -> t -> bool

(** Is the first entirely contained in the second? *)
val is_subset : t -> t -> bool

(** Do the two ranges intersect? *)
val intersect : t -> t -> t option

val open_stag : Format.formatter -> t -> unit
val close_stag : Format.formatter -> t -> unit

(** For testing only: used to enable outputting of the [Stag] semantic tag. *)
val stag_functions : Stdlib.Format.formatter_stag_functions
