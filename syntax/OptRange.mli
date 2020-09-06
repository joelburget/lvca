type t = Range.t option

type Caml.Format.stag += Stag of t

val mk : int -> int -> t
val to_string : t -> string

val extend_to : t -> int -> t
(** Extend this range to include the given position *)

val (<>) : t -> t -> t
(** Append two ranges. This creates a new range spanning from the earlier start to the
    later finish. *)

val list_range : t list -> t
(** Combine a list of ranges. This creates a new range spanning from the earliest start to
    the latest finish. *)

val (=) : t -> t -> bool
(** Are the two ranges equal *)

val (<) : t -> t -> bool
(** Is the first entirely contained in the second? *)

val intersect : t -> t -> t
(** Do the two ranges intersect? *)

val pp : t Fmt.t
(** Pretty-print this range. *)
