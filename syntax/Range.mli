type t =
  { start : int
  ; finish : int
  }

type Caml.Format.stag += Stag of t

val mk : int -> int -> t
val to_string : t -> string

val (<>) : t -> t -> t
(** Append two ranges. This creates a new range spanning from the earlier start to the
    later finish. *)

val list_range : t list -> t option
(** Combine a list of ranges. This creates a new range spanning from the earliest start to
    the latest finish. *)

exception Empty_list
(** Raised by [list_range_nonempty] when passed an empty list *)

val list_range_nonempty : t list -> t
(** Combine a list of ranges. This creates a new range spanning from the earliest start to
    the latest finish. *)

val (=) : t -> t -> bool
(** Are the two ranges equal *)

val (<) : t -> t -> bool
(** Is the first entirely contained in the second? *)

val intersect : t -> t -> t option
(** Do the two ranges intersect? *)

val pp : t Fmt.t
(** Pretty-print this range. *)

val stag_functions : Caml.Format.formatter_stag_functions
(** For testing only: used to enable outputting of the [Stag] semantic tag. *)
