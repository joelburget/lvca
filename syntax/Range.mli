type t =
  { start : int
  ; finish : int
  }

type Stdlib.Format.stag += Stag of t

val mk : int -> int -> t
val to_string : t -> string

(** Extend this range to include the given position *)
val extend_to : t -> int -> t

(** Append two ranges. This creates a new range spanning from the earlier start to the
    later finish. *)
val union : t -> t -> t

(** Combine a list of ranges. This creates a new range spanning from the earliest start to
    the latest finish. *)
val list_range : t list -> t option

(** Raised by [list_range_nonempty] when passed an empty list *)
exception Empty_list

(** Combine a list of ranges. This creates a new range spanning from the earliest start to
    the latest finish.

    @raise Empty_list *)
val list_range_nonempty : t list -> t

(** Are the two ranges equal *)
val ( = ) : t -> t -> bool

(** Does one range occur entirely before the other *)
val is_before : t -> t -> bool

(** Is the first entirely contained in the second? *)
val is_subset : t -> t -> bool

(** Do the two ranges intersect? *)
val intersect : t -> t -> t option

(** Pretty-print this range. *)
val pp : t Fmt.t

(** For testing only: used to enable outputting of the [Stag] semantic tag. *)
val stag_functions : Stdlib.Format.formatter_stag_functions
