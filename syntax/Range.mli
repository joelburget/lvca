type t =
  { start : int
  ; finish : int
  }

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

val pp : t Fmt.t
