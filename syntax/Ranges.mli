type t = Range.t list
(** The type of disjoint ranges in a buffer.

 Invariants: each range must be disjoint, ranges must be ordered.
*)

val invariants : t -> bool
(** Test invariants. Returns true on success. *)

val of_opt_range : OptRange.t -> t

val of_list : Range.t list -> t
(** Convert from a (possibly unordered and overlapping) list of ranges. *)

val of_set : (Range.t , _) Base.Set.t -> t
(** Convert from a (possibly overlapping) set of ranges. *)

val to_string : t -> string

val (=) : t -> t -> bool
(** Are the two sets of ranges equal *)

val union : t -> t -> t
(** Union two sets of ranges. *)

val is_subset : t -> t -> bool
(** Is the first entirely contained in the second? *)

val intersect : t -> t -> t
(** The intersection of two ranges *)

val pp : t Fmt.t
(** Pretty-print this range. *)
