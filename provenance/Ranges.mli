(** The type of disjoint ranges in a buffer.

    Invariants: each range must be disjoint, ranges must be ordered. *)
type t = Range.t list

(** Test invariants. Returns true on success. *)
val invariants : t -> bool

val of_opt_range : Opt_range.t -> t

(** Convert from a (possibly unordered and overlapping) list of ranges. *)
val of_list : Range.t list -> t

(** Convert from a (possibly overlapping) set of ranges. *)
val of_set : (Range.t, _) Base.Set.t -> t

(** Are the two sets of ranges equal? *)
val ( = ) : t -> t -> bool

(** Union two sets of ranges. *)
val union : t -> t -> t

(** Is the first entirely contained in the second? *)
val is_subset : t -> t -> bool

(** The intersection of two ranges. *)
val intersect : t -> t -> t

(** Pretty-print this range. *)
val pp : t Fmt.t

type string_status =
  | Covered of Range.t
  | Uncovered of Range.t

(** Mark all string segments as either covered or uncovered. *)
val mark_string : t -> string -> string_status list
