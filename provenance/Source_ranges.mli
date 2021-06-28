(** The type of disjoint ranges in a set of buffers.

    Invariants: each range must be disjoint, ranges must be ordered. *)
type t = Ranges.t Lvca_util.String.Map.t

type Stdlib.Format.stag += Stag of t

(** Test invariants. Returns true on success. *)
val invariants : t -> bool

val empty : t
val mk : string -> int -> int -> t
val of_source_range : Source_range.t -> t
val of_range : buf:string -> Range.t -> t
val of_opt_range : buf:string -> Opt_range.t -> t

(* (** Convert from a (possibly unordered and overlapping) list of ranges. *) val of_list
   : Range.t list -> t

   (** Convert from a (possibly overlapping) set of ranges. *) val of_set : (Range.t, _)
   Base.Set.t -> t *)

(** Pretty-print this range. *)
val pp : t Fmt.t

(** Are the two sets of ranges equal *)
val ( = ) : t -> t -> bool

(** Union two sets of ranges. *)
val union : t -> t -> t

val unions : t list -> t

(** Is the first entirely contained in the second? *)
val is_subset : t -> t -> bool

(** The intersection of two ranges *)
val intersect : t -> t -> t

(** Restrict to a given buffer *)
val restrict : buf:string -> t -> t

val open_stag : Format.formatter -> t -> unit
val close_stag : Format.formatter -> t -> unit

(** For testing only: used to enable outputting of the [Stag] semantic tag. *)
val stag_functions : Stdlib.Format.formatter_stag_functions
