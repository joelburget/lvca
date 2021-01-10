(** A path points to some subterm. At each step we point to a slot and an index within
    that slot. For example [foo(bar(a, b; c))]

    - [\[\]] points to [foo(bar(a, b; c))]
    - [\[0, 0\]] points to [bar(a, b; c)]
    - [\[0, 0; 0, 0\]] points to [a]
    - [\[0, 0; 0, 1\]] points to [b]
    - [\[0, 0; 1, 0\]] points to [c] *)
type t = (int * int) list

type comparator_witness

val comparator : (t, comparator_witness) Base.Comparator.t
val compare : t -> t -> int
val sexp_of_t : t -> Sexplib0.Sexp.t
val ( = ) : t -> t -> bool
val is_prefix : path:t -> prefix:t -> bool
