(** A path (sometimes called "occurrence") points to some subterm. At each step we point
    to a subterm. For example [foo(a; bar(b; c))].

    - [\[\]] points to [foo(a; bar(b; c))]
    - [\[0\]] points to [a]
    - [\[1; 0\]] points to [b]
    - [\[1; 1\]] points to [c] *)
type t = int list

type comparator_witness

val comparator : (t, comparator_witness) Base.Comparator.t
val compare : t -> t -> int
val sexp_of_t : t -> Sexplib0.Sexp.t
val ( = ) : t -> t -> bool
val is_prefix : path:t -> prefix:t -> bool
