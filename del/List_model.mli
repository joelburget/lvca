open Lvca_syntax
module Kernel : [%lvca.abstract_syntax_module_sig "list a := Nil() | Cons(a; list a)"]

type 'a t = 'a Kernel.List.t =
  | Nil of Provenance.t
  | Cons of Provenance.t * 'a * 'a t

val equivalent
  :  (?info_eq:(Lvca_syntax.Provenance.t -> Lvca_syntax.Provenance.t -> bool)
      -> 'a
      -> 'b
      -> bool)
  -> ?info_eq:(Lvca_syntax.Provenance.t -> Lvca_syntax.Provenance.t -> bool)
  -> 'a t
  -> 'b t
  -> bool

val to_nominal : ('a -> Nominal.Term.t) -> 'a t -> Nominal.Term.t

val of_nominal
  :  (Nominal.Term.t -> ('a, Nominal.Conversion_error.t) Result.t)
  -> Nominal.Term.t
  -> ('a t, Nominal.Conversion_error.t) Result.t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val map : f:('a -> 'b) -> 'a t -> 'b t
val extract_vars_from_empty_pattern : Pattern.t -> string list
val make_empty_pattern : (Provenance.t * string) list -> Pattern.t
