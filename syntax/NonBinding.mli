(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)

type 'a term =
  | Operator of 'a * string * 'a term list list
  | Primitive of 'a * Primitive.t

val location : 'a term -> 'a

(** Raised by [from_de_bruijn_exn] or [from_nominal_exn] when they encounter a scope.
 *)
exception ScopeEncountered

(** {1 de Bruijn conversion} *)

(** @raise ScopeEncountered *)
val from_de_bruijn_exn : 'a Binding.DeBruijn.term -> 'a term
val from_de_bruijn : 'a Binding.DeBruijn.term -> 'a term option
val to_de_bruijn : 'a term -> unit Binding.DeBruijn.term

(** {1 Nominal conversion} *)

(** @raise ScopeEncountered *)
val from_nominal_exn : 'a Binding.Nominal.term -> 'a term
val from_nominal : 'a Binding.Nominal.term -> 'a term option
val to_nominal : 'a term -> 'a Binding.Nominal.term

(** {1 Printing} *)

val pp : Format.formatter -> 'a term -> unit
val pp_range : Format.formatter -> OptRange.t term -> unit
val to_string : 'a term -> string

val hash : 'a term -> string
val erase : 'loc term -> unit term
