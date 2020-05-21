(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)

type term =
  | Operator of string * term list
  | Primitive of Primitive.t

(** Raised by [from_de_bruijn_exn] or [from_nominal_exn] when they encounter a scope.
 *)
exception ScopeEncountered

(** {1 de Bruijn conversion} *)

(** @raise ScopeEncountered *)
val from_de_bruijn_exn : Binding.DeBruijn.term -> term
val from_de_bruijn : Binding.DeBruijn.term -> term option
val to_de_bruijn : term -> Binding.DeBruijn.term

(** {1 Nominal conversion} *)

(** @raise ScopeEncountered *)
val from_nominal_exn : Binding.Nominal.term -> term
val from_nominal : Binding.Nominal.term -> term option
val to_nominal : term -> Binding.Nominal.term

(** {1 Printing} *)

val pp : Format.formatter -> term -> unit
val to_string : term -> string
