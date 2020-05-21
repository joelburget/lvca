(** Patterns match terms and bind variables. *)

type pattern =
  | Operator of string * pattern list
  | Primitive of Primitive.t
  | Var of string
  | Ignored of string

type t = pattern

val vars_of_pattern : t -> Util.String.Set.t
val list_vars_of_pattern : t -> string list
val to_string : t -> string
val pp : Format.formatter -> t -> unit