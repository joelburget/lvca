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
val jsonify : t -> Util.Json.t
val unjsonify : Util.Json.t -> t option

module Properties : sig
  val round_trip1 : t -> bool
  val round_trip2 : Util.Json.t -> bool
end
