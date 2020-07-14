(** Patterns match terms and bind variables. *)

type pattern =
  | Operator of string * pattern list list
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

module Parse (Comment : Util.Angstrom.Comment_int) : sig
  val t : t Angstrom.t
end

module Properties : sig
  val json_round_trip1 : t -> bool
  val json_round_trip2 : Util.Json.t -> bool
  val string_round_trip1 : t -> bool
  val string_round_trip2 : string -> bool
end
