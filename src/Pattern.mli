(** Patterns match terms and bind variables. *)

type 'a pattern =
  | Operator of 'a * string * 'a pattern list list
  | Primitive of 'a * Primitive.t
  | Var of 'a * string
  | Ignored of 'a * string

type 'a t = 'a pattern

val vars_of_pattern : 'a t -> Util.String.Set.t
val list_vars_of_pattern : 'a t -> string list
val to_string : 'a t -> string
val pp : Format.formatter -> 'a t -> unit
val jsonify : 'a t -> Util.Json.t
val unjsonify : Util.Json.t -> unit t option
val erase : 'a pattern -> unit pattern
val location : 'a pattern -> 'a

module Parse (Comment : Util.Angstrom.Comment_int) : sig
  val t : Position.t t Angstrom.t
end

module Properties : sig
  val json_round_trip1 : unit t -> bool
  val json_round_trip2 : Util.Json.t -> bool
  val string_round_trip1 : unit t -> bool
  val string_round_trip2 : string -> bool
end
