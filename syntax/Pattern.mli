(** Patterns match terms and bind variables. *)

type 'a pattern =
  | Operator of 'a * string * 'a pattern list list
  | Primitive of 'a * Primitive.t
  | Var of 'a * string
  | Ignored of 'a * string

type 'a t = 'a pattern

val vars_of_pattern : 'a t -> Lvca_util.String.Set.t
val list_vars_of_pattern : 'a t -> ('a * string) list
val to_string : 'a t -> string
val pp : 'a t Fmt.t
val pp_range : OptRange.t t Fmt.t
val jsonify : 'a t -> Lvca_util.Json.t
val unjsonify : Lvca_util.Json.t -> unit t option
val erase : 'a pattern -> unit pattern
val location : 'a pattern -> 'a

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : OptRange.t t ParseUtil.t
end

module Properties : sig
  val json_round_trip1 : unit t -> bool
  val json_round_trip2 : Lvca_util.Json.t -> bool
  val string_round_trip1 : unit t -> bool
  val string_round_trip2 : string -> bool
end
