type t =
  | PrimInteger of Z.t
  | PrimString of string
  | PrimFloat of float
  | PrimChar of char

val ( = ) : t -> t -> bool
val pp : t Fmt.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : t ParseUtil.t
end

val check : 'info -> t -> 'info Sort.t -> string option
val jsonify : t -> Lvca_util.Json.t
val unjsonify : Lvca_util.Json.t -> t option

module Properties : sig
  val json_round_trip1 : t -> PropertyResult.t
  val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
  val string_round_trip1 : t -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end
