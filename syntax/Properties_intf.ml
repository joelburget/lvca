module type S = sig
  type t

  val json_round_trip1 : t -> PropertyResult.t
  val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
  val string_round_trip1 : t -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end
