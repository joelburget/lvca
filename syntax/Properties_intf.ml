module type Parse_pretty_s = sig
  type 'info t

  val string_round_trip1 : unit t -> Property_result.t
  val string_round_trip2 : string -> Property_result.t
end

module type Json_s = sig
  type 'info t

  val json_round_trip1 : unit t -> Property_result.t
  val json_round_trip2 : Lvca_util.Json.t -> Property_result.t
end

module type S = sig
  type 'info t

  include Parse_pretty_s with type 'info t := 'info t
  include Json_s with type 'info t := 'info t
end
