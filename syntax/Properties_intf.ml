open Lvca_util

module type Parse_pretty_s = sig
  type t

  val string_round_trip1 : t -> Property_result.t
  val string_round_trip2 : string -> Property_result.t
end

module type Json_s = sig
  type t

  val json_round_trip1 : t -> Property_result.t
  val json_round_trip2 : Lvca_util.Json.t -> Property_result.t
end

module type S = sig
  type t

  include Parse_pretty_s with type t := t
  include Json_s with type t := t
end
