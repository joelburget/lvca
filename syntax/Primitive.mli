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

module Integer : sig
  type 'info t

  module Plain : sig
    type t = Z.t
  end

  val to_plain : _ t -> Plain.t
  val of_plain : Plain.t -> unit t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

module Int : sig
  type 'info t

  module Plain : sig
    type t = int
  end

  val to_plain : _ t -> Plain.t
  val of_plain : Plain.t -> unit t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

module String : sig
  type 'info t

  module Plain : sig
    type t = string
  end

  val to_plain : _ t -> Plain.t
  val of_plain : Plain.t -> unit t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end
