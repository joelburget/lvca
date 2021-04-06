type 'info t =
  | PrimInteger of 'info * Z.t
  | PrimString of 'info * string
  | PrimFloat of 'info * float
  | PrimChar of 'info * char

module Plain : sig
  type t =
    | PrimInteger of Z.t
    | PrimString of string
    | PrimFloat of float
    | PrimChar of char
end

val to_plain : _ t -> Plain.t
val of_plain : Plain.t -> unit t
val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase : _ t -> unit t
val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val pp : _ t Fmt.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : OptRange.t t ParseUtil.t
end

val check : 'info -> _ t -> 'info Sort.t -> string option
val jsonify : _ t -> Lvca_util.Json.t
val unjsonify : Lvca_util.Json.t -> unit t option

module Properties : Properties_intf.S with type 'info t := 'info t

module Integer : sig
  module Plain_typedef : sig
    type t = Z.t
  end

  include
    LanguageObject_intf.S with type 'info t = 'info * Z.t and module Plain = Plain_typedef
end

module Int : sig
  module Plain_typedef : sig
    type t = int
  end

  include
    LanguageObject_intf.S with type 'info t = 'info * int and module Plain = Plain_typedef
end

module String : sig
  module Plain_typedef : sig
    type t = string
  end

  include
    LanguageObject_intf.S
      with type 'info t = 'info * string
       and module Plain = Plain_typedef
end
