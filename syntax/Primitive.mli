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

module Properties : Properties_intf.S with type t := t

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
