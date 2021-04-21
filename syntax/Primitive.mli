(* TODO: potentially add (u)int8, (u)int64, etc via stdint. Downside: "integers
   smaller than the standard integer type are stored in a standard int." Or
   possibly using bitvec? (https://stackoverflow.com/a/65080349) *)
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

module Int32 : sig
  module Plain_typedef : sig
    type t = int32
  end

  include
    LanguageObject_intf.S
      with type 'info t = 'info * int32
       and module Plain = Plain_typedef
end

module Float : sig
  module Plain_typedef : sig
    type t = float
  end

  include
    LanguageObject_intf.S
      with type 'info t = 'info * float
       and module Plain = Plain_typedef
end

module Char : sig
  module Plain_typedef : sig
    type t = char
  end

  include
    LanguageObject_intf.S
      with type 'info t = 'info * char
       and module Plain = Plain_typedef
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

module Plain : sig
  type t =
    | Integer of Z.t
    | String of string
    | Float of float
    | Char of char
end

type 'info t = 'info * Plain.t

val to_plain : _ t -> Plain.t
val of_plain : Plain.t -> unit t
val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
val info : 'info t -> 'info
val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase : _ t -> unit t
val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val pp : _ t Fmt.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : OptRange.t t ParseUtil.t
end

val check : _ t -> 'info Sort.t -> string option
val jsonify : _ t -> Lvca_util.Json.t
val unjsonify : Lvca_util.Json.t -> unit t option

module Properties : Properties_intf.S with type 'info t := 'info t
