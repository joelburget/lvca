(* TODO: potentially add (u)int8, (u)int64, etc via stdint. Downside: "integers
   smaller than the standard integer type are stored in a standard int." Or
   possibly using bitvec? (https://stackoverflow.com/a/65080349) *)
module Integer : sig
  module Plain : sig
    type t = Z.t
  end

  include
    Nominal.Convertible.Extended_s
      with type 'info t = 'info * Z.t
       and module Plain := Plain
end

module Int32 : sig
  module Plain : sig
    type t = int32
  end

  include
    Nominal.Convertible.Extended_s
      with type 'info t = 'info * int32
       and module Plain := Plain
end

module Float : sig
  module Plain : sig
    type t = float
  end

  include
    Nominal.Convertible.Extended_s
      with type 'info t = 'info * float
       and module Plain := Plain
end

module Char : sig
  module Plain : sig
    type t = char
  end

  include
    Nominal.Convertible.Extended_s
      with type 'info t = 'info * char
       and module Plain := Plain
end

module String : sig
  module Plain : sig
    type t = string
  end

  include
    Nominal.Convertible.Extended_s
      with type 'info t = 'info * string
       and module Plain := Plain
end

module All : sig
  module Plain : sig
    type t = Primitive_impl.Plain.t
    (* TODO: would be nice to include this but I can't get the equality to work out.
      | Integer of Z.t
      | Int32 of int32
      | String of string
      | Float of float
      | Char of char
      *)
  end

  include
    Nominal.Convertible.Extended_s
      with type 'info t = 'info * Plain.t
       and module Plain := Plain

  val check : _ t -> 'info Sort.t -> string option

  module Properties : sig
    include Properties_intf.Parse_pretty_s with type 'info t := 'info t
    include Properties_intf.Json_s with type 'info t := 'info t
  end
end
