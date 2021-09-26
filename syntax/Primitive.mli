(* TODO: potentially add (u)int8, (u)int64, etc via stdint. Downside: "integers
   smaller than the standard integer type are stored in a standard int." Or
   possibly using bitvec? (https://stackoverflow.com/a/65080349) *)
module Integer : Nominal.Convertible.Extended_s with type 'info t = 'info * Z.t
module Int32 : Nominal.Convertible.Extended_s with type 'info t = 'info * int32
module Float : Nominal.Convertible.Extended_s with type 'info t = 'info * float
module Char : Nominal.Convertible.Extended_s with type 'info t = 'info * char
module String : Nominal.Convertible.Extended_s with type 'info t = 'info * string

module All : sig
  include
    Nominal.Convertible.Extended_s with type 'info t = 'info * Primitive_impl.All_plain.t

  val check : _ t -> 'info Sort.t -> string option

  module Properties : sig
    include Properties_intf.Parse_pretty_s with type 'info t := 'info t
    include Properties_intf.Json_s with type 'info t := 'info t
  end
end
