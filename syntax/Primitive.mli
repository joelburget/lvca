(* TODO: potentially add (u)int8, (u)int64, etc via stdint. Downside: "integers
   smaller than the standard integer type are stored in a standard int." Or
   possibly using bitvec? (https://stackoverflow.com/a/65080349) *)
module Integer :
  Nominal.Convertible.Extended_s
    with type 'info t = 'info * Z.t
     and module Plain = Primitive_impl.Integer.Plain

module Int32 :
  Nominal.Convertible.Extended_s
    with type 'info t = 'info * int32
     and module Plain = Primitive_impl.Int32.Plain

module Float :
  Nominal.Convertible.Extended_s
    with type 'info t = 'info * float
     and module Plain = Primitive_impl.Float.Plain

module Char :
  Nominal.Convertible.Extended_s
    with type 'info t = 'info * char
     and module Plain = Primitive_impl.Char.Plain

module String :
  Nominal.Convertible.Extended_s
    with type 'info t = 'info * string
     and module Plain = Primitive_impl.String.Plain

module All : sig
  include
    Nominal.Convertible.Extended_s
      with type 'info t = 'info * Primitive_impl.All.Plain.t
       and module Plain = Primitive_impl.All.Plain

  val check : _ t -> 'info Sort.t -> string option

  module Properties : sig
    include Properties_intf.Parse_pretty_s with type 'info t := 'info t
    include Properties_intf.Json_s with type 'info t := 'info t
  end
end
