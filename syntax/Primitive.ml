let mk_error tm =
  Error (Nominal.Conversion_error.mk_Term ~provenance:(Provenance.of_here [%here]) tm)
;;

module Integer_kernel = struct
  include Primitive_impl.Integer

  let to_nominal (info, z) = Nominal.Term.Primitive (info, Integer z)

  let of_nominal tm =
    match tm with
    | Nominal.Term.Primitive (info, Integer z) -> Ok (info, z)
    | _ -> mk_error tm
  ;;
end

module Integer = Nominal.Convertible.Extend (Integer_kernel)

module Int32_kernel = struct
  include Primitive_impl.Int32

  let to_nominal (info, z) = Nominal.Term.Primitive (info, Int32 z)

  let of_nominal tm =
    match tm with
    | Nominal.Term.Primitive (info, Int32 z) -> Ok (info, z)
    | _ -> mk_error tm
  ;;
end

module Int32 = Nominal.Convertible.Extend (Int32_kernel)

module Float_kernel = struct
  include Primitive_impl.Float

  let to_nominal (info, f) = Nominal.Term.Primitive (info, Float f)

  let of_nominal tm =
    match tm with
    | Nominal.Term.Primitive (info, Float f) -> Ok (info, f)
    | _ -> mk_error tm
  ;;
end

module Float = Nominal.Convertible.Extend (Float_kernel)

module Char_kernel = struct
  include Primitive_impl.Char

  let to_nominal (info, x) = Nominal.Term.Primitive (info, Char x)

  let of_nominal tm =
    match tm with
    | Nominal.Term.Primitive (info, Char x) -> Ok (info, x)
    | _ -> mk_error tm
  ;;
end

module Char = Nominal.Convertible.Extend (Char_kernel)

module String_kernel = struct
  include Primitive_impl.String

  let to_nominal (info, x) = Nominal.Term.Primitive (info, String x)

  let of_nominal tm =
    match tm with
    | Nominal.Term.Primitive (info, String x) -> Ok (info, x)
    | _ -> mk_error tm
  ;;
end

module String = Nominal.Convertible.Extend (String_kernel)

module All_kernel = struct
  include Primitive_impl.All

  let to_nominal (info, x) = Nominal.Term.Primitive (info, x)

  let of_nominal tm =
    match tm with Nominal.Term.Primitive (info, x) -> Ok (info, x) | _ -> mk_error tm
  ;;
end

module All = struct
  include Nominal.Convertible.Extend (All_kernel)
  module Properties = Primitive_impl.All.Properties

  let check = Primitive_impl.All.check
end
