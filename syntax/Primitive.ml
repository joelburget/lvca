include Primitive_impl

module Integer = struct
  include Primitive_impl.Integer

  let to_nominal (info, z) = Nominal.Term.Primitive (info, Integer z)

  let of_nominal tm =
    match tm with
    | Nominal.Term.Primitive (info, Integer z) -> Ok (info, z)
    | _ -> Error tm
  ;;
end

module Float = struct
  include Primitive_impl.Float

  let to_nominal (info, f) = Nominal.Term.Primitive (info, Float f)

  let of_nominal tm =
    match tm with Nominal.Term.Primitive (info, Float f) -> Ok (info, f) | _ -> Error tm
  ;;
end

module Char = struct
  include Primitive_impl.Char

  let to_nominal (info, x) = Nominal.Term.Primitive (info, Char x)

  let of_nominal tm =
    match tm with Nominal.Term.Primitive (info, Char x) -> Ok (info, x) | _ -> Error tm
  ;;
end

module String = struct
  include Primitive_impl.String

  let to_nominal (info, x) = Nominal.Term.Primitive (info, String x)

  let of_nominal tm =
    match tm with
    | Nominal.Term.Primitive (info, String x) -> Ok (info, x)
    | _ -> Error tm
  ;;
end
