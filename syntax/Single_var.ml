type 'info t =
  { info : 'info
  ; name : string
  }

let map_info ~f { info; name } = { info = f info; name }

module Plain = struct
  type t = { name : string }
end

let to_plain { name; _ } = Plain.{ name }
let of_plain Plain.{ name } = { name; info = () }

let to_nominal { info; name } =
  Nominal.Term.Primitive (info, Primitive_impl.All_plain.String name)
;;

let of_nominal = function
  | Nominal.Term.Primitive (info, Primitive_impl.All_plain.String name) ->
    Ok { info; name }
  | tm -> Error tm
;;

let mk ~info name = { info; name }
