type t =
  { info : Provenance.t
  ; name : string
  }

let equivalent ?(info_eq = fun _ _ -> true) t1 t2 =
  info_eq t1.info t2.info && Base.String.(t1.name = t2.name)
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )
let map_info ~f { info; name } = { info = f info; name }

let to_nominal { info; name } =
  Nominal.Term.Primitive (info, Primitive_impl.All_plain.String name)
;;

let of_nominal = function
  | Nominal.Term.Primitive (info, Primitive_impl.All_plain.String name) ->
    Ok { info; name }
  | tm -> Error tm
;;

let mk ?(info = Provenance.of_here [%here]) name = { info; name }
