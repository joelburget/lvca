type 'comment t =
  { comment : 'comment option
  ; range : Opt_range.t
  }

let none = { comment = None; range = None }
let get_range { range; _ } = range
let get_comment { comment; _ } = comment

let equal f t1 t2 =
  Base.Option.equal f t1.comment t2.comment && Opt_range.(t1.range = t2.range)
;;

let pp pp_comment =
  let open Fmt in
  record
    [ field "comment" get_comment (option pp_comment)
    ; field "range" get_range Opt_range.pp
    ]
;;

let of_opt_range range = { comment = None; range }

module Properties = struct
  let round_trip range =
    let range' = range |> of_opt_range |> get_range in
    Lvca_util.Property_result.check
      Opt_range.(range' = range)
      (Fmt.str "%a <> %a" Opt_range.pp range' Opt_range.pp range)
  ;;
end
