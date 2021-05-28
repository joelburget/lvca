open Lvca_provenance

type 'a t =
  { latest_pos : int
  ; value : 'a
  ; range : OptRange.t
  }

let equal a_eq r1 r2 =
  Base.Int.(r1.latest_pos = r2.latest_pos)
  && a_eq r1.value r2.value
  && OptRange.(r1.range = r2.range)
;;

let pp pp_a ppf { value; range; latest_pos } =
  Fmt.pf
    ppf
    "{ value = %a; range = %a; latest_pos = %d }"
    pp_a
    value
    OptRange.pp
    range
    latest_pos
;;
