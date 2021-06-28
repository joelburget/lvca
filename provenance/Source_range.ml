open Base

type t =
  { source : string
  ; range : Range.t
  }

type Stdlib.Format.stag += Stag of t

let mk source p1 p2 = { source; range = Range.mk p1 p2 }
let pp ppf { source; range } = Fmt.pf ppf "%s:%a" source Range.pp range
let extend_to { source; range } pos = { source; range = Range.extend_to range pos }

let union t1 t2 =
  if String.(t1.source = t2.source)
  then Some { source = t1.source; range = Range.union t1.range t2.range }
  else None
;;

let ( = ) p1 p2 = String.(p1.source = p2.source) && Range.(p1.range = p2.range)

let is_before p1 p2 =
  String.(p1.source = p2.source) && Range.(is_before p1.range p2.range)
;;

let is_subset p1 p2 =
  String.(p1.source = p2.source) && Range.(is_subset p1.range p2.range)
;;

let intersect p1 p2 =
  if String.(p1.source = p2.source)
  then (
    match Range.intersect p1.range p2.range with
    | None -> None
    | Some range -> Some { source = p1.source; range })
  else None
;;

let open_stag ppf rng = Stdlib.Format.pp_open_stag ppf (Stag rng)
let close_stag ppf _ = Stdlib.Format.pp_close_stag ppf ()

let stag_functions =
  Stdlib.Format.
    { mark_open_stag = (function Stag t -> Fmt.str "<%a>" pp t | _ -> "")
    ; mark_close_stag = (function Stag t -> Fmt.str "</%a>" pp t | _ -> "")
    ; print_open_stag = (fun _ -> ())
    ; print_close_stag = (fun _ -> ())
    }
;;
