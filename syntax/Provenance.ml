open Base
open Lvca_provenance

module Located = struct
  type t =
    | Source_located of Source_code_position.t
    | Parse_located of Opt_range.t

  let ( = ) a b =
    match a, b with
    | Source_located a, Source_located b -> Source_code_position.(a = b)
    | Parse_located a, Parse_located b -> Opt_range.(a = b)
    | _, _ -> false
  ;;

  let pp ppf = function
    | Source_located { pos_fname; pos_lnum; pos_cnum; _ } ->
      Fmt.pf ppf "%s:%d:%d" pos_fname pos_lnum pos_cnum
    | Parse_located opt_range -> Opt_range.pp ppf opt_range
  ;;
end

(* A term is either written directly or computed from others *)
type t =
  [ `Located of Located.t
  | `Calculated of Located.t * t list
  ]

type Stdlib.Format.stag += Stag of t

let open_stag ppf rng = Stdlib.Format.pp_open_stag ppf (Stag rng)
let close_stag ppf _ = Stdlib.Format.pp_close_stag ppf ()
let calculated_here here provs = `Calculated (Located.Source_located here, provs)
let of_here here = `Located (Located.Source_located here)
let of_range opt_range = `Located (Located.Parse_located opt_range)

let rec ( = ) a b =
  match a, b with
  | `Located a, `Located b -> Located.(a = b)
  | `Calculated (x, xs), `Calculated (y, ys) -> Located.(x = y) && List.equal ( = ) xs ys
  | _, _ -> false
;;

let pp ppf = function
  | `Located located -> Located.pp ppf located
  | `Calculated (located, _) -> Located.pp ppf located
;;

let stag_functions =
  Stdlib.Format.
    { mark_open_stag = (function Stag t -> Fmt.str "<%a>" pp t | _ -> "")
    ; mark_close_stag = (function Stag t -> Fmt.str "</%a>" pp t | _ -> "")
    ; print_open_stag = (fun _ -> ())
    ; print_close_stag = (fun _ -> ())
    }
;;

let make0 f p = Lvca_parsing.make0 (fun ~info -> f ~info:(of_range info)) p
let make1 f p = Lvca_parsing.make1 (fun ~info -> f ~info:(of_range info)) p
let make2 f p1 p2 = Lvca_parsing.make2 (fun ~info -> f ~info:(of_range info)) p1 p2
let make3 f p1 p2 p3 = Lvca_parsing.make3 (fun ~info -> f ~info:(of_range info)) p1 p2 p3

let make4 f p1 p2 p3 p4 =
  Lvca_parsing.make4 (fun ~info -> f ~info:(of_range info)) p1 p2 p3 p4
;;

let make5 f p1 p2 p3 p4 p5 =
  Lvca_parsing.make5 (fun ~info -> f ~info:(of_range info)) p1 p2 p3 p4 p5
;;

let make6 f p1 p2 p3 p4 p5 p6 =
  Lvca_parsing.make6 (fun ~info -> f ~info:(of_range info)) p1 p2 p3 p4 p5 p6
;;
