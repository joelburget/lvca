open Base
open Lvca_util

type t = Ranges.t String.Map.t
type Stdlib.Format.stag += Stag of t

let pp' ppf (buf, ranges) = Fmt.pf ppf "%s:%a" buf Ranges.pp ranges
let pp ppf t = Fmt.(list ~sep:(any ",") pp') ppf (Map.to_alist t)
let invariants t = Map.for_all t ~f:Ranges.invariants
let ( = ) x y = Map.equal Ranges.( = ) x y

let union x y =
  Map.merge x y ~f:(fun ~key:_ -> function
    | `Left v | `Right v -> Some v | `Both (v1, v2) -> Some (Ranges.union v1 v2))
;;

let empty = String.Map.empty
let unions lst = Base.List.fold lst ~f:union ~init:empty
let mk buf p1 p2 = String.Map.singleton buf (Ranges.of_list [ Range.mk p1 p2 ])
let of_range ~buf Range.{ start; finish } = mk buf start finish
let of_source_range Source_range.{ source; range } = of_range ~buf:source range
let of_opt_range ~buf = function Some range -> of_range ~buf range | None -> empty

let is_subset p1 p2 =
  Map.for_alli p1 ~f:(fun ~key ~data:r1 ->
      match Map.find p2 key with None -> false | Some r2 -> Ranges.is_subset r1 r2)
;;

let intersect p1 p2 = String.Map.intersect p1 p2 ~f:Ranges.intersect

let restrict ~buf p =
  match Map.find p buf with
  | None -> empty
  | Some ranges -> String.Map.singleton buf ranges
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

let%test_module "Range" =
  (module struct
    let%expect_test _ =
      Fmt.pr "%a" pp (unions []);
      [%expect {| |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp (unions [ mk "input" 16 17 ]);
      [%expect {| input:{16,17} |}]
    ;;
  end)
;;
