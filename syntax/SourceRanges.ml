open Base

type t = Ranges.t Lvca_util.String.Map.t
type Caml.Format.stag += Stag of t

let to_string t =
  t
  |> Map.to_alist
  |> List.map ~f:(fun (buf, ranges) ->
         Caml.Printf.sprintf "%s:%s" buf (Ranges.to_string ranges))
  |> String.concat ~sep:","
;;

let pp' ppf (buf, ranges) = Fmt.pf ppf "%s:%a" buf Ranges.pp ranges
let pp ppf t = Fmt.pf ppf "%a" (Fmt.list ~sep:(Fmt.any ",") pp') (Map.to_alist t)
let invariants t = Map.for_all t ~f:Ranges.invariants
let ( = ) x y = Map.equal Ranges.( = ) x y

let union x y =
  Map.merge x y ~f:(fun ~key:_ -> function
    | `Left v | `Right v -> Some v | `Both (v1, v2) -> Some (Ranges.union v1 v2))
;;

let empty = Lvca_util.String.Map.empty
let unions lst = Base.List.fold lst ~f:union ~init:empty
let mk buf p1 p2 = Lvca_util.String.Map.singleton buf (Ranges.of_list [ Range.mk p1 p2 ])
let of_range ~buf Range.{ start; finish } = mk buf start finish
let of_opt_range ~buf = function Some range -> of_range ~buf range | None -> empty

let is_subset p1 p2 =
  Map.for_alli p1 ~f:(fun ~key ~data:r1 ->
      match Map.find p2 key with None -> false | Some r2 -> Ranges.is_subset r1 r2)
;;

let intersect p1 p2 = Lvca_util.String.Map.intersect p1 p2 ~f:Ranges.intersect

let stag_functions =
  Caml.Format.
    { mark_open_stag =
        (function Stag t -> Caml.Printf.sprintf "<%s>" (to_string t) | _ -> "")
    ; mark_close_stag =
        (function Stag t -> Caml.Printf.sprintf "</%s>" (to_string t) | _ -> "")
    ; print_open_stag = (fun _ -> ())
    ; print_close_stag = (fun _ -> ())
    }
;;
