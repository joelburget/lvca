type t = Range.t option

let mk start finish = Some (Range.mk start finish)

let extend_to opt_rng pos =
  match opt_rng with None -> None | Some rng -> Some (Range.extend_to rng pos)
;;

let union a b =
  match a, b with
  | Some a', Some b' -> Some (Range.union a' b')
  | Some a, None | None, Some a -> Some a
  | None, None -> None
;;

let list_range = Base.List.fold ~init:None ~f:union

let ( = ) a b =
  match a, b with
  | Some a', Some b' -> Range.(a' = b')
  | Some _, None | None, Some _ -> false
  | None, None -> true
;;

let is_before a b =
  match a, b with Some a', Some b' -> Range.is_before a' b' | _, _ -> false
;;

let is_subset a b =
  match a, b with Some a', Some b' -> Range.is_subset a' b' | _, _ -> false
;;

let intersect a b =
  match a, b with Some a', Some b' -> Range.intersect a' b' | _, _ -> None
;;

let pp ppf rng_opt =
  match rng_opt with Some rng -> Range.pp ppf rng | None -> Fmt.pf ppf "_"
;;

let open_stag ppf = function
  | Some rng -> Format.pp_open_stag ppf (Range.Stag rng)
  | None -> ()
;;

let close_stag ppf = function Some _rng -> Format.pp_close_stag ppf () | None -> ()
