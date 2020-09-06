type t = Range.t option

type Caml.Format.stag += Stag of t

let mk start finish = Some (Range.mk start finish)

let to_string = function
  | Some rng -> Range.to_string rng
  | None -> "_"

let extend_to opt_rng pos = match opt_rng with
  | None -> None
  | Some rng -> Some (Range.extend_to rng pos)

let (<>) a b = match a, b with
  | Some a', Some b' -> Some Range.(a' <> b')
  | Some a, None
  | None, Some a -> Some a
  | None, None -> None

let list_range = Base.List.fold ~init:None ~f:(<>)

let (=) a b = match a, b with
  | Some a', Some b' -> Range.(a' = b')
  | Some _, None
  | None, Some _ -> false
  | None, None -> true

let (<) a b = match a, b with
  | Some a', Some b' -> Range.(a' < b')
  | _, _ -> false

let intersect a b = match a, b with
  | Some a', Some b' -> Range.intersect a' b'
  | _, _ -> None

let pp = fun ppf rng_opt -> match rng_opt with
  | Some rng -> Fmt.pf ppf "%a" Range.pp rng
  | None -> Fmt.pf ppf "_"
