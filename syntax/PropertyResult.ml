type t =
  | Ok
  | Uninteresting
  | Failed of string

let check b msg = if b then Ok else Failed msg
