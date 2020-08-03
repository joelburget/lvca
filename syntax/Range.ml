type t =
  { start : int
  ; finish : int
  }

let mk start finish = { start; finish }

let to_string : t -> string
  = fun { start; finish } -> Printf.sprintf "%i-%i" start finish
