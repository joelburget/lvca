open Base

module T = struct
  type t = int list

  let compare = List.compare Int.compare
  let sexp_of_t = List.sexp_of_t Int.sexp_of_t
end

include T
include Comparable.Make (T)

let empty = []
let is_prefix ~path ~prefix = List.is_prefix path ~prefix ~equal:Int.( = )
let pp = Fmt.(list int ~sep:(any "."))
