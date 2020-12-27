open Base

module Tuple2 = struct
  let compare (x1, y1) (x2, y2) =
    let c1 = Int.compare x1 x2 in
    if c1 <> 0 then c1 else Int.compare y1 y2

  let sexp_of_t (x, y) = Sexplib0.Sexp.List [Int.sexp_of_t x; Int.sexp_of_t y]

  let (=) (x1, y1) (x2, y2) = Int.(x1 = x2) && Int.(y1 = y2)
end

module T = struct
  type t = (int * int) list
  let compare = List.compare Tuple2.compare
  let sexp_of_t = List.sexp_of_t Tuple2.sexp_of_t
end

include T
include Comparable.Make(T)

let is_prefix ~path ~prefix = List.is_prefix path ~prefix ~equal:Tuple2.(=)
