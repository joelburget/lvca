open Base

module T = struct
  type t = (int * int) list
  let compare = List.compare Lvca_util.Tuple2.Int.compare
  let sexp_of_t = List.sexp_of_t Lvca_util.Tuple2.Int.sexp_of_t
end

include T
include Comparable.Make(T)

let is_prefix ~path ~prefix = List.is_prefix path ~prefix ~equal:Lvca_util.Tuple2.Int.(=)
