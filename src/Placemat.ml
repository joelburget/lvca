(** Sits on top of the tablecloth, but still below everything else *)

module List = struct
  let unzip : ('a * 'b) list -> 'a list * 'b list
    = Belt.List.unzip

  let forEach : 'a list -> ('a -> 'b) -> unit
    = Belt.List.forEach

  let forEachWithIndex : 'a list -> (int -> 'a -> 'b) -> unit
    = Belt.List.forEachWithIndex

  let makeBy : int -> (int -> 'a) -> 'a list
    = Belt.List.makeBy

  let map_with_index : f:(int -> 'a -> 'b) -> 'a list -> 'b list
    = fun ~f lst -> Belt.List.mapWithIndex lst f

  let mapWithIndex = map_with_index

  let zipBy : 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c list
    = Belt.List.zipBy

  let fold_right : f:('a -> 'b -> 'b) -> initial:'b -> 'a list -> 'b
    = fun ~f ~initial ->
    let rec go = function
      | [] -> initial
      | x :: xs -> f x (go xs)
    in go

  let fold_left : f:('b -> 'a -> 'b) -> initial:'b -> 'a list -> 'b
    = fun ~f ~initial ->
    let rec go accum = function
      | [] -> accum
      | x :: xs -> go (f accum x) xs
    in go initial
end

module Array = struct
  let unzip : ('a * 'b) array -> 'a array * 'b array
    = Belt.Array.unzip

  let makeBy : int -> (int -> 'a) -> 'a array
    = Belt.Array.makeBy

  let map_with_index = Belt.Array.mapWithIndex
end

module MutableMap = struct
  module Int = Belt.MutableMap.Int
  module String = Belt.MutableMap.String
end

module MutableSet = struct
  module Int = Belt.MutableSet.Int
  module String = Belt.MutableSet.String
end

module MutableStack = Belt.MutableStack

module IntDict = struct
  let remove = Belt.Map.Int.remove

  let has = Belt.Map.Int.has

  let size = Belt.Map.Int.size

  let forEach = Belt.Map.Int.forEach
  let findFirstBy = Belt.Map.Int.findFirstBy
end

module StrDict = struct
  let remove = Belt.Map.String.remove

  let has = Belt.Map.String.has

  let size = Belt.Map.String.size
end

module IntSet = struct
  let size = Belt.Set.Int.size
  let forEach = Belt.Set.Int.forEach
  let cmp = Belt.Set.Int.cmp
end
