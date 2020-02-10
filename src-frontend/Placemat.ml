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

  type ('a, 'b) t = ('a, 'b) Belt.MutableSet.t
  let from_array = Belt.MutableSet.fromArray
  let to_array = Belt.MutableSet.toArray
  let to_list = Belt.MutableSet.toList
  let copy = Belt.MutableSet.copy
  let is_empty = Belt.MutableSet.isEmpty
  let for_each = Belt.MutableSet.forEach
  let has = Belt.MutableSet.has
  let add = Belt.MutableSet.add
  let make = Belt.MutableSet.make
end

module MutableStack = Belt.MutableStack
module MutableQueue = Belt.MutableQueue
module Id = Belt.Id

module IntDict = struct
  let remove = Belt.Map.Int.remove

  let has = Belt.Map.Int.has

  let size = Belt.Map.Int.size

  let forEach = Belt.Map.Int.forEach
  let findFirstBy = Belt.Map.Int.findFirstBy

  let from_array = Belt.Map.Int.fromArray
  let to_array = Belt.Map.Int.toArray
  let map_with_key = Belt.Map.Int.mapWithKey
  let cmp = Belt.Map.Int.cmp

  let values_to_array dict = dict
    |> to_array
    |> Tablecloth.Array.map ~f:(fun (_, v) -> v)
end

module StrDict = struct
  let remove = Belt.Map.String.remove

  let remove_many = Belt.Map.String.removeMany

  let has = Belt.Map.String.has

  let size = Belt.Map.String.size
  let forEach = Belt.Map.String.forEach
  let from_array = Belt.Map.String.fromArray
  let to_array = Belt.Map.String.toArray
end

module IntSet = struct
  type t = Belt.Set.Int.t
  let eq = Belt.Set.Int.eq
  let size = Belt.Set.Int.size
  let forEach = Belt.Set.Int.forEach
  let cmp = Belt.Set.Int.cmp
  let intersect = Belt.Set.Int.intersect
  let from_array = Belt.Set.Int.fromArray
  let to_array = Belt.Set.Int.toArray
end

module Set = struct
  type ('a, 'b) t = ('a, 'b) Belt.Set.t
  let for_each = Belt.Set.forEach
  let to_array = Belt.Set.toArray
  let from_array = Belt.Set.fromArray
  let union = Belt.Set.union
  let cmp = Belt.Set.cmp
  let eq = Belt.Set.eq
end

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Belt.Result.t
end
