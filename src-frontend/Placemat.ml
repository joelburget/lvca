(** Sits on top of the tablecloth, but still below everything else *)

module List = struct
  let unzip : ('a * 'b) list -> 'a list * 'b list
    = Belt.List.unzip

  let for_each : f:('a -> 'b) -> 'a list -> unit
    = fun ~f lst -> Belt.List.forEach lst f

  let for_each_with_index : f:(int -> 'a -> 'b) -> 'a list -> unit
    = fun ~f lst -> Belt.List.forEachWithIndex lst f

  let initialize : length:int -> f:(int -> 'a) -> 'a list
    = fun ~length ~f -> Belt.List.makeBy length f

  let map_with_index : f:(int -> 'a -> 'b) -> 'a list -> 'b list
    = fun ~f lst -> Belt.List.mapWithIndex lst f

  let zip_by : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    = fun ~f lst_a lst_b -> Belt.List.zipBy lst_a lst_b f

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

  let map_with_index : f:(int -> 'a -> 'b) -> 'a array -> 'b array
    = fun ~f arr -> Belt.Array.mapWithIndex arr f
end

module MutableMap = struct
  module Int = Belt.MutableMap.Int
  module String = Belt.MutableMap.String
end

module MutableSet = struct
  module Int = Belt.MutableSet.Int
  module String = Belt.MutableSet.String

  type ('a, 'b) t = ('a, 'b) Belt.MutableSet.t
  type ('k, 'id) id = ('k, 'id) Belt_Id.comparable

  let from_array : 'k array -> id:('k, 'id) id -> ('k, 'id) t
    = Belt.MutableSet.fromArray

  let to_array : ('value, 'id) t -> 'value array
    = Belt.MutableSet.toArray

  let to_list : ('value, 'id) t -> 'value list
    = Belt.MutableSet.toList

  let copy : ('value, 'id) t -> ('value, 'id) t
    = Belt.MutableSet.copy

  let is_empty : ('value, 'id) t -> bool
    = Belt.MutableSet.isEmpty

  let for_each : f:('value -> unit) -> ('value, 'id) t -> unit
    = fun ~f set -> Belt.MutableSet.forEach set f

  let has : value:'value -> ('value, 'a) t -> bool
    = fun ~value set -> Belt.MutableSet.has set value

  let add : value:'value -> ('value, 'id) t -> unit
    = fun ~value set -> Belt.MutableSet.add set value

  let make : id:('value, 'id) id -> ('value, 'id) t
    = Belt.MutableSet.make
end

module MutableStack = Belt.MutableStack
module MutableQueue = Belt.MutableQueue
module Id = Belt.Id

module IntDict = struct
  let remove = Belt.Map.Int.remove

  let has = Belt.Map.Int.has

  let size = Belt.Map.Int.size

  let for_each
    : f:(int -> 'a -> unit) -> 'a Belt.Map.Int.t -> unit
    = fun ~f int_map -> Belt.Map.Int.forEach int_map f

  let find_first_by
    : f:(int -> 'v -> bool) -> 'v Belt.Map.Int.t -> (int * 'v) option
    = fun ~f int_map -> Belt.Map.Int.findFirstBy int_map f

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

  let for_each : f:(string -> 'v -> unit) -> 'v Belt.Map.String.t -> unit
    = fun ~f map -> Belt.Map.String.forEach map f

  let from_array = Belt.Map.String.fromArray
  let to_array = Belt.Map.String.toArray
end

module IntSet = struct
  type t = Belt.Set.Int.t
  let eq = Belt.Set.Int.eq
  let size = Belt.Set.Int.size
  let cmp = Belt.Set.Int.cmp
  let intersect = Belt.Set.Int.intersect
  let from_array = Belt.Set.Int.fromArray
  let to_array = Belt.Set.Int.toArray

  let for_each
    : f:(int -> unit) -> t -> unit
    = fun ~f set -> Belt.Set.Int.forEach set f
end

module Set = struct
  type ('a, 'b) t = ('a, 'b) Belt.Set.t
  let to_array = Belt.Set.toArray
  let from_array = Belt.Set.fromArray
  let union = Belt.Set.union
  let cmp = Belt.Set.cmp
  let eq = Belt.Set.eq

  let for_each
    : f:('value -> unit) -> ('value, 'id) t -> unit
    = fun ~f set -> Belt.Set.forEach set f
end

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Belt.Result.t
end
