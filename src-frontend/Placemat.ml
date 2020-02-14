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
  type 'a t = 'a array

  let unzip : ('a * 'b) t -> 'a t * 'b t
    = Belt.Array.unzip

  let map_with_index : f:(int -> 'a -> 'b) -> 'a t -> 'b t
    = fun ~f arr -> Belt.Array.mapWithIndex arr f

  let filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    = fun arr ~f ->
    let result = [||] in
    Tablecloth.Array.for_each arr ~f:(fun a ->
      match f a with
      | None -> ()
      | Some b ->
        let _ = Js.Array2.push result b in
        ());
    result
end

module MutableMap = struct
  module Int = struct
    type 'a t = 'a Belt.MutableMap.Int.t

    let make : unit -> 'a t
      = Belt.MutableMap.Int.make

    let get : 'a t -> int -> 'a option
      = Belt.MutableMap.Int.get

    let get_exn : 'a t -> int -> 'a
      = Belt.MutableMap.Int.getExn

    let has : 'a t -> int -> bool
      = Belt.MutableMap.Int.has

    let set : 'a t -> int -> 'a -> unit
      = Belt.MutableMap.Int.set

    let remove : 'a t -> int -> unit
      = Belt.MutableMap.Int.remove

    let to_list : 'a t -> (int * 'a) list
      = Belt.MutableMap.Int.toList

    let to_array : 'a t -> (int * 'a) array
      = Belt.MutableMap.Int.toArray

    let keys_to_array : 'a t -> int array
      = Belt.MutableMap.Int.keysToArray

    let from_array : (int * 'a) array -> 'a t
      = Belt.MutableMap.Int.fromArray
  end
end

module MutableSet = struct
  module Int = struct
    type t = Belt.MutableSet.Int.t

    let copy : t -> t
      = Belt.MutableSet.Int.copy

    let is_empty : t -> bool
      = Belt.MutableSet.Int.isEmpty

    let minimum : t -> int option
      = Belt.MutableSet.Int.minimum

    let remove : t -> int -> unit
      = Belt.MutableSet.Int.remove

    let add : t -> int -> unit
      = Belt.MutableSet.Int.add

    let make : unit -> t
      = Belt.MutableSet.Int.make

    let to_list : t -> int list
      = Belt.MutableSet.Int.toList

    let for_each : t -> (int -> unit) -> unit
      = Belt.MutableSet.Int.forEach

    let from_array : int array -> t
      = Belt.MutableSet.Int.fromArray

    let merge_many : t -> int array -> unit
      = Belt.MutableSet.Int.mergeMany

    let to_array : t -> int array
      = Belt.MutableSet.Int.toArray

    let has : t -> int -> bool
      = Belt.MutableSet.Int.has

    let subset : t -> t -> bool
      = Belt.MutableSet.Int.subset

    let cmp : t -> t -> int
      = Belt.MutableSet.Int.cmp
  end

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

module MutableStack = struct
  type 'a t = 'a Belt.MutableStack.t

  let for_each : 'a t -> ('a -> unit) -> unit
    = Belt.MutableStack.forEach

  let make : unit -> 'a t
    = Belt.MutableStack.make

  let push : 'a t -> 'a -> unit
    = Belt.MutableStack.push

  let top : 'a t -> 'a option
    = Belt.MutableStack.top

  let pop : 'a t -> 'a option
    = Belt.MutableStack.pop

  let size : 'a t -> int
    = Belt.MutableStack.size

  let is_empty : 'a t -> bool
    = Belt.MutableStack.isEmpty

  let to_array : 'a t -> 'a array =
   fun stack ->
   let result = [||] in
   for_each stack (fun item ->
     let _ = Js.Array2.push result item in
     ());
   result
end

module MutableQueue = struct
  type 'a t = 'a Belt.MutableQueue.t

  let dequeue : 'a t -> 'a option
    = Belt.MutableQueue.pop

  let make : unit -> 'a t
    = Belt.MutableQueue.make

  let enqueue : 'a t -> 'a -> unit
    = Belt.MutableQueue.add

  let to_array : 'a t -> 'a array
    = Belt.MutableQueue.toArray

  let from_array : 'a array -> 'a t
    = Belt.MutableQueue.fromArray
end

module Id = Belt.Id

module IntDict = struct
  type 'a t = 'a Belt.Map.Int.t

  let remove : 'v t -> int -> 'v t
    = Belt.Map.Int.remove

  let has : 'v t -> int -> bool
    = Belt.Map.Int.has

  let size : 'v t -> int
    = Belt.Map.Int.size

  let for_each
    : f:(int -> 'a -> unit) -> 'a Belt.Map.Int.t -> unit
    = fun ~f int_map -> Belt.Map.Int.forEach int_map f

  let find_first_by
    : f:(int -> 'v -> bool) -> 'v Belt.Map.Int.t -> (int * 'v) option
    = fun ~f int_map -> Belt.Map.Int.findFirstBy int_map f

  let from_array : (int * 'v) array -> 'v t
    = Belt.Map.Int.fromArray

  let to_array : 'v t -> (int * 'v) array
    = Belt.Map.Int.toArray

  let map_with_key : 'v t -> (int -> 'v -> 'v2) -> 'v2 t
    = Belt.Map.Int.mapWithKey

  let cmp : 'v t -> 'v t -> ('v -> 'v -> int) -> int
    = Belt.Map.Int.cmp

  let values_to_array dict = dict
    |> to_array
    |> Tablecloth.Array.map ~f:(fun (_, v) -> v)
end

module StrDict = struct
  type 'a t = 'a Belt.Map.String.t

  let remove : 'v t -> string -> 'v t
    = Belt.Map.String.remove

  let remove_many : 'v t -> string array -> 'v t
    = Belt.Map.String.removeMany

  let has : 'v t -> string -> bool
    = Belt.Map.String.has

  let size : 'v t -> int
    = Belt.Map.String.size

  let for_each : f:(string -> 'v -> unit) -> 'v t -> unit
    = fun ~f map -> Belt.Map.String.forEach map f

  let from_array : (string * 'v) array -> 'v t
    = Belt.Map.String.fromArray

  let to_array : 'v t -> (string * 'v) array
    = Belt.Map.String.toArray
end

module IntSet = struct
  type t = Belt.Set.Int.t

  let eq : t -> t -> bool
    = Belt.Set.Int.eq

  let size : t -> int
    = Belt.Set.Int.size

  let cmp : t -> t -> int
    = Belt.Set.Int.cmp

  let intersect : t -> t -> t
    = Belt.Set.Int.intersect

  let from_array : int array -> t
    = Belt.Set.Int.fromArray

  let to_array : t -> int array
    = Belt.Set.Int.toArray

  let for_each
    : f:(int -> unit) -> t -> unit
    = fun ~f set -> Belt.Set.Int.forEach set f
end

module Set = struct
  type ('a, 'b) t = ('a, 'b) Belt.Set.t
  type ('value, 'id) id = ('value, 'id) Belt_Id.comparable

  let to_array : ('value, 'id) t -> 'value array
    = Belt.Set.toArray

  let from_array : 'value array -> id:('value, 'id) id -> ('value, 'id) t
    = Belt.Set.fromArray

  let union : ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
    = Belt.Set.union

  let cmp : ('value, 'id) t -> ('value, 'id) t -> int
    = Belt.Set.cmp

  let eq : ('value, 'id) t -> ('value, 'id) t -> bool
    = Belt.Set.eq

  let for_each
    : f:('value -> unit) -> ('value, 'id) t -> unit
    = fun ~f set -> Belt.Set.forEach set f
end

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Belt.Result.t
end

module Bytes = struct
 type t = External.Bytes.t
end

module Json = struct
 type t = Js.Json.t

 let array : t array -> t
   = Js.Json.array

 let string : string -> t
   = Js.Json.string
end

module Sha256 = struct
  let hash : Bytes.t -> string
    = fun bytes -> bytes
      |> External.BitArray.from_u8_array
      |> External.Sha256.hash_ba
end

module Cbor = struct
  let encode : Json.t -> Bytes.t
    = fun json -> json
      |> External.Cbor.encode_ab
      |> External.Bytes.from_array_buffer

  let decode : Bytes.t -> Json.t option
    = fun buf -> buf
      |> External.Bytes.to_array_buffer
      |> External.Cbor.decode_ab
end

module String = struct
  let concat_array : ?sep:string -> string array -> string
    = fun ?sep:(sep="") arr -> Js.Array2.joinWith arr sep

  (** raises [Invalid_argument] *)
  let get : string -> int -> char
    = fun str i -> str
      |. Js.String2.charAt i
      |> Tablecloth.Char.from_string
      |> (function
        | None -> raise (Invalid_argument "index out of bounds")
        | Some c -> c)
end
