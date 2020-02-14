(** Sits on top of the tablecloth, but still below everything else *)

module List = struct
  let unzip : ('a * 'b) list -> 'a list * 'b list
    = Base.List.unzip

  let for_each : f:('a -> 'b) -> 'a list -> unit
    = fun ~f lst ->
      let _ = Base.List.for_all lst ~f:(fun a -> f a; true)
      in ()

  let for_each_with_index : f:(int -> 'a -> 'b) -> 'a list -> unit
    = fun ~f lst ->
      let _ = Base.List.for_alli lst ~f:(fun i a -> f i a; true)
      in ()

  let initialize : length:int -> f:(int -> 'a) -> 'a list
    = fun ~length ~f -> Core_kernel.List.init length ~f

  let map_with_index : f:(int -> 'a -> 'b) -> 'a list -> 'b list
    = fun ~f lst -> Base.List.mapi lst ~f

  let zip_by : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    = fun ~f lst_a lst_b ->
      let rec go xs ys = match xs, ys with
        | x :: xs, y :: ys -> f x y :: go xs ys
        | _, _ -> []
      in go lst_a lst_b

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
    = Base.Array.unzip

  let map_with_index : f:(int -> 'a -> 'b) -> 'a array -> 'b array
    = fun ~f arr -> Base.Array.mapi arr ~f
end

module MutableMap = struct
  module Int = struct
    type 'a t = (int, 'a) Base.Hashtbl.t

    let make : unit -> 'a t
      = fun () -> Base.Hashtbl.create (module Base.Int)

    let get : 'a t -> key:int -> 'a option
      = fun map ~key -> Base.Hashtbl.find map key

    let get_exn : 'a t -> int -> 'a
      = Base.Hashtbl.find_exn

    let has : 'a t -> int -> bool
      = Base.Hashtbl.mem

    let set : 'a t -> int -> 'a -> unit
      = fun t key data -> let _ = Base.Hashtbl.add t ~key ~data in ()

    let remove : 'a t -> int -> unit
      = Base.Hashtbl.remove

    let to_list : 'a t -> (int * 'a) list
      = Base.Hashtbl.to_alist

    let to_array : 'a t -> (int * 'a) array
      = fun map -> map
        |> to_list
        |> Base.List.to_array

    let keys_to_array : 'a t -> int array
      = fun map -> map
        |> Base.Hashtbl.keys
        |> Base.List.to_array

    let from_array : (int * 'a) array -> 'a t
      = fun kvs -> kvs
        |> Base.Array.fold
          ~init:(Base.Hashtbl.create (module Base.Int))
          ~f:(fun map (key, data) -> Base.Hashtbl.set map ~key ~data; map)

    let%test _ = ([| 0, 1; 1, 2; 0, 3 |]
      |> from_array
      |> get ~key:0) = Some 3

  end
end

module Id = struct

  type ('a, 'id) cmp = 'a -> 'a -> int

  module type Comparable = sig
    type identity
    type t
    val cmp: (t, identity) cmp
  end

  type ('key, 'id) comparable =
    (module Comparable with type identity = 'id and type t = 'key)

  module MakeComparable (M : sig
    type t
    val cmp: t -> t -> int
  end) =
  struct
    type identity
    type t = M.t
    let cmp = M.cmp
  end

  let comparable
    (type key)
    ~cmp
    =
    let module N = MakeComparable(struct
        type t = key
        let cmp = cmp
      end) in
    (module N : Comparable with type t = key)
end

module MutableSet = struct
  module Int = struct
    type t = int Core_kernel.Hash_set.t

    let copy : t -> t
      = Core_kernel.Hash_set.copy

    let is_empty : t -> bool
      = Core_kernel.Hash_set.is_empty

    let minimum : t -> int option
      = fun set -> Core_kernel.Hash_set.min_elt set ~compare:(Base.Int.compare)

    let remove : t -> int -> unit
      = Core_kernel.Hash_set.remove

    let add : t -> int -> unit
      = Core_kernel.Hash_set.add

    let make : unit -> t
      = fun () -> Core_kernel.Hash_set.create (module Base.Int)

    let to_list : t -> int list
      = Core_kernel.Hash_set.to_list

    let for_each : t -> (int -> unit) -> unit
      = fun set f ->
        let _ = Core_kernel.Hash_set.for_all set ~f:(fun i -> f i; true) in ()

    let from_array : int array -> t
      = fun ints -> ints
        |> Base.Array.to_list
        |> Core_kernel.Hash_set.of_list (module Base.Int)

    let merge_many : t -> int array -> unit
      = fun set ints ->
        let _ = Base.Array.for_all ints
          ~f:(fun i -> Core_kernel.Hash_set.add set i; true)
        in ()

    let to_array : t -> int array
      = Core_kernel.Hash_set.to_array

    let has : t -> int -> bool
      = Core_kernel.Hash_set.mem

    let subset : t -> t -> bool
      = fun s1 s2 -> Core_kernel.Hash_set.(length (diff s1 s2) > 0)

    let size : t -> int
      = Core_kernel.Hash_set.length

    let cmp : t -> t -> int
      = fun s1 s2 ->
        let len1, len2 = size s1, size s2 in
        if len1 = len2 then
          let arr1, arr2 = to_array s1, to_array s2 in
          Base.Array.compare (failwith "TODO") arr1 arr2
        else if len1 < len2 then -1 else 1
  end

  module Avltree = Base.Avltree

  type ('value, 'id) t = {
    cmp: ('value, 'id) Id.cmp;
    size: int;
    mutable data: ('value, unit) Avltree.t
  }

  type ('k, 'id) id = ('k, 'id) Id.comparable

  let from_array (type value) (type identity)
    : value array -> id:(value, identity) id -> (value, identity) t
    = fun arr ~id ->
      let module M = (val id) in
      let tree = ref Avltree.empty in
      let size = ref 0 in
      Base.Array.iter arr ~f:(fun value ->
        let added = ref false in
        tree := Avltree.add !tree
          ~compare:M.cmp ~added ~replace:true ~key:value ~data:();
        if !added then incr size;
      );
      { cmp = M.cmp
      ; size = Base.Array.length arr
      ; data = !tree
      }

  let to_array : ('value, 'id) t -> 'value array
    = fun { data; size; _ } ->
      let q = Base.Queue.create ~capacity:size () in
      Avltree.iter data
        ~f:(fun ~key:value ~data:_ -> Base.Queue.enqueue q value);
      Base.Queue.to_array q

  let to_list : ('value, 'id) t -> 'value list
    = fun set -> set |> to_array |> Base.Array.to_list

  let copy : ('value, 'id) t -> ('value, 'id) t
    = fun { cmp; size; data } ->
      let new_table = ref Avltree.empty in
      let added = ref false in
      Avltree.iter data ~f:(fun ~key ~data ->
        new_table := Avltree.add !new_table
          ~compare:cmp ~added ~replace:false ~key ~data
      );
      { cmp; size; data = !new_table }

  let is_empty : ('value, 'id) t -> bool
    = fun { data; _ } -> Avltree.is_empty data

  let for_each : f:('value -> unit) -> ('value, 'id) t -> unit
    = fun ~f { data; _ }
    -> Avltree.iter data ~f:(fun ~key ~data:_ -> f key)

  let has : value:'value -> ('value, 'a) t -> bool
    = fun ~value { cmp; data; _ }
    -> Avltree.mem data ~compare:cmp value

  let add : value:'value -> ('value, 'id) t -> unit
    = fun ~value ({ cmp; data; _ } as set) ->
      let added = ref false in
      let new_table = Avltree.add data ~compare:cmp ~added ~replace:true
        ~key:value ~data:()
      in
      set.data <- new_table

  let make (type value) (type identity)
    : id:(value, identity) id -> (value, identity) t
    = fun ~id ->
      let module M = (val id) in
      { cmp = M.cmp
      ; size = 0
      ; data = Avltree.empty
      }
end

module MutableStack = struct
  type 'a t = 'a Core_kernel.Stack.t

  let for_each : 'a t -> ('a -> unit) -> unit
    = fun stack f ->
      let _ = Core_kernel.Stack.for_all stack ~f:(fun a -> f a; true) in ()

  let make : unit -> 'a t
    = fun () -> Core_kernel.Stack.of_list []

  let push : 'a t -> 'a -> unit
    = Core_kernel.Stack.push

  let top : 'a t -> 'a option
    = Core_kernel.Stack.top

  let pop : 'a t -> 'a option
    = Core_kernel.Stack.pop

  let size : 'a t -> int
    = Core_kernel.Stack.length

  let is_empty : 'a t -> bool
    = Core_kernel.Stack.is_empty
end

module MutableQueue = struct
  type 'a t = 'a Core_kernel.Queue.t

  let dequeue : 'a t -> 'a option
    = Core_kernel.Queue.dequeue

  let make : unit -> 'a t
    = fun () -> Core_kernel.Queue.of_list []

  let enqueue : 'a t -> 'a -> unit
    = Core_kernel.Queue.enqueue

  let to_array : 'a t -> 'a array
    = Core_kernel.Queue.to_array

  let from_array : 'a array -> 'a t
    = Core_kernel.Queue.of_array
end

module IntDict = struct
  type 'a t = 'a Core_kernel.Int.Map.t

  let remove : 'v t -> int -> 'v t
    = Core_kernel.Int.Map.remove

  let has : 'v t -> int -> bool
    = fun map k -> match Core_kernel.Int.Map.find map k with
      | None -> false
      | Some _ -> true

  let size : 'v t -> int
    = Core_kernel.Int.Map.length

  let for_each
    : f:(int -> 'a -> unit) -> 'a t -> unit
    = fun ~f int_map ->
      let _ = Core_kernel.Int.Map.for_alli int_map
        ~f:(fun ~key ~data -> f key data; true)
      in ()

  let find_first_by
    : f:(int -> 'v -> bool) -> 'v t -> (int * 'v) option
    = fun ~f int_map -> int_map
      |> Core_kernel.Int.Map.to_sequence
      |> Base.Sequence.find ~f:(fun (k, v) -> f k v)

  let from_array : (int * 'v) array -> 'v t
    = fun arr -> arr
      |> Base.Array.to_list
      |> Core_kernel.Int.Map.of_alist_reduce
        ~f:Base.Fn.const

  let to_array : 'v t -> (int * 'v) array
    = fun map -> map
      |> Core_kernel.Int.Map.to_alist
      |> Base.Array.of_list

  let map_with_key : 'v t -> (int -> 'v -> 'v2) -> 'v2 t
    = fun map f -> map
      |> Core_kernel.Int.Map.mapi ~f:(fun ~key ~data -> f key data)

  let cmp : 'v t -> 'v t -> ('v -> 'v -> int) -> int
    = fun m1 m2 f -> Core_kernel.Int.Map.compare f m1 m2

  let values_to_array dict = dict
    |> to_array
    |> Tablecloth.Array.map ~f:(fun (_, v) -> v)
end

module StrDict = struct
  type 'a t = 'a Core_kernel.String.Map.t

  let remove : 'v t -> string -> 'v t
    = Core_kernel.String.Map.remove

  let remove_many : 'v t -> string array -> 'v t
    = fun set keys -> Base.Array.fold_right
      keys
      ~init:set
      ~f:(fun key set' -> Core_kernel.String.Map.remove set' key)

  let has : 'v t -> string -> bool
    = fun map k -> match Core_kernel.String.Map.find map k with
      | None -> false
      | Some _ -> true

  let size : 'v t -> int
    = Core_kernel.String.Map.length

  let for_each : f:(string -> 'v -> unit) -> 'v t -> unit
    = fun ~f str_map ->
      let _ = Core_kernel.String.Map.for_alli str_map
        ~f:(fun ~key ~data -> f key data; true)
      in ()

  let from_array : (string * 'v) array -> 'v t
    = fun arr -> arr
      |> Base.Array.to_list
      |> Core_kernel.String.Map.of_alist_reduce
        ~f:Base.Fn.const

  let to_array : 'v t -> (string * 'v) array
    = fun map -> map
      |> Core_kernel.String.Map.to_alist
      |> Base.Array.of_list
end

module IntSet = struct
  type t = Core_kernel.Int.Set.t

  let eq : t -> t -> bool
    = Core_kernel.Int.Set.equal

  let size : t -> int
    = Core_kernel.Int.Set.length

  let cmp : t -> t -> int
    = Core_kernel.Int.Set.compare

  let intersect : t -> t -> t
    = Core_kernel.Int.Set.inter

  let from_array : int array -> t
    = Core_kernel.Int.Set.of_array

  let to_array : t -> int array
    = Core_kernel.Int.Set.to_array

  let for_each
    : f:(int -> unit) -> t -> unit
    = fun ~f set ->
      let _ = Core_kernel.Int.Set.for_all set ~f:(fun str -> f str; true) in ()
end

module Set = struct
  type ('a, 'b) t = ('a, 'b) Core_kernel.Set.t
  type ('value, 'id) id = ('value, 'id) Core_kernel.Set.comparator

  let to_array : ('value, 'id) t -> 'value array
    = Core_kernel.Set.to_array

  let from_array : 'value array -> id:('value, 'id) id -> ('value, 'id) t
    = fun arr ~id -> Core_kernel.Set.of_array id arr

  let union : ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
    = Core_kernel.Set.union

  let cmp : ('value, 'id) t -> ('value, 'id) t -> int
    = fun s1 s2 ->
      let len1, len2 = Core_kernel.Set.(length s1, length s2) in
      if len1 = len2 then
        let arr1, arr2 = to_array s1, to_array s2 in
        Base.Array.compare (failwith "TODO") arr1 arr2
      else if len1 < len2 then -1 else 1

  let eq : ('value, 'id) t -> ('value, 'id) t -> bool
    = Core_kernel.Set.equal

  let for_each
    : f:('value -> unit) -> ('value, 'id) t -> unit
    = fun ~f set ->
      let _ = Core_kernel.Set.for_all set ~f:(fun v -> f v; true) in ()
end

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Base.Result.t
end

module Bytes = struct
 type t = Core_kernel.Bytes.t
end

module Json = struct
  type t =
    | String of string
    | Array of t array

  let array : t array -> t
    = fun arr -> Array arr

  let string : string -> t
    = fun str -> String str
end

module Sha256 = struct
  let hash_str : string -> Sha256.t
    = Sha256.string

  let to_hex : Sha256.t -> string
    = Sha256.to_hex

  let hash : Bytes.t -> string
    = fun bytes -> bytes
      |> Core_kernel.Bytes.to_string
      |> hash_str
      |> to_hex
end

module Cbor = struct
  let rec of_json : Json.t -> CBOR.Simple.t
    = function
      | String str -> `Text str
      | Array arr -> `Array (arr
        |> Base.Array.map ~f:of_json
        |> Base.Array.to_list
      )

  let rec to_json : CBOR.Simple.t -> Json.t option
    = function
      | `Text str -> Some (String str)
      | `Array arr -> arr
        |> Base.List.map ~f:to_json
        |> Base.Option.all
        |> Base.Option.map ~f:(fun lst -> Json.Array (Base.Array.of_list lst))
      | _ -> None

  let encode : Json.t -> Bytes.t
    = fun json -> json
      |> of_json
      |> CBOR.Simple.encode
      |> Core_kernel.Bytes.of_string

  let decode : Bytes.t -> Json.t option
    = fun buf -> buf
      |> Core_kernel.Bytes.to_string
      |> CBOR.Simple.decode
      |> to_json
end

module String = struct
  let concat_array : ?sep:string -> string array -> string
    = Core_kernel.String.concat_array

  (** raises [Invalid_argument] *)
  let get : string -> int -> char
    = Core_kernel.String.get
end
