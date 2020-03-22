open Core_kernel

let rec snoc lst a = match lst with [] -> [ a ] | x :: xs -> x :: snoc xs a

let rec unsnoc lst =
  match lst with
  | [] -> failwith "unsnoc empty list"
  | [ x ] -> [], x
  | x :: lst' ->
    let front, last = unsnoc lst' in
    x :: front, last
;;

let map_union m1 m2 =
  Map.merge m1 m2 ~f:(fun ~key:_k ->
    function `Both (_, v) -> Some v | `Left v | `Right v -> Some v)
;;

let int_map_unions : 'a Int.Map.t list -> 'a Int.Map.t =
  List.fold_right ~f:map_union ~init:Int.Map.empty
;;

let string_map_unions : 'a String.Map.t list -> 'a String.Map.t =
  List.fold_right ~f:map_union ~init:String.Map.empty
;;

let get_option : 'b -> 'a option -> 'a =
 fun err -> function None -> raise err | Some a -> a
;;

exception InvariantViolation of string

let invariant_violation str = raise (InvariantViolation str)

let get_option' : (unit -> string) -> 'a option -> 'a =
 fun msg -> function
  | None -> raise (InvariantViolation ("invariant violation: " ^ msg ()))
  | Some a -> a
;;

let stringify_list : ('a -> string) -> string -> 'a list -> string =
 fun f sep elems -> elems |> Array.of_list |> Array.map ~f |> String.concat_array ~sep
;;

module String = struct
  module Map = struct
    type 'a t = 'a String.Map.t

    let remove_many : 'a t -> string array -> 'a t =
     fun map keys -> Array.fold keys ~init:map ~f:String.Map.remove
   ;;
  end
end

module Array = struct
  type 'a t = 'a array

  let reverse_iteri : 'a t -> f:(int -> 'a -> unit) -> unit =
   fun arr ~f ->
    for i = Array.length arr - 1 downto 0 do
      f i arr.(i)
    done
 ;;

  let reverse_iter : 'a t -> f:('a -> unit) -> unit =
   fun arr ~f -> reverse_iteri arr ~f:(fun _ a -> f a)
 ;;

  let reverse : 'a t -> 'a t =
   fun arr ->
    let result = Array.copy arr in
    Array.rev_inplace result;
    result
 ;;
end

module MutableSet = struct
  module Impl = struct
    type ('elt, 'cmp) t = ('elt, 'cmp) Set.t ref

    type ('k, 'cmp) comparator =
      (module Comparator.S with type comparator_witness = 'cmp and type t = 'k)

    let of_list : ('a, 'cmp) comparator -> 'a list -> ('a, 'cmp) t =
     fun comparator lst -> ref (Set.of_list comparator lst)
   ;;

    let create : ('a, 'cmp) comparator -> ('a, 'cmp) t =
     fun comparator -> of_list comparator []
   ;;

    let of_array : ('a, 'cmp) comparator -> 'a array -> ('a, 'cmp) t =
     fun comparator lst -> ref (Set.of_array comparator lst)
   ;;

    let to_list : ('a, _) t -> 'a list = fun t -> Set.to_list !t
    let to_array : ('a, _) t -> 'a array = fun t -> Set.to_array !t
    let copy : ('a, 'cmp) t -> ('a, 'cmp) t = fun t -> ref !t
    let is_empty : ('a, _) t -> bool = fun t -> Set.is_empty !t
    let iter : ('a, _) t -> f:('a -> unit) -> unit = fun t ~f -> Set.iter !t ~f
    let mem : ('a, _) t -> 'a -> bool = fun t a -> Set.mem !t a
    let add : ('a, 'cmp) t -> 'a -> unit = fun t a -> t := Set.add !t a
    let remove : ('a, 'cmp) t -> 'a -> unit = fun t a -> t := Set.remove !t a

    let union : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t =
     fun a b -> ref (Set.union !a !b)
   ;;

    let is_subset : ('a, 'cmp) t -> of_:('a, 'cmp) t -> bool =
     fun t1 ~of_:t2 -> Set.is_subset !t1 ~of_:!t2
   ;;

    let min_elt : ('a, _) t -> 'a option = fun t -> Set.min_elt !t
    let min_elt_exn : ('a, _) t -> 'a = fun t -> Set.min_elt_exn !t
    let snapshot : ('a, 'cmp) t -> ('a, 'cmp) Set.t = fun t -> !t
  end

  module Int = struct
    type t = (int, Int.comparator_witness) Impl.t

    let of_list = Impl.of_list (module Int)
    let to_list = Impl.to_list
    let to_array = Impl.to_array
    let add = Impl.add
    let mem = Impl.mem
    let snapshot = Impl.snapshot
    let is_empty = Impl.is_empty
    let create () = Impl.create (module Int)

    let merge_many : t -> int list -> unit =
     fun t ints -> List.iter ints ~f:(fun i -> add t i)
   ;;
  end

  include Impl
end

module Hash_set = struct
  let is_subset : 'a Hash_set.t -> of_:'a Hash_set.t -> bool =
   fun s1 ~of_:s2 -> Hash_set.(for_all s1 ~f:(mem s2))
 ;;
end

module Json = struct
  type t =
    | String of string
    | Array of t array

  let array : t array -> t = fun arr -> Array arr
  let string : string -> t = fun str -> String str
end

module Sha256 = struct
  let hash_str : string -> Digestif.SHA256.t = Digestif.SHA256.digest_string
  let to_hex : Digestif.SHA256.t -> string = Digestif.SHA256.to_hex

  let hash : Bytes.t -> string =
   fun bytes -> bytes |> Bytes.to_string |> hash_str |> to_hex
 ;;
end

module Cbor = struct
  open Core_kernel

  let rec of_json : Json.t -> CBOR.Simple.t = function
    | String str -> `Text str
    | Array arr -> `Array (arr |> Array.map ~f:of_json |> Array.to_list)
  ;;

  let rec to_json : CBOR.Simple.t -> Json.t option = function
    | `Text str -> Some (String str)
    | `Array arr ->
      arr
      |> List.map ~f:to_json
      |> Option.all
      |> Option.map ~f:(fun lst -> Json.Array (Array.of_list lst))
    | _ -> None
  ;;

  let encode : Json.t -> Bytes.t =
   fun json -> json |> of_json |> CBOR.Simple.encode |> Bytes.of_string
 ;;

  let decode : Bytes.t -> Json.t option =
   fun buf -> buf |> Bytes.to_string |> CBOR.Simple.decode |> to_json
 ;;
end
