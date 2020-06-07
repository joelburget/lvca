open Base

(* Used by bidirectional *)
module Map = struct
  include Base.Map

  let remove_many : ('k, 'v, 'cmp) t -> string array -> ('k, 'v, 'cmp) t =
   fun map keys -> Array.fold keys ~init:map ~f:Map.remove
  ;;

  let union_left_biased m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_k ->
      function `Both (v, _) -> Some v | `Left v | `Right v -> Some v)
  ;;

  let union_right_biased m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_k ->
      function `Both (_, v) -> Some v | `Left v | `Right v -> Some v)
  ;;
end

module String = struct
  include Base.String

  module Map = struct
    type 'a t = (string, 'a, Base.String.comparator_witness) Base.Map.t
    type key = string
    let empty = Base.Map.empty (module Base.String)
    let singleton k v = Base.Map.singleton (module Base.String) k v
    let of_alist lst = Base.Map.of_alist (module Base.String) lst
    let of_alist_exn lst = Base.Map.of_alist_exn (module Base.String) lst

    (* Used by Core.Types, Bidirectional *)
    let unions_left_biased : 'a t list -> 'a t
      = List.fold_left ~init:empty
          ~f:(Map.merge_skewed ~combine:(fun ~key:_k v1 _v2 -> v1))

    let unions_right_biased : 'a t list -> 'a t
      = List.fold_left ~init:empty
          ~f:(Map.merge_skewed ~combine:(fun ~key:_k _v1 v2 -> v2))

    exception DuplicateKey of string

    let strict_union
      : 'a t -> 'a t -> [ `Ok of 'a t | `Duplicate_key of string ]
    = fun m1 m2 -> try
        `Ok (Map.merge m1 m2 ~f:(fun ~key -> function
          | `Both _ -> raise (DuplicateKey key)
          | `Left v | `Right v -> Some v))
      with
        DuplicateKey k -> `Duplicate_key k
    ;;

    let strict_unions
      : 'a t list -> [ `Ok of 'a t | `Duplicate_key of string ]
      = fun lst -> try
          `Ok (List.fold_left lst
            ~init:empty
            ~f:(fun accum m -> match strict_union accum m with
              | `Ok m' -> m'
              | `Duplicate_key key -> raise (DuplicateKey key))
          )
        with
          DuplicateKey k -> `Duplicate_key k
  end

  module Set = struct
    type t = (string, Base.String.comparator_witness) Base.Set.t
    let empty = Base.Set.empty (module Base.String)
    let of_list = Base.Set.of_list (module Base.String)
  end

  let slice : string -> int -> int -> string
    = fun t start stop ->
      let normalize t' i = if Int.(i < 0) then i + length t' else i in
      let stop = if Int.(stop = 0) then length t else stop in
      let pos = normalize t start in
      let len = normalize t stop - pos in
      sub t ~pos ~len
end

module List = struct
  let rec unsnoc lst =
    match lst with
    | [] -> failwith "unsnoc empty list"
    | [ x ] -> [], x
    | x :: lst' ->
      let front, last = unsnoc lst' in
      x :: front, last
  ;;
end

exception InvariantViolation of string

let invariant_violation str = raise (InvariantViolation str)

module Option = struct
  let get_or_raise : 'b -> 'a option -> 'a =
   fun err -> function None -> raise err | Some a -> a
  ;;

  let get_invariant : (unit -> string) -> 'a option -> 'a =
   fun msg -> function
    | None -> raise (InvariantViolation ("invariant violation: " ^ msg ()))
    | Some a -> a
  ;;
end

module Json = struct
  type t =
    | String of string
    | Array of t array
    | Float of float

  let array : t array -> t = fun arr -> Array arr
  let string : string -> t = fun str -> String str
  let float : float -> t = fun f -> Float f

  let rec (=) : t -> t -> bool
    = fun t1 t2 -> match t1, t2 with
      | String s1, String s2 -> String.(s1 = s2)
      | Array arr1, Array arr2 -> (match Array.zip arr1 arr2 with
        | None -> false
        | Some arr -> Array.for_all arr ~f:(fun (t1, t2) -> t1 = t2))
      | Float f1, Float f2 -> Float.(f1 = f2)
      | _, _ -> false
end

module Sha256 = struct
  let hash_str : string -> Digestif.SHA256.t = Digestif.SHA256.digest_string
  let to_hex : Digestif.SHA256.t -> string = Digestif.SHA256.to_hex

  let hash : Bytes.t -> string =
   fun bytes -> bytes |> Bytes.to_string |> hash_str |> to_hex
 ;;
end

module Cbor = struct
  let rec of_json : Json.t -> CBOR.Simple.t = function
    | String str -> `Text str
    | Array arr -> `Array (arr |> Array.map ~f:of_json |> Array.to_list)
    | Float f -> `Float f
  ;;

  let rec to_json : CBOR.Simple.t -> Json.t option = function
    | `Text str -> Some (String str)
    | `Array arr ->
      arr
      |> Base.List.map ~f:to_json
      |> Base.Option.all
      |> Base.Option.map ~f:(fun lst -> Json.Array (Array.of_list lst))
    | `Float f -> Some (Float f)
    | _ -> None
  ;;

  let encode : Json.t -> Bytes.t =
   fun json -> json |> of_json |> CBOR.Simple.encode |> Bytes.of_string
 ;;

  let decode : Bytes.t -> Json.t option =
   fun buf -> buf |> Bytes.to_string |> CBOR.Simple.decode |> to_json
 ;;
end
