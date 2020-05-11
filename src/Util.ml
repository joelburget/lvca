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

(* Used by Dynamics.Core, Bidirectional *)
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

(* Used by bidirectional *)
module String = struct
  module Map = struct
    type 'a t = 'a String.Map.t

    let remove_many : 'a t -> string array -> 'a t =
     fun map keys -> Array.fold keys ~init:map ~f:String.Map.remove
   ;;
  end
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
