open Core_kernel

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

(* module String = struct let concat_array : ?sep:string -> string array -> string =
   String.concat_array

   (** raises [Invalid_argument] *) let get : string -> int -> char = String.get end

   module Re = struct type t = Re.re

   let replace : re:t -> replacement:string -> string -> string = fun ~re ~replacement str
   -> Re.replace re ~f:(fun _ -> replacement) str end *)
