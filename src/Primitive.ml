(** A primitive is either a string or an integer. *)

type t =
  | PrimInteger of Bigint.t
  | PrimString of string

let to_string = function
  | PrimInteger i -> Bigint.to_string i
  | PrimString str -> "\"" ^ Caml.String.escaped str ^ "\""
;;

let (=) p1 p2 =
  match p1, p2 with
  | PrimInteger i1, PrimInteger i2 -> Bigint.(i1 = i2) [@warning "-44"]
  | PrimString s1, PrimString s2 -> s1 = s2
  | _ -> false
;;

(** Primitive pretty-printer. *)
let pp : Format.formatter -> t -> unit
  = fun ppf -> function
  | PrimInteger i -> Format.fprintf ppf "%s" (Bigint.to_string i)
  | PrimString s -> Format.fprintf ppf "\"%s\"" s
;;

let jsonify =
  Util.Json.(
    function
    | PrimInteger i -> array [| string "i"; string (Bigint.to_string i) |]
    | PrimString s -> array [| string "s"; string s |])
;;

let unjsonify = Util.Json.(function
  | Array [| String "i"; String i |]
  -> (try
       Some (PrimInteger (Bigint.of_string i))
      with
        Failure _ -> None)
  | Array [| String "s"; String str |]
  -> Some (PrimString str)
  | _
  -> None)

module Properties = struct
  let round_trip1 : t -> bool
    = fun t -> match t |> jsonify |> unjsonify with
      | None -> false
      | Some t' -> t = t'

  let round_trip2 : Util.Json.t -> bool
    = fun json -> match json |> unjsonify with
      | None -> false
      | Some t -> Util.Json.(jsonify t = json)
end
