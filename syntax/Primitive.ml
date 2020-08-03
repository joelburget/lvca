(** A primitive is either a string or an integer. *)

module Util = Lvca_util

type t =
  | PrimInteger of Bigint.t
  | PrimString of string
  | PrimFloat of float
  | PrimChar of char

let to_string = function
  | PrimInteger i -> Bigint.to_string i
  | PrimString str -> "\"" ^ Caml.String.escaped str ^ "\""
  | PrimFloat f -> Float.to_string f
  | PrimChar c -> "\'" ^ Base.Char.to_string c ^ "\'"
;;

let (=) p1 p2 =
  match p1, p2 with
  | PrimInteger i1, PrimInteger i2 -> Bigint.(i1 = i2) [@warning "-44"]
  | PrimString s1, PrimString s2 -> s1 = s2
  | PrimFloat f1, PrimFloat f2 -> f1 = f2
  | PrimChar c1, PrimChar c2 -> c1 = c2
  | _ -> false
;;

module Parse (Comment : Util.Angstrom.Comment_int) = struct
  module Parsers = Util.Angstrom.Mk(Comment)

  let t : t Angstrom.t
    = let open Angstrom in
      let char_lit, integer_or_float_lit, string_lit =
        Parsers.(char_lit, integer_or_float_lit, string_lit)
      in
      choice
        [ integer_or_float_lit >>| (function
            | First i -> PrimInteger (Bigint.of_string i)
            | Second f -> PrimFloat f)
        ; string_lit >>| (fun s -> PrimString s)
        ; char_lit >>| (fun c -> PrimChar c)
        ] <?> "primitive"
end

(** Primitive pretty-printer. *)
let pp : Format.formatter -> t -> unit
  = fun ppf -> function
  | PrimInteger i -> Format.fprintf ppf "%s" (Bigint.to_string i)
  | PrimString s -> Format.fprintf ppf "\"%s\"" s
  | PrimFloat f -> Format.fprintf ppf "%f" f
  | PrimChar c -> Format.fprintf ppf "'%c'" c
;;

let jsonify =
  Util.Json.(
    function
    | PrimInteger i -> array [| string "i"; string (Bigint.to_string i) |]
    | PrimString s -> array [| string "s"; string s |]
    | PrimFloat f -> array [| string "f"; float f |]
    | PrimChar c -> array [| string "c"; string (Base.Char.to_string c) |])
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
  let json_round_trip1 : t -> bool
    = fun t -> match t |> jsonify |> unjsonify with
      | None -> false
      | Some t' -> t = t'

  let json_round_trip2 : Util.Json.t -> bool
    = fun json -> match json |> unjsonify with
      | None -> true (* malformed input *)
      | Some t -> Util.Json.(jsonify t = json)

  module Parse' = Parse(Util.Angstrom.NoComment)

  let string_round_trip1 : t -> bool
    = fun t -> match t |> to_string |> Angstrom.parse_string ~consume:All Parse'.t with
      | Ok prim -> prim = t
      | Error _ -> false

  let string_round_trip2 : string -> bool
    = fun str -> match Angstrom.parse_string ~consume:All Parse'.t str with
      | Ok prim -> let str' = to_string prim in Base.String.(str' = str)
      | Error _ -> true (* malformed input *)
end

let%test_module "Parsing" = (module struct
  let (=) = Caml.(=)
  let parse' = Angstrom.parse_string ~consume:All Properties.Parse'.t

  let%test _ = parse' "123" = Ok (PrimInteger (Bigint.of_int 123))
  let%test _ = parse' {|"abc"|} = Ok (PrimString "abc")
  let%test _ = parse' "1.1" = Ok (PrimFloat 1.1)
  let%test _ = parse' {|'c'|} = Ok (PrimChar 'c')
end);;
