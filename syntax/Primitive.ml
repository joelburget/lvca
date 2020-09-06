(** A primitive is either a string or an integer. *)

open Base

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
  | PrimString s1, PrimString s2 -> String.(s1 = s2)
  | PrimFloat f1, PrimFloat f2 -> Float.(f1 = f2)
  | PrimChar c1, PrimChar c2 -> Char.(c1 = c2)
  | _ -> false
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk(Comment)

  let t : t Parsers.t
    = let open Parsers in
      choice
        [ integer_or_float_lit >>| (fun i_or_f -> match i_or_f with
            | First i -> PrimInteger (Bigint.of_string i)
            | Second f -> PrimFloat f)
        ; string_lit >>| (fun s -> PrimString s)
        ; char_lit >>| (fun c -> PrimChar c)
        ] <?> "primitive"
end

(** Primitive pretty-printer. *)
let pp : t Fmt.t
  = fun ppf -> function
  | PrimInteger i -> Fmt.pf ppf "%s" (Bigint.to_string i)
  | PrimString s -> Fmt.pf ppf "\"%s\"" s
  | PrimFloat f -> Fmt.pf ppf "%g" f
  | PrimChar c -> Fmt.pf ppf "'%c'" c
;;

let jsonify =
  Lvca_util.Json.(
    function
    | PrimInteger i -> array [| string "i"; string (Bigint.to_string i) |]
    | PrimString s -> array [| string "s"; string s |]
    | PrimFloat f -> array [| string "f"; float f |]
    | PrimChar c -> array [| string "c"; string (Base.Char.to_string c) |])
;;

let unjsonify = Lvca_util.Json.(function
  | Array [| String "i"; String i |]
  ->
    begin
      try
       Some (PrimInteger (Bigint.of_string i))
      with
        Failure _ -> None
    end
  | Array [| String "s"; String str |]
  -> Some (PrimString str)
  | _
  -> None)

module Properties = struct
  let json_round_trip1 : t -> bool
    = fun t -> match t |> jsonify |> unjsonify with
      | None -> false
      | Some t' -> t = t'

  let json_round_trip2 : Lvca_util.Json.t -> bool
    = fun json -> match json |> unjsonify with
      | None -> true (* malformed input *)
      | Some t -> Lvca_util.Json.(jsonify t = json)

  module Parse' = Parse(ParseUtil.NoComment)

  let string_round_trip1 : t -> bool
    = fun t -> match t |> to_string |> Angstrom.parse_string ~consume:All Parse'.t with
      | Ok (prim, _) -> prim = t
      | Error _ -> false

  let string_round_trip2 : string -> bool
    = fun str -> match Angstrom.parse_string ~consume:All Parse'.t str with
      | Ok (prim, _) -> let str' = to_string prim in Base.String.(str' = str)
      | Error _ -> true (* malformed input *)
end

let%test_module "Parsing" = (module struct
  let print_parse str =
    match Angstrom.parse_string ~consume:All Properties.Parse'.t str with
      | Ok (prim, range) -> Fmt.pr "%a %a" pp prim Range.pp range
      | Error msg -> Fmt.pr "%s" msg

  let%expect_test _ = print_parse "123"; [%expect{| 123 {0,3} |}]
  let%expect_test _ = print_parse {|"abc"|}; [%expect{| "abc" {0,5} |}]
  let%expect_test _ = print_parse "1.1"; [%expect{| 1.1 {0,3} |}]
  let%expect_test _ = print_parse {|'c'|}; [%expect{| 'c' {0,3} |}]
end);;
