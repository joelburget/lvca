open Base

type t =
  | PrimInteger of Z.t
  | PrimString of string
  | PrimFloat of float
  | PrimChar of char

let to_string = function
  | PrimInteger i -> Z.to_string i
  | PrimString str -> {|"|} ^ String.escaped str ^ {|"|}
  (* | PrimFloat f -> Float.to_string f *)
  | PrimFloat f -> Printf.sprintf "%f" f
  | PrimChar c -> {|'|} ^ Char.to_string c ^ {|'|}
;;

let ( = ) p1 p2 =
  match p1, p2 with
  | PrimInteger i1, PrimInteger i2 -> (Z.Compare.(i1 = i2) [@warning "-44"])
  | PrimString s1, PrimString s2 -> String.(s1 = s2)
  | PrimFloat f1, PrimFloat f2 -> Float.(f1 = f2)
  | PrimChar c1, PrimChar c2 -> Char.(c1 = c2)
  | _ -> false
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)

  let t : t Parsers.t =
    let open Parsers in
    choice
      [ (integer_or_float_lit
        >>| fun i_or_f ->
        match i_or_f with
        | First i -> PrimInteger (Z.of_string i)
        | Second f -> PrimFloat f)
      ; (string_lit >>| fun s -> PrimString s)
      ; (char_lit >>| fun c -> PrimChar c)
      ]
    <?> "primitive"
  ;;
end

(** Primitive pretty-printer. *)
let pp : t Fmt.t =
 fun ppf -> function
  | PrimInteger i -> Fmt.pf ppf "%s" (Z.to_string i)
  | PrimString s -> Fmt.pf ppf {|"%s"|} s
  | PrimFloat f -> Fmt.pf ppf "%f" f
  (* | PrimFloat f -> Fmt.pf ppf "%s" (Float.to_string f) *)
  | PrimChar c -> Fmt.pf ppf "'%c'" c
;;

let jsonify =
  Lvca_util.Json.(
    function
    | PrimInteger i -> array [| string "i"; string (Z.to_string i) |]
    | PrimString s -> array [| string "s"; string s |]
    | PrimFloat f -> array [| string "f"; float f |]
    | PrimChar c -> array [| string "c"; string (Char.to_string c) |])
;;

let unjsonify =
  Lvca_util.Json.(
    function
    | Array [| String "i"; String i |] ->
      (try Some (PrimInteger (Z.of_string i)) with Failure _ -> None)
    | Array [| String "f"; Float f |] -> Some (PrimFloat f)
    | Array [| String "c"; String c |] ->
      if Int.(String.length c = 1) then Some (PrimChar (Char.of_string c)) else None
    | Array [| String "s"; String str |] -> Some (PrimString str)
    | _ -> None)
;;

module Properties = struct
  let json_round_trip1 : t -> PropertyResult.t =
   fun t ->
    match t with
    | PrimFloat f when Float.is_nan f -> Uninteresting
    | _ ->
      (match t |> jsonify |> unjsonify with
      | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
      | Some t' -> PropertyResult.check (t' = t) (Fmt.str "%a <> %a" pp t' pp t))
 ;;

  let json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t =
   fun json ->
    match json |> unjsonify with
    | Some t ->
      PropertyResult.check
        Lvca_util.Json.(jsonify t = json)
        "jsonify t <> json (TODO: print)"
    | None -> Uninteresting
 ;;

  module ParsePrimitive = Parse (ParseUtil.NoComment)

  let string_round_trip1 : t -> PropertyResult.t =
   fun t ->
    match t |> to_string |> ParseUtil.parse_string ParsePrimitive.t with
    | Ok prim -> PropertyResult.check (prim = t) (Fmt.str "%a <> %a" pp prim pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
 ;;

  (* Note: +1 -> 1. If the first round-trip isn't equal, try once more. *)
  let string_round_trip2 : string -> PropertyResult.t =
   fun str ->
    match ParseUtil.parse_string ParsePrimitive.t str with
    | Error _ -> Uninteresting
    | Ok prim ->
      let str' = to_string prim in
      if String.(str' = str)
      then Ok
      else (
        match ParseUtil.parse_string ParsePrimitive.t str with
        | Error msg -> Failed msg
        | Ok prim' ->
          let str'' = to_string prim' in
          PropertyResult.check String.(str'' = str') (Fmt.str {|"%s" <> "%s"|} str'' str'))
 ;;

  (* malformed input *)
end

let%test_module "Parsing" =
  (module struct
    let print_parse str =
      match ParseUtil.parse_string_pos Properties.ParsePrimitive.t str with
      | Ok (prim, range) -> Fmt.pr "%a %a" pp prim OptRange.pp range
      | Error msg -> Fmt.pr "%s" msg
    ;;

    let%expect_test _ =
      print_parse "123";
      [%expect {| 123 {0,3} |}]
    ;;

    let%expect_test _ =
      print_parse {|"abc"|};
      [%expect {| "abc" {0,5} |}]
    ;;

    let%expect_test _ =
      print_parse "1.1";
      [%expect {| 1.100000 {0,3} |}]
    ;;

    let%expect_test _ =
      print_parse {|'c'|};
      [%expect {| 'c' {0,3} |}]
    ;;

    let%expect_test _ =
      print_parse "0.00000";
      [%expect {| 0.000000 {0,7} |}]
    ;;

    let%expect_test _ =
      print_parse "-0.00000";
      [%expect {| -0.000000 {0,8} |}]
    ;;

    (* TODO: are floats a good idea?
    let%expect_test _ =
      let f = 2.9384442618974733e-233 in
      Fmt.pr "~negative:%b ~exponent:%i ~mantissa:%s\n"
        (* 1 bit: *) (Float.ieee_negative f)
        (* 11 bits: *) (Float.ieee_exponent f)
        (* 52 bits: *) (f |> Float.ieee_mantissa |> Int63.to_string);
      (* Fmt.pr "%a" pp (PrimFloat f); *)
      Fmt.pr "%f %F %e %E %g %G %s\n" f f f f f f (Float.to_string f);
      [%expect]
      *)
  end)
;;
