open Base
open Lvca_util

module type Plain_base_s = sig
  type t

  val pp : t Fmt.t
  val ( = ) : t -> t -> bool
  val parse : t Lvca_parsing.t
  val jsonify : t Json.serializer
  val unjsonify : t Json.deserializer
end

module Make (Plain_base : Plain_base_s) = struct
  type 'info t = 'info * Plain_base.t

  module Plain = Plain_base

  let info (i, _) = i
  let to_plain (_, x) = x
  let of_plain x = (), x
  let equal ~info_eq (i1, x1) (i2, x2) = info_eq i1 i2 && Plain_base.(x1 = x2)
  let map_info ~f (i, z) = f i, z

  let pp_generic ~open_loc ~close_loc ppf (i, x) =
    open_loc ppf i;
    Plain_base.pp ppf x;
    close_loc ppf i
  ;;

  let jsonify (_, plain) = Plain_base.jsonify plain
  let unjsonify json = Plain_base.unjsonify json |> Option.map ~f:(fun tm -> (), tm)

  module Parse = struct
    open Lvca_parsing

    let t =
      Plain_base.parse
      >>|| fun (Parse_result.{ value; range } as parse_result) ->
      { parse_result with value = range, value }
    ;;
  end
end

module Integer = Make (struct
  type t = Z.t

  let pp ppf x = Fmt.string ppf (Z.to_string x)
  let ( = ) x1 x2 = (Z.Compare.(x1 = x2) [@warning "-44"])
  let parse = Lvca_parsing.(integer_lit >>| Z.of_string <?> "integer")
  let jsonify i = Json.string (Z.to_string i)

  let unjsonify =
    Json.(
      function
      | String i -> (try Some (Z.of_string i) with Failure _ -> None) | _ -> None)
  ;;
end)

module Float = Make (struct
  type t = float

  let pp ppf = Fmt.pf ppf "%f"
  let ( = ) = Float.( = )

  let parse =
    let open Lvca_parsing in
    integer_or_float_lit
    >>= (function First _ -> fail "TODO" | Second f -> return f)
    <?> "float"
  ;;

  let jsonify f = Json.float f
  let unjsonify = Json.(function Float f -> Some f | _ -> None)
end)

module Char = Make (struct
  type t = char

  let pp = Fmt.quote ~mark:"\'" Fmt.char
  let ( = ) = Char.( = )
  let parse = Lvca_parsing.(char_lit <?> "char")
  let jsonify c = Json.string (Char.to_string c)

  let unjsonify =
    Json.(
      function
      | String c ->
        if Base.Int.(Base.String.length c = 1) then Some (Base.Char.of_string c) else None
      | _ -> None)
  ;;
end)

module Int32 = Make (struct
  type t = int32

  let pp = Fmt.int32
  let ( = ) = Int32.( = )
  let parse = Lvca_parsing.(integer_lit >>| Int32.of_string <?> "int32")

  (* TODO: remove exns *)
  let jsonify = Int32.to_int_exn >> Json.int
  let unjsonify = Json.(function Int f -> Some (Int32.of_int_exn f) | _ -> None)
end)

module String = Make (struct
  type t = string

  let pp = Fmt.(quote string)
  let ( = ) = String.( = )
  let parse = Lvca_parsing.(string_lit <?> "string")
  let jsonify s = Json.string s
  let unjsonify = Json.(function String s -> Some s | _ -> None)
end)

module Plain = struct
  type t =
    | Integer of Z.t
    | Int32 of int32
    | String of string
    | Float of float
    | Char of char
end

type 'info t = 'info * Plain.t

let to_plain (_, x) = x
let of_plain x = (), x

let equal ~info_eq (info1, p1) (info2, p2) =
  let same_ps =
    match p1, p2 with
    | Plain.Integer i1, Plain.Integer i2 -> Z.Compare.(i1 = i2) [@warning "-44"]
    | Int32 i1, Int32 i2 -> Base.Int32.(i1 = i2)
    | String s1, String s2 -> Base.String.(s1 = s2)
    | Float f1, Float f2 -> Base.Float.(f1 = f2)
    | Char c1, Char c2 -> Base.Char.(c1 = c2)
    | _ -> false
  in
  same_ps && info_eq info1 info2
;;

let info (i, _) = i
let map_info ~f (i, x) = f i, x
let erase t = map_info ~f:(Fn.const ()) t

(** Primitive pretty-printer. *)
let pp_generic ~open_loc ~close_loc ppf (info, prim) =
  match prim with
  | Plain.Integer i -> Integer.pp_generic ~open_loc ~close_loc ppf (info, i)
  | Int32 i -> Int32.pp_generic ~open_loc ~close_loc ppf (info, i)
  | String s -> String.pp_generic ~open_loc ~close_loc ppf (info, s)
  | Float f -> Float.pp_generic ~open_loc ~close_loc ppf (info, f)
  | Char c -> Char.pp_generic ~open_loc ~close_loc ppf (info, c)
;;

let pp ppf prim = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf prim
let to_string t = Fmt.to_to_string pp t

let check prim sort =
  match snd prim, sort with
  | Plain.String _, Sort.Name (_, "string")
  | Float _, Sort.Name (_, "float")
  | Char _, Sort.Name (_, "char")
  | Integer _, Sort.Name (_, "integer")
  | Int32 _, Sort.Name (_, "int32") ->
    None
  | _, _ ->
    Some
      (Printf.sprintf
         "Unexpected sort (%s) for a primitive (%s)"
         (Fmt.to_to_string Sort.pp sort)
         (to_string prim))
;;

module Parse = struct
  open Lvca_parsing

  let t =
    choice
      [ (integer_or_float_lit
        >>| function First i -> Plain.Integer (Z.of_string i) | Second f -> Float f)
        (* Note: all ints parse to Integer *)
      ; (string_lit >>| fun s -> Plain.String s)
      ; (char_lit >>| fun c -> Plain.Char c)
      ]
    >>|| (fun parse_result ->
           let value = parse_result.range, parse_result.value in
           { parse_result with value })
    <?> "primitive"
  ;;
end

let jsonify (_, p) =
  Json.(
    match p with
    | Plain.Integer i -> array [| string "i"; string (Z.to_string i) |]
    | Int32 i -> array [| string "i32"; string (Base.Int32.to_string i) |]
    | String s -> array [| string "s"; string s |]
    | Float f -> array [| string "f"; float f |]
    | Char c -> array [| string "c"; string (Base.Char.to_string c) |])
;;

let unjsonify json =
  Json.(
    match json with
    | Array [| String "i"; String i |] ->
      (try Some ((), Plain.Integer (Z.of_string i)) with Failure _ -> None)
    | Array [| String "i32"; String i |] ->
      (try Some ((), Int32 (Base.Int32.of_string i)) with Failure _ -> None)
    | Array [| String "f"; Float f |] -> Some ((), Float f)
    | Array [| String "c"; String c |] ->
      if Base.Int.(Base.String.length c = 1)
      then Some ((), Char (Base.Char.of_string c))
      else None
    | Array [| String "s"; String str |] -> Some ((), String str)
    | _ -> None)
;;

module Properties = struct
  let ( = ) = equal ~info_eq:Unit.( = )

  let json_round_trip1 : unit t -> Property_result.t =
   fun t ->
    match t with
    | _, Plain.Float f when Base.Float.is_nan f -> Uninteresting
    | _ ->
      (match t |> jsonify |> unjsonify with
      | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
      | Some t' -> Property_result.check (t' = t) (Fmt.str "%a <> %a" pp t' pp t))
 ;;

  let json_round_trip2 : Json.t -> Property_result.t =
   fun json ->
    match json |> unjsonify with
    | Some t ->
      Property_result.check Json.(jsonify t = json) "jsonify t <> json (TODO: print)"
    | None -> Uninteresting
 ;;

  let string_round_trip1 : unit t -> Property_result.t =
   fun t ->
    match t |> to_string |> Lvca_parsing.parse_string Parse.t with
    | Ok prim -> Property_result.check (erase prim = t) (Fmt.str "%a <> %a" pp prim pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
 ;;

  (* Note: +1 -> 1. If the first round-trip isn't equal, try once more. *)
  let string_round_trip2 : string -> Property_result.t =
   fun str ->
    match Lvca_parsing.parse_string Parse.t str with
    | Error _ -> Uninteresting
    | Ok prim ->
      let str' = to_string prim in
      if Base.String.(str' = str)
      then Ok
      else (
        match Lvca_parsing.parse_string Parse.t str with
        | Error msg -> Failed msg
        | Ok prim' ->
          let str'' = to_string prim' in
          Property_result.check
            Base.String.(str'' = str')
            (Fmt.str {|"%s" <> "%s"|} str'' str'))
 ;;

  (* malformed input *)
end

let%test_module "Parsing" =
  (module struct
    open Lvca_provenance

    let print_parse str =
      match Lvca_parsing.parse_string_pos Parse.t str with
      | Ok { value = prim; range } -> Fmt.pr "%a %a" pp prim Opt_range.pp range
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
      (* Fmt.pr "%a" pp (Float f); *)
      Fmt.pr "%f %F %e %E %g %G %s\n" f f f f f f (Float.to_string f);
      [%expect]
      *)
  end)
;;
