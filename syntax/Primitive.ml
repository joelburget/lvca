open Base

module type PlainBase_s = sig
  type t

  val pp : t Fmt.t
  val ( = ) : t -> t -> bool
  val parse : (module ParseUtil.Parsers) -> t ParseUtil.t
end

module Make (PlainBase : PlainBase_s) = struct
  type 'info t = 'info * PlainBase.t

  module Plain_typedef = PlainBase
  module Plain = PlainBase

  let to_plain (_, x) = x
  let of_plain x = (), x
  let equal ~info_eq (i1, x1) (i2, x2) = info_eq i1 i2 && PlainBase.(x1 = x2)
  let map_info ~f (i, z) = f i, z

  let pp_generic ~open_loc ~close_loc ppf (i, x) =
    open_loc ppf i;
    PlainBase.pp ppf x;
    close_loc ppf i
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)

    let t =
      let ( >>|| ) = Parsers.( >>|| ) in
      PlainBase.parse (module Parsers) >>|| fun ~pos t -> (pos, t), pos
    ;;
  end
end

module Integer = Make (struct
  type t = Z.t

  let pp ppf x = Fmt.pf ppf "%s" (Z.to_string x)
  let ( = ) x1 x2 = (Z.Compare.(x1 = x2) [@warning "-44"])

  let parse (module Parsers : ParseUtil.Parsers) =
    Parsers.(integer_lit >>| Z.of_string <?> "integer")
  ;;
end)

module Float = Make (struct
  type t = float

  let pp = Fmt.float
  let ( = ) = Float.( = )

  let parse (module Parsers : ParseUtil.Parsers) =
    let open Parsers in
    integer_or_float_lit
    >>= (function First _ -> fail "TODO" | Second f -> return f)
    <?> "float"
  ;;
end)

module Char = Make (struct
  type t = char

  let pp = Fmt.char
  let ( = ) = Char.( = )
  let parse (module Parsers : ParseUtil.Parsers) = Parsers.(char_lit <?> "char")
end)

module Int = Make (struct
  type t = int

  let pp = Fmt.int
  let ( = ) = Int.( = )

  let parse (module Parsers : ParseUtil.Parsers) =
    Parsers.(integer_lit >>| Int.of_string <?> "int")
  ;;
end)

module Int32 = Make (struct
  type t = int32

  let pp = Fmt.int32
  let ( = ) = Int32.( = )

  let parse (module Parsers : ParseUtil.Parsers) =
    Parsers.(integer_lit >>| Int32.of_string <?> "int32")
  ;;
end)

module String = Make (struct
  type t = string

  let pp = Fmt.string
  let ( = ) = String.( = )
  let parse (module Parsers : ParseUtil.Parsers) = Parsers.(string_lit <?> "string")
end)

module Plain = struct
  type t =
    | PrimInteger of Z.t
    | PrimString of string
    | PrimFloat of float
    | PrimChar of char
end

type 'info t = 'info * Plain.t

let to_plain (_, x) = x
let of_plain x = (), x

let equal ~info_eq (info1, p1) (info2, p2) =
  let same_ps =
    match p1, p2 with
    | Plain.PrimInteger i1, Plain.PrimInteger i2 -> Z.Compare.(i1 = i2) [@warning "-44"]
    | PrimString s1, PrimString s2 -> Base.String.(s1 = s2)
    | PrimFloat f1, PrimFloat f2 -> Base.Float.(f1 = f2)
    | PrimChar c1, PrimChar c2 -> Base.Char.(c1 = c2)
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
  | Plain.PrimInteger i -> Integer.pp_generic ~open_loc ~close_loc ppf (info, i)
  | PrimString s -> String.pp_generic ~open_loc ~close_loc ppf (info, s)
  | PrimFloat f -> Float.pp_generic ~open_loc ~close_loc ppf (info, f)
  (* | PrimFloat f -> Fmt.pf ppf "%s" (Float.to_string f) *)
  | PrimChar c -> Char.pp_generic ~open_loc ~close_loc ppf (info, c)
;;

let pp ppf prim = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf prim
let to_string t = Fmt.to_to_string pp t

let check prim sort =
  match snd prim, sort with
  | Plain.PrimString _, Sort.Name (_, "string")
  | PrimFloat _, Sort.Name (_, "float")
  | PrimChar _, Sort.Name (_, "char")
  | PrimInteger _, Sort.Name (_, "integer") ->
    None
  | _, _ ->
    Some
      (Printf.sprintf
         "Unexpected sort (%s) for a primitive (%s)"
         (Fmt.to_to_string Sort.pp sort)
         (to_string prim))
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)

  let t : OptRange.t t Parsers.t =
    let open Parsers in
    choice
      [ (integer_or_float_lit
        >>|| fun ~pos i_or_f ->
        let tm =
          match i_or_f with
          | First i -> pos, Plain.PrimInteger (Z.of_string i)
          | Second f -> pos, PrimFloat f
        in
        tm, pos)
      ; (string_lit >>|| fun ~pos s -> (pos, Plain.PrimString s), pos)
      ; (char_lit >>|| fun ~pos c -> (pos, Plain.PrimChar c), pos)
      ]
    <?> "primitive"
  ;;
end

let jsonify (_, p) =
  Lvca_util.Json.(
    match p with
    | Plain.PrimInteger i -> array [| string "i"; string (Z.to_string i) |]
    | PrimString s -> array [| string "s"; string s |]
    | PrimFloat f -> array [| string "f"; float f |]
    | PrimChar c -> array [| string "c"; string (Base.Char.to_string c) |])
;;

let unjsonify json =
  Lvca_util.Json.(
    match json with
    | Array [| String "i"; String i |] ->
      (try Some ((), Plain.PrimInteger (Z.of_string i)) with Failure _ -> None)
    | Array [| String "f"; Float f |] -> Some ((), PrimFloat f)
    | Array [| String "c"; String c |] ->
      if Base.Int.(Base.String.length c = 1)
      then Some ((), PrimChar (Base.Char.of_string c))
      else None
    | Array [| String "s"; String str |] -> Some ((), PrimString str)
    | _ -> None)
;;

module Properties = struct
  let ( = ) = equal ~info_eq:Unit.( = )

  let json_round_trip1 : unit t -> PropertyResult.t =
   fun t ->
    match t with
    | _, Plain.PrimFloat f when Base.Float.is_nan f -> Uninteresting
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

  let string_round_trip1 : unit t -> PropertyResult.t =
   fun t ->
    match t |> to_string |> ParseUtil.parse_string ParsePrimitive.t with
    | Ok prim -> PropertyResult.check (erase prim = t) (Fmt.str "%a <> %a" pp prim pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
 ;;

  (* Note: +1 -> 1. If the first round-trip isn't equal, try once more. *)
  let string_round_trip2 : string -> PropertyResult.t =
   fun str ->
    match ParseUtil.parse_string ParsePrimitive.t str with
    | Error _ -> Uninteresting
    | Ok prim ->
      let str' = to_string prim in
      if Base.String.(str' = str)
      then Ok
      else (
        match ParseUtil.parse_string ParsePrimitive.t str with
        | Error msg -> Failed msg
        | Ok prim' ->
          let str'' = to_string prim' in
          PropertyResult.check
            Base.String.(str'' = str')
            (Fmt.str {|"%s" <> "%s"|} str'' str'))
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
