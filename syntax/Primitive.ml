open Base

type 'info t =
  | PrimInteger of 'info * Z.t
  | PrimString of 'info * string
  | PrimFloat of 'info * float
  | PrimChar of 'info * char

module Plain = struct
  type t =
    | PrimInteger of Z.t
    | PrimString of string
    | PrimFloat of float
    | PrimChar of char
end

let to_plain = function
  | PrimInteger (_, x) -> Plain.PrimInteger x
  | PrimString (_, x) -> Plain.PrimString x
  | PrimFloat (_, x) -> Plain.PrimFloat x
  | PrimChar (_, x) -> Plain.PrimChar x
;;

let of_plain = function
  | Plain.PrimInteger x -> PrimInteger ((), x)
  | Plain.PrimString x -> PrimString ((), x)
  | Plain.PrimFloat x -> PrimFloat ((), x)
  | Plain.PrimChar x -> PrimChar ((), x)
;;

let equal ~info_eq p1 p2 =
  match p1, p2 with
  | PrimInteger (info1, i1), PrimInteger (info2, i2) ->
    info_eq info1 info2 && (Z.Compare.(i1 = i2) [@warning "-44"])
  | PrimString (info1, s1), PrimString (info2, s2) ->
    info_eq info1 info2 && String.(s1 = s2)
  | PrimFloat (info1, f1), PrimFloat (info2, f2) -> info_eq info1 info2 && Float.(f1 = f2)
  | PrimChar (info1, c1), PrimChar (info2, c2) -> info_eq info1 info2 && Char.(c1 = c2)
  | _ -> false
;;

let info = function
  | PrimInteger (info, _) | PrimString (info, _) | PrimFloat (info, _) | PrimChar (info, _)
    ->
    info
;;

let map_info ~f = function
  | PrimInteger (info, i) -> PrimInteger (f info, i)
  | PrimString (info, s) -> PrimString (f info, s)
  | PrimFloat (info, x) -> PrimFloat (f info, x)
  | PrimChar (info, c) -> PrimChar (f info, c)
;;

let erase t = map_info ~f:(Fn.const ()) t

(** Primitive pretty-printer. *)
let pp_generic ~open_loc ~close_loc ppf prim =
  open_loc ppf (info prim);
  (match prim with
  | PrimInteger (_, i) -> Fmt.pf ppf "%s" (Z.to_string i)
  | PrimString (_, s) -> Fmt.pf ppf {|"%s"|} s
  | PrimFloat (_, f) -> Fmt.pf ppf "%f" f
  (* | PrimFloat f -> Fmt.pf ppf "%s" (Float.to_string f) *)
  | PrimChar (_, c) -> Fmt.pf ppf "'%c'" c);
  close_loc ppf (info prim)
;;

let pp ppf prim = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf prim
let to_string t = Fmt.to_to_string pp t

let check _info prim sort =
  match prim, sort with
  | PrimString _, Sort.Name (_, "string")
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
          | First i -> PrimInteger (pos, Z.of_string i)
          | Second f -> PrimFloat (pos, f)
        in
        tm, pos)
      ; (string_lit >>|| fun ~pos s -> PrimString (pos, s), pos)
      ; (char_lit >>|| fun ~pos c -> PrimChar (pos, c), pos)
      ]
    <?> "primitive"
  ;;
end

let jsonify =
  Lvca_util.Json.(
    function
    | PrimInteger (_, i) -> array [| string "i"; string (Z.to_string i) |]
    | PrimString (_, s) -> array [| string "s"; string s |]
    | PrimFloat (_, f) -> array [| string "f"; float f |]
    | PrimChar (_, c) -> array [| string "c"; string (Char.to_string c) |])
;;

let unjsonify =
  Lvca_util.Json.(
    function
    | Array [| String "i"; String i |] ->
      (try Some (PrimInteger ((), Z.of_string i)) with Failure _ -> None)
    | Array [| String "f"; Float f |] -> Some (PrimFloat ((), f))
    | Array [| String "c"; String c |] ->
      if Int.(String.length c = 1) then Some (PrimChar ((), Char.of_string c)) else None
    | Array [| String "s"; String str |] -> Some (PrimString ((), str))
    | _ -> None)
;;

module Properties = struct
  let ( = ) = equal ~info_eq:Unit.( = )

  let json_round_trip1 : unit t -> PropertyResult.t =
   fun t ->
    match t with
    | PrimFloat ((), f) when Float.is_nan f -> Uninteresting
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

module Integer = struct
  type 'info t = 'info * Z.t

  module Plain_typedef = struct
    type t = Z.t
  end

  module Plain = Plain_typedef

  let to_plain (_, z) = z
  let of_plain z = (), z

  let equal ~info_eq (i1, z1) (i2, z2) =
    info_eq i1 i2 && (Z.Compare.(z1 = z2) [@warning "-44"])
  ;;

  let map_info ~f (i, z) = f i, z

  let pp_generic ~open_loc ~close_loc ppf (i, z) =
    open_loc ppf i;
    Fmt.pf ppf "%s" (Z.to_string z);
    close_loc ppf i
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)

    let t : OptRange.t t Parsers.t =
      let open Parsers in
      integer_lit >>|| (fun ~pos s -> (pos, Z.of_string s), pos) <?> "integer"
    ;;
  end
end

module Int = struct
  type 'info t = 'info * int

  module Plain_typedef = struct
    type t = int
  end

  module Plain = Plain_typedef

  let to_plain (_, n) = n
  let of_plain n = (), n
  let equal ~info_eq (i1, n1) (i2, n2) = info_eq i1 i2 && Int.(n1 = n2)
  let map_info ~f (i, n) = f i, n

  let pp_generic ~open_loc ~close_loc ppf (i, n) =
    open_loc ppf i;
    Fmt.pf ppf "%d" n;
    close_loc ppf i
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)

    let t : OptRange.t t Parsers.t =
      let open Parsers in
      integer_lit >>|| (fun ~pos s -> (pos, Int.of_string s), pos) <?> "integer"
    ;;
  end
end

module String = struct
  type 'info t = 'info * string

  module Plain_typedef = struct
    type t = string
  end

  module Plain = Plain_typedef

  let to_plain (_, s) = s
  let of_plain s = (), s
  let equal ~info_eq (i1, s1) (i2, s2) = info_eq i1 i2 && String.(s1 = s2)
  let map_info ~f (i, s) = f i, s

  let pp_generic ~open_loc ~close_loc ppf (i, s) =
    open_loc ppf i;
    Fmt.pf ppf {|"%s"|} s;
    close_loc ppf i
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)

    let t : OptRange.t t Parsers.t =
      let open Parsers in
      string_lit >>|| (fun ~pos s -> (pos, s), pos) <?> "string"
    ;;
  end
end
