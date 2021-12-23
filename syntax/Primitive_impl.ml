open Base
open Lvca_util

module type Base_plain_s = sig
  type t

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
  val jsonify : t Json.serializer
  val unjsonify : t Json.deserializer
end

module Make (Base_plain : Base_plain_s) : sig
  type t = Provenance.t * Base_plain.t

  val info : t -> Provenance.t
  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
  val jsonify : t Json.serializer
  val unjsonify : t Json.deserializer
end = struct
  type t = Provenance.t * Base_plain.t

  let info (i, _) = i

  let equivalent ?(info_eq = fun _ _ -> true) (i1, x1) (i2, x2) =
    info_eq i1 i2 && Base_plain.(x1 = x2)
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
  let pp ppf (i, x) = Provenance.fmt_stag i Base_plain.pp ppf x
  let jsonify (_, plain) = Base_plain.jsonify plain

  let unjsonify json =
    Base_plain.unjsonify json |> Option.map ~f:(fun tm -> Provenance.of_here [%here], tm)
  ;;

  let parse =
    let open Lvca_parsing in
    Base_plain.parse >>~ fun location value -> Provenance.of_range location, value
  ;;
end

module Integer_plain = struct
  type t = Z.t

  let pp ppf x = Fmt.string ppf (Z.to_string x)
  let ( = ) x1 x2 = (Z.Compare.(x1 = x2) [@warning "-44"])
  let parse = Lvca_parsing.(C_comment_parser.integer_lit >>| Z.of_string <?> "integer")
  let jsonify i = Json.string (Z.to_string i)

  let unjsonify =
    Json.(
      function
      | String i -> (try Some (Z.of_string i) with Failure _ -> None) | _ -> None)
  ;;
end

module Integer = Make (Integer_plain)

module Float_plain = struct
  type t = float

  let pp ppf = Fmt.pf ppf "%f"
  let ( = ) = Float.( = )

  let parse =
    let open Lvca_parsing in
    C_comment_parser.integer_or_float_lit
    >>= (function First _ -> fail "TODO" | Second f -> return f)
    <?> "float"
  ;;

  let jsonify f = Json.float f
  let unjsonify = Json.(function Float f -> Some f | _ -> None)
end

module Float = Make (Float_plain)

module Char_plain = struct
  type t = char

  let pp = Fmt.quote ~mark:"\'" Fmt.char
  let ( = ) = Char.( = )
  let parse = Lvca_parsing.(C_comment_parser.char_lit <?> "char")
  let jsonify c = Json.string (Char.to_string c)

  let unjsonify =
    Json.(
      function
      | String c ->
        if Base.Int.(Base.String.length c = 1) then Some (Base.Char.of_string c) else None
      | _ -> None)
  ;;
end

module Char = Make (Char_plain)

module Int32_plain = struct
  type t = int32

  let pp = Fmt.int32
  let ( = ) = Int32.( = )
  let parse = Lvca_parsing.(C_comment_parser.integer_lit >>| Int32.of_string <?> "int32")

  (* TODO: remove exns *)
  let jsonify = Int32.to_int_exn >> Json.int
  let unjsonify = Json.(function Int f -> Some (Int32.of_int_exn f) | _ -> None)
end

module Int32 = Make (Int32_plain)

module String_plain = struct
  type t = string

  let pp = Fmt.(quote string)
  let ( = ) = String.( = )
  let parse = Lvca_parsing.(C_comment_parser.string_lit <?> "string")
  let jsonify s = Json.string s
  let unjsonify = Json.(function String s -> Some s | _ -> None)
end

module String = Make (String_plain)

module All_plain = struct
  type t =
    | Integer of Z.t
    | Int32 of int32
    | String of string
    | Float of float
    | Char of char

  let pp ppf = function
    | Integer i -> Integer_plain.pp ppf i
    | Int32 i -> Int32_plain.pp ppf i
    | String s -> String_plain.pp ppf s
    | Float f -> Float_plain.pp ppf f
    | Char c -> Char_plain.pp ppf c
  ;;

  let ( = ) p1 p2 =
    match p1, p2 with
    | Integer i1, Integer i2 -> Integer_plain.(i1 = i2)
    | Int32 i1, Int32 i2 -> Int32_plain.(i1 = i2)
    | String s1, String s2 -> String_plain.(s1 = s2)
    | Float f1, Float f2 -> Float_plain.(f1 = f2)
    | Char c1, Char c2 -> Char_plain.(c1 = c2)
    | _ -> false
  ;;

  let parse =
    let open Lvca_parsing in
    let module Ws = C_comment_parser in
    choice
      ~failure_msg:"looking for an integer, float, string, or character literal"
      [ (Ws.integer_or_float_lit
        >>| function First i -> Integer (Z.of_string i) | Second f -> Float f)
        (* Note: all ints parse to Integer *)
      ; (Ws.string_lit >>| fun s -> String s)
      ; (Ws.char_lit >>| fun c -> Char c)
      ]
    <?> "primitive"
  ;;

  let jsonify p =
    Json.(
      match p with
      | Integer i -> array [| string "i"; string (Z.to_string i) |]
      | Int32 i -> array [| string "i32"; string (Base.Int32.to_string i) |]
      | String s -> array [| string "s"; string s |]
      | Float f -> array [| string "f"; float f |]
      | Char c -> array [| string "c"; string (Base.Char.to_string c) |])
  ;;

  let unjsonify json =
    Json.(
      match json with
      | Array [| String "i"; String i |] ->
        (try Some (Integer (Z.of_string i)) with Failure _ -> None)
      | Array [| String "i32"; String i |] ->
        (try Some (Int32 (Base.Int32.of_string i)) with Failure _ -> None)
      | Array [| String "f"; Float f |] -> Some (Float f)
      | Array [| String "c"; String c |] ->
        if Base.Int.(Base.String.length c = 1)
        then Some (Char (Base.Char.of_string c))
        else None
      | Array [| String "s"; String str |] -> Some (String str)
      | _ -> None)
  ;;
end

module All = struct
  module All_kernel = Make (All_plain)
  include All_kernel

  let mk_String ?(provenance = Provenance.of_here [%here]) x =
    provenance, All_plain.String x
  ;;

  let mk_Float ?(provenance = Provenance.of_here [%here]) x =
    provenance, All_plain.Float x
  ;;

  let mk_Char ?(provenance = Provenance.of_here [%here]) x = provenance, All_plain.Char x

  let mk_Integer ?(provenance = Provenance.of_here [%here]) x =
    provenance, All_plain.Integer x
  ;;

  let mk_Int32 ?(provenance = Provenance.of_here [%here]) x =
    provenance, All_plain.Int32 x
  ;;

  let jsonify (_, p) = All_plain.jsonify p

  let unjsonify json =
    All_plain.unjsonify json
    |> Option.map ~f:(fun prim -> Provenance.of_here [%here], prim)
  ;;

  let check prim sort =
    match snd prim, sort with
    | All_plain.String _, Sort.Name (_, "string")
    | Float _, Sort.Name (_, "float")
    | Char _, Sort.Name (_, "char")
    | Integer _, Sort.Name (_, "integer")
    | Int32 _, Sort.Name (_, "int32") ->
      None
    | _, _ ->
      Some (Fmt.str "Unexpected sort (%a) for a primitive (%a)" Sort.pp sort pp prim)
  ;;

  module Properties = struct
    let to_string p = Fmt.to_to_string pp p
    let ( = ) = equivalent ~info_eq:(fun _ _ -> true)

    let json_round_trip1 : t -> Property_result.t =
     fun t ->
      match t with
      | _, All_plain.Float f when Base.Float.is_nan f -> Uninteresting
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

    let string_round_trip1 : t -> Property_result.t =
     fun t ->
      match t |> to_string |> Lvca_parsing.parse_string parse with
      | Ok prim -> Property_result.check (prim = t) (Fmt.str "%a <> %a" pp prim pp t)
      | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
   ;;

    (* Note: +1 -> 1. If the first round-trip isn't equal, try once more. *)
    let string_round_trip2 : string -> Property_result.t =
     fun str ->
      match Lvca_parsing.parse_string parse str with
      | Error _ -> Uninteresting
      | Ok prim ->
        let str' = to_string prim in
        if Base.String.(str' = str)
        then Ok
        else (
          match Lvca_parsing.parse_string parse str with
          | Error msg -> Failed msg
          | Ok prim' ->
            let str'' = to_string prim' in
            Property_result.check
              Base.String.(str'' = str')
              (Fmt.str {|"%s" <> "%s"|} str'' str'))
   ;;

    (* malformed input *)
  end
end

let%test_module "Parsing" =
  (module struct
    open All
    open Lvca_provenance

    let print_parse str =
      match Lvca_parsing.parse_string_pos parse str with
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
