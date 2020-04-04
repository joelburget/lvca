open Core_kernel

module Parse_statics = Parsing.Incremental (Parsing.Parseable_statics)
(* open Statics *)

let print_parse desc =
  print_string (match Parse_statics.parse desc with
    | Error msg -> msg
    | Ok _lang -> "parsed")

let%expect_test _ =
  print_parse {|
    ---
    ctx >> tm => ty
  |};
  [%expect{| parsed |}]

let%expect_test _ =
  print_parse {|
    ctx, x : a >> foo(x) <= a
    ---
    ctx >> tm => ty
  |};
  [%expect{| parsed |}]
