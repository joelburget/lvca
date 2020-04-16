open Core_kernel

(* See the Bidirectional module for more tests *)

let print_parse desc =
  print_string (match Parsing.Statics.parse desc with
    | Error err -> ParseError.to_string err
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
