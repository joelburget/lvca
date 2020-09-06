open Lvca_syntax

(* See the Bidirectional module for more tests *)
module Parse = Statics.Parse(ParseUtil.NoComment)

let print_parse desc =
  let str = ParseUtil.parse_string Parse.whitespace_t desc
    |> Base.Result.ok_or_failwith
    |> Fn.const "parsed"
  in
  print_string str

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
