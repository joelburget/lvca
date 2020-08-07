(* See the Bidirectional module for more tests *)
module Parse = Statics.Parse(Lvca_util.Angstrom.NoComment)

let print_parse desc =
  let str =
    match
      Angstrom.parse_string ~consume:All
        Angstrom.(Lvca_util.Angstrom.whitespace *> Parse.t)
        desc
    with
      | Error err -> err
      | Ok _lang -> "parsed"
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