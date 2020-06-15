(* See the Bidirectional module for more tests *)

let print_parse desc =
  let module Parse = Statics.Parse(struct
    let comment = Angstrom.fail "no comment"
    let reserved = Util.String.Set.empty
  end) in
  print_string (
    match
      Angstrom.parse_string ~consume:All
        Angstrom.(Util.Angstrom.whitespace *> Parse.t)
        desc
    with
      | Error err -> err
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
