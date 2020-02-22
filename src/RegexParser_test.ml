(* Expect round trip: to_string . parse = id *)
let expect_parse_and_rt str re =
  let re' = Regex_Parser.regex Regex_Lexer.read (Lexing.from_string str) in
  assert (re' = re);
  assert (Regex.to_string re = str)

let%test_module "Regex_Parser" = (module struct
  let%test_unit "" = expect_parse_and_rt "foo" (ReString "foo")
  (* let%test_unit "" = expect_parse_and_rt "(foo)(bar)" *)
  (*   (ReConcat [ReString "foo"; ReString "bar"]) *)
  let%test_unit "" = expect_parse_and_rt "[a-z]"    (ReSet "a-z")
  let%test_unit "" = expect_parse_and_rt "[a-zA-Z]" (ReSet "a-zA-Z")
  (* let%test_unit "" = expect_parse_and_rt "(foo)*" (ReStar (ReString "foo")) *)
  (* let%test_unit "" = expect_parse_and_rt "(foo)+" (RePlus (ReString "foo")) *)
  (* let%test_unit "" = expect_parse_and_rt "(foo)?" *)
  (*   (ReOption (ReString "foo")) *)
  let%test_unit "" = expect_parse_and_rt "foo|bar"
    (ReChoice [ReString "foo"; ReString "bar"])
  let%test_unit "" = expect_parse_and_rt "." ReAny

  let%test_unit "" = expect_parse_and_rt "[a-z][a-zA-Z0-9]*"
    (ReConcat [ReSet "a-z"; ReStar (ReSet "a-zA-Z0-9")])
  let%test_unit "" = expect_parse_and_rt "([a-z][a-zA-Z0-9])*"
    (ReStar (ReConcat [ReSet "a-z"; ReSet "a-zA-Z0-9"]))

  let%test_unit "" = expect_parse_and_rt "[ ]+" (RePlus (ReSet " "))
  let%test_unit "" = expect_parse_and_rt " +" (RePlus (ReString " "))

  (* let%test_unit "" = expect_parse_and_rt {|\\|} (ReString {|\|}) *)
  let%test_unit "" = expect_parse_and_rt {|\b|}
    (ReClass (PosClass Boundary))
  let%test_unit "" = expect_parse_and_rt {|\B|}
    (ReClass (NegClass Boundary))

end)
