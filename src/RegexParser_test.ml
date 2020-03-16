(* Expect round trip: to_string . parse = id *)
let expect_parse_and_rt str re =
  let re' = Regex_Parser.regex Regex_Lexer.read (Lexing.from_string str) in
  if re' <> re
  then
    failwith
      (Printf.sprintf "expect_parse_and_rt %s <> %s" (Regex.show re') (Regex.show re));
  if Regex.to_string re <> str
  then
    failwith
      (Printf.sprintf {|expect_parse_and_rt "%s" <> "%s"|} (Regex.to_string re) str)
;;

let%test_module "Regex_Parser" =
  (module struct
    open Regex
    open Classes

    let%test_unit "foo" = expect_parse_and_rt "foo" (re_str "foo")

    let%test_unit "(foo)(bar)" =
      expect_parse_and_rt "(foo)(bar)" (ReConcat [ re_str "foo"; re_str "bar" ])
    ;;

    let%test_unit "[a-z]" = expect_parse_and_rt "[a-z]" lower_alpha
    let%test_unit "[a-zA-Z]" = expect_parse_and_rt "[a-zA-Z]" alpha
    let%test_unit "(foo)*" = expect_parse_and_rt "(foo)*" (ReStar (re_str "foo"))
    let%test_unit "(foo)+" = expect_parse_and_rt "(foo)+" (RePlus (re_str "foo"))
    let%test_unit "" = expect_parse_and_rt "(foo)?" (ReOption (re_str "foo"))

    let%test_unit "foo|bar" =
      expect_parse_and_rt "foo|bar" (ReChoice [ re_str "foo"; re_str "bar" ])
    ;;

    let%test_unit "." = expect_parse_and_rt "." ReAny

    let%test_unit "[a-z][a-zA-Z0-9]*" =
      expect_parse_and_rt "[a-z][a-zA-Z0-9]*" (ReConcat [ lower_alpha; ReStar words ])
    ;;

    let%test_unit "([a-z][a-zA-Z0-9])*" =
      expect_parse_and_rt "([a-z][a-zA-Z0-9])*" (ReStar (ReConcat [ lower_alpha; words ]))
    ;;

    (*
    let%test_unit "[ ]+" =
      expect_parse_and_rt "[ ]+" (RePlus (ReSet [ SingleCharacter ' ' ]))
    ;;

    let%test_unit " +" = expect_parse_and_rt " +" (RePlus (ReChar ' '))
    *)
    let%test_unit {|\\|} = expect_parse_and_rt {|\\|} (ReChar '\\')
    let%test_unit {|\b|} = expect_parse_and_rt {|\b|} (ReClass (PosClass Boundary))
    let%test_unit {|\B|} = expect_parse_and_rt {|\B|} (ReClass (NegClass Boundary))
  end)
;;
