open Jest
open Expect

let _ = describe "Regex_Parser" (fun () ->
  (* Expect round trip: to_string . parse = id *)
  let expectParseAndRT parser str re = test ("parse '" ^ str ^ "'") (fun () ->
    match parser Regex_Lexer.read (Lexing.from_string str) with
      re' -> expect re' |> toEqual re;
      expect (Regex.to_string re) |> toEqual str
  ) in

  expectParseAndRT Regex_Parser.regex__test "foo" (ReString "foo");
  expectParseAndRT Regex_Parser.regex__test "(foo)(bar)"
    (ReConcat [ReString "foo"; ReString "bar"]);
  expectParseAndRT Regex_Parser.regex__test "[a-z]"    (ReSet "a-z");
  expectParseAndRT Regex_Parser.regex__test "[a-zA-Z]" (ReSet "a-zA-Z");
  expectParseAndRT Regex_Parser.regex__test "[a-z][A-Z]"
    (ReConcat [ReSet "a-z"; ReSet "A-Z"]);
  expectParseAndRT Regex_Parser.regex__test "(foo)*" (ReStar (ReString "foo"));
  expectParseAndRT Regex_Parser.regex__test "(foo)+" (RePlus (ReString "foo"));
  expectParseAndRT Regex_Parser.regex__test "(foo)?"
    (ReOption (ReString "foo"));
  expectParseAndRT Regex_Parser.regex__test "foo|bar"
    (ReChoice (ReString "foo", ReString "bar"));
  expectParseAndRT Regex_Parser.regex__test "." ReAny;

  expectParseAndRT Regex_Parser.regex__test {|\\|} (ReString {|\|});
  expectParseAndRT Regex_Parser.regex__test {|\b|}
    (ReClass (PosClass Boundary));
  expectParseAndRT Regex_Parser.regex__test {|\B|}
    (ReClass (NegClass Boundary));

);
