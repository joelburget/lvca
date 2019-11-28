open Jest
open Expect

let _ = describe "Regex_Parser" (fun () ->
  (* Expect round trip: to_string . parse = id *)
  let expectParseAndRT parser str re = test ("parse '" ^ str ^ "'") (fun () ->
    match parser Regex_Lexer.read (Lexing.from_string str) with
      re' -> expect re' |> toEqual re;
      expect (Regex.to_string re) |> toEqual str
  ) in

  expectParseAndRT Regex_Parser.regex "foo" (ReString "foo");
  expectParseAndRT Regex_Parser.regex "(foo)(bar)"
    (ReConcat [ReString "foo"; ReString "bar"]);
  expectParseAndRT Regex_Parser.regex "[a-z]"    (ReSet "a-z");
  expectParseAndRT Regex_Parser.regex "[a-zA-Z]" (ReSet "a-zA-Z");
  expectParseAndRT Regex_Parser.regex "(foo)*" (ReStar (ReString "foo"));
  expectParseAndRT Regex_Parser.regex "(foo)+" (RePlus (ReString "foo"));
  expectParseAndRT Regex_Parser.regex "(foo)?"
    (ReOption (ReString "foo"));
  expectParseAndRT Regex_Parser.regex "foo|bar"
    (ReChoice (ReString "foo", ReString "bar"));
  expectParseAndRT Regex_Parser.regex "." ReAny;

  expectParseAndRT Regex_Parser.regex "[a-z][a-zA-Z0-9]*"
    (ReConcat [ReSet "a-z"; ReStar (ReSet "a-zA-Z0-9")]);
  expectParseAndRT Regex_Parser.regex "([a-z][a-zA-Z0-9])*"
    (ReStar (ReConcat [ReSet "a-z"; ReSet "a-zA-Z0-9"]));

  expectParseAndRT Regex_Parser.regex "[ ]+" (RePlus (ReSet " "));
  expectParseAndRT Regex_Parser.regex " +" (RePlus (ReString " "));

  expectParseAndRT Regex_Parser.regex {|\\|} (ReString {|\|});
  expectParseAndRT Regex_Parser.regex {|\b|}
    (ReClass (PosClass Boundary));
  expectParseAndRT Regex_Parser.regex {|\B|}
    (ReClass (NegClass Boundary));

);
