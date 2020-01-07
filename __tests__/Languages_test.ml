open Jest
open Expect
module Parseable_abstract_syntax' = ParseStatus.Make(Parsing.Parseable_abstract_syntax)
module Parseable_concrete = ParseStatus.Make(Parsing.Parseable_concrete_syntax)
module Parseable_term' = ParseStatus.Make(Parsing.Parseable_term)

let can_parse_abstract language = (fun () ->
  let _, language =
    Parseable_abstract_syntax'.parse language
  in
  match language with
    | Belt.Result.Ok _ -> pass
    | Error msg -> fail msg
)

let can_parse_concrete language = (fun () ->
  let (_, concrete) =
    Parseable_concrete.parse language
  in
  match concrete with
    | Belt.Result.Ok _ -> pass
    | Error msg -> fail msg
)

let eval_str evaluator str =
  let _, tm = Parseable_term'.parse str in
  match tm with
    | Belt.Result.Ok tm' -> evaluator tm'
    | Error err -> Belt.Result.Error err

let evaluates_to evaluator str expected =
  expect (eval_str evaluator str) |> toEqual (Belt.Result.Ok expected)

let () = describe "Integer Language" (fun () ->
  test "parse abstract syntax" (can_parse_abstract LanguageInteger.abstractSyntax);
  test "parse concrete syntax" (can_parse_concrete LanguageInteger.concrete);

  let evaluates_to' str i = evaluates_to LanguageInteger.eval_tm str
    (Binding.Nominal.Primitive (PrimInteger (Bigint.of_int i)))
  in

  testAll "evaluate" [
    evaluates_to' "add(1; 2)" 3;
    evaluates_to' "add(1; 2)" 3;
    evaluates_to' "sub(1; 2)" (-1);
    evaluates_to' "abs(101)"  101;
    evaluates_to' "abs(-101)" 101;
    evaluates_to' "neg(101)"  (-101);
    evaluates_to' "neg(-101)" 101;
    evaluates_to' "min(1; 2)" 1;
    evaluates_to' "max(1; 2)" 2;
  ] Util.id;
)

let () = describe "JSON Language" (fun () ->
  test "parse abstract syntax" (can_parse_abstract LanguageJson.abstractSyntax);
  test "parse concrete syntax" (can_parse_concrete LanguageJson.concreteSyntax);
)

let () = describe "Document Language" (fun () ->
  test "parse abstract syntax" (can_parse_abstract LanguageDocument.abstractSyntax);
)

let () = describe "Lambda Calculus Language" (fun () ->
  test "parse abstract syntax" (can_parse_abstract LanguageLambda.abstractSyntax);
  test "parse concrete syntax" (can_parse_concrete LanguageLambda.concreteSyntax);
)
