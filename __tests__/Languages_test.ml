open Jest
open Expect
module Parseable_abstract_syntax' = ParseStatus.Make(Parsing.Parseable_abstract_syntax)
module Parseable_concrete = ParseStatus.Make(Parsing.Parseable_concrete_syntax)
module Parseable_term' = ParseStatus.Make(Parsing.Parseable_term)

let can_parse_abstract language = fun () ->
  let _, language = Parseable_abstract_syntax'.parse language in
  match language with
    | Ok _ -> pass
    | Error msg -> fail msg

let can_parse_concrete language = fun () ->
  let _, concrete = Parseable_concrete.parse language in
  match concrete with
    | Ok _ -> pass
    | Error msg -> fail msg

let parses_to concrete_lang root str expected_str = fun () ->
  match ConcreteSyntax.parse concrete_lang root str with
    | Error msg -> fail msg
    | Ok tree -> match ConcreteSyntax.to_ast concrete_lang tree with
      | Error msg -> fail msg
      | Ok ast -> match Parseable_term'.parse expected_str with
        | _, Error msg -> fail msg
        | _, Ok expected_ast -> expect ast |> toEqual expected_ast

let eval_str evaluator str =
  let _, tm = Parseable_term'.parse str in
  match tm with
    | Ok tm' -> evaluator tm'
    | Error err -> Error err

let evaluates_to evaluator str expected =
  expect (eval_str evaluator str) |> toEqual (Ok expected)

let () = describe "Integer Language" (fun () ->
  let abstractSyntax, concreteSyntax = LanguageInteger.(abstractSyntax, concreteSyntax) in
  test "parse abstract syntax" (can_parse_abstract abstractSyntax);
  test "parse concrete syntax" (can_parse_concrete concreteSyntax);

  let evaluates_to' str i = test str (fun () ->
    evaluates_to LanguageInteger.eval_tm str
      (Binding.Nominal.Primitive (PrimInteger (Bigint.of_int i)))
  )
  in

  describe "evaluate" (fun () ->
    evaluates_to' "add(1; 2)" 3;
    evaluates_to' "add(1; 2)" 3;
    evaluates_to' "sub(1; 2)" (-1);
    evaluates_to' "abs(101)"  101;
    evaluates_to' "abs(-101)" 101;
    evaluates_to' "neg(101)"  (-101);
    evaluates_to' "neg(-101)" 101;
    evaluates_to' "min(1; 2)" 1;
    evaluates_to' "max(1; 2)" 2;
  );

  let terminal_rules, sort_rules = match Parseable_concrete.parse concreteSyntax with
    | _, Ok rules -> rules
    | _, Error msg -> failwith msg
  in
  let concrete = ConcreteSyntax.make_concrete_description terminal_rules sort_rules in

  test "parses to ..." (parses_to concrete "tm"
    "1" {|lit(1)|});
  test "parses to ..." (parses_to concrete "tm"
    "max 1 1" {|max(lit(1); lit(1))|});
  test "parses to ..." (parses_to concrete "tm"
    "1 - 1" {|sub(lit(1); lit(1))|});
  test "parses to ..." (parses_to concrete "tm"
    "|1|" {|abs(lit(1))|});
  test "parses to ..." (parses_to concrete "tm"
    "-1" {|neg(lit(1))|});
  test "parses to ..." (parses_to concrete "tm"
    "|-1|" {|abs(neg(lit(1)))|});
)

let () = describe "JSON Language" (fun () ->
  let abstractSyntax, concreteSyntax = LanguageJson.(abstractSyntax, concreteSyntax) in
  test "parse abstract syntax" (can_parse_abstract abstractSyntax);
  test "parse concrete syntax" (can_parse_concrete concreteSyntax);

  (*
  let abstract = match Parseable_abstract_syntax'.parse abstractSyntax with
    | _, Ok abstract -> abstract
    | _, Error msg -> failwith msg
  in
  let terminal_rules, sort_rules = match Parseable_concrete.parse concreteSyntax with
    | _, Ok rules -> rules
    | _, Error msg -> failwith msg
  in
  let concrete = ConcreteSyntax.make_concrete_description terminal_rules sort_rules in

  test "parses to ..." (parses_to concrete "json" {|null|} {|null())|});
  test "parses to ..." (parses_to concrete "json" {|true|} {|bool(true())|});
  test "parses to ..." (parses_to concrete "json" {|"foo"|} {|"foo"|});
  *)
)

let () = describe "Document Language" (fun () ->
  test "parse abstract syntax" (can_parse_abstract LanguageDocument.abstractSyntax);
  (* test "parse concrete syntax" (can_parse_concrete LanguageDocument.concreteSyntax); *)
)

let () = describe "Lambda Calculus Language" (fun () ->
  test "parse abstract syntax" (can_parse_abstract LanguageLambda.abstractSyntax);
  test "parse concrete syntax" (can_parse_concrete LanguageLambda.concreteSyntax);
)

let () = describe "SVG Language" (fun () ->
  test "parse abstract syntax" (can_parse_abstract LanguageSvg.abstractSyntax);
  (* test "parse concrete syntax" (can_parse_concrete LanguageSvg.concreteSyntax); *)
)
