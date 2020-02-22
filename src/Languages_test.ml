module Parseable_concrete = Parsing.Incremental(Parsing.Parseable_concrete_syntax)
module Parseable_abstract_syntax = Parsing.Incremental(Parsing.Parseable_abstract_syntax)

let can_parse_abstract language =
  match Parseable_abstract_syntax.parse language with
    | Ok _ -> ()
    | Error msg -> failwith msg

let can_parse_concrete language =
  match Parseable_concrete.parse language with
    | Ok _ -> ()
    | Error msg -> failwith msg

let parses_to concrete_lang root str expected_str =
  let module Parseable_term = Parsing.Incremental(Parsing.Parseable_term) in
  match ConcreteSyntax.parse concrete_lang root str with
    | Error msg -> Printf.printf "%s\n" msg; false
    | Ok tree -> match ConcreteSyntax.to_ast concrete_lang tree with
      | Error msg -> Printf.printf "%s\n" msg; false
      | Ok ast -> match Parseable_term.parse expected_str with
        | Error msg -> Printf.printf "%s\n" msg; false
        | Ok expected_ast -> ast = expected_ast

let eval_str evaluator str =
  let module Parse = Parsing.Incremental(Parsing.Parseable_term) in
  match Parse.parse str with
    | Ok tm -> evaluator tm
    | Error err -> Error err

let evaluates_to evaluator str expected =
  eval_str evaluator str = Ok expected

let%test_module "Integer Language" = (module struct
  let abstractSyntax, concreteSyntax = LanguageInteger.(abstractSyntax, concreteSyntax)
  let%test_unit "parse abstract syntax" = can_parse_abstract abstractSyntax
  let%test_unit "parse concrete syntax" = can_parse_concrete concreteSyntax

  let evaluates_to' str i =
    evaluates_to LanguageInteger.eval_tm str
      (Binding.Nominal.Primitive (PrimInteger (Bigint.of_int i)))

  let%test "" = evaluates_to' "add(1; 2)" 3
  let%test "" = evaluates_to' "add(1; 2)" 3
  let%test "" = evaluates_to' "sub(1; 2)" (-1)
  let%test "" = evaluates_to' "abs(101)"  101
  let%test "" = evaluates_to' "abs(-101)" 101
  let%test "" = evaluates_to' "neg(101)"  (-101)
  let%test "" = evaluates_to' "neg(-101)" 101
  let%test "" = evaluates_to' "min(1; 2)" 1
  let%test "" = evaluates_to' "max(1; 2)" 2

  let terminal_rules, sort_rules = match Parseable_concrete.parse concreteSyntax with
    | Ok rules -> rules
    | Error msg -> failwith msg
  let concrete = ConcreteSyntax.make_concrete_description terminal_rules sort_rules

  let%test "parses to ..." = parses_to concrete "tm"
    "1" {|lit(1)|}
  let%test "parses to ..." = parses_to concrete "tm"
    "max 1 1" {|max(lit(1); lit(1))|}
  let%test "parses to ..." = parses_to concrete "tm"
    "1 - 1" {|sub(lit(1); lit(1))|}
  let%test "parses to ..." = parses_to concrete "tm"
    "|1|" {|abs(lit(1))|}
  let%test "parses to ..." = parses_to concrete "tm"
    "-1" {|neg(lit(1))|}
  let%test "parses to ..." = parses_to concrete "tm"
    "|-1|" {|abs(neg(lit(1)))|}
end)

let%test_module "JSON Language" = (module struct
  let abstractSyntax, concreteSyntax = LanguageJson.(abstractSyntax, concreteSyntax)
  let%test_unit "parse abstract syntax" = can_parse_abstract abstractSyntax
  let%test_unit "parse concrete syntax" = can_parse_concrete concreteSyntax

  (*
  let abstract = match Parseable_abstract_syntax.parse abstractSyntax with
    | _, Ok abstract -> abstract
    | _, Error msg -> failwith msg
  in
  let terminal_rules, sort_rules = match Parseable_concrete.parse concreteSyntax with
    | _, Ok rules -> rules
    | _, Error msg -> failwith msg
  in
  let concrete = ConcreteSyntax.make_concrete_description terminal_rules sort_rules in

  let%test "parses to ..." (parses_to concrete "json" {|null|} {|null())|});
  let%test "parses to ..." (parses_to concrete "json" {|true|} {|bool(true())|});
  let%test "parses to ..." (parses_to concrete "json" {|"foo"|} {|"foo"|});
  *)
end)

let%test_module "Document Language" = (module struct
 let%test_unit "parse abstract syntax" =
   can_parse_abstract LanguageDocument.abstractSyntax
  (* let%test_unit "parse concrete syntax" (can_parse_concrete LanguageDocument.concreteSyntax); *)
end)

let%test_module "Lambda Calculus Language" = (module struct
  let%test_unit "parse abstract syntax" =
    can_parse_abstract LanguageLambda.abstractSyntax
  let%test_unit "parse concrete syntax" =
    can_parse_concrete LanguageLambda.concreteSyntax
end)

(*
let%test_module "SVG Language" = (module struct
  let%test_unit "parse abstract syntax" = can_parse_abstract LanguageSvg.abstractSyntax
  let%test_unit "parse concrete syntax" = can_parse_concrete LanguageSvg.concreteSyntax
end)
*)
