open Jest
open Expect
module Parseable_abstract_syntax' = ParseStatus.Make(Parsing.Parseable_abstract_syntax)
module Parseable_concrete = ParseStatus.Make(Parsing.Parseable_concrete_syntax)
module Parseable_term' = ParseStatus.Make(Parsing.Parseable_term)

let () = describe "Integer Language" (fun () ->
  test "parse abstract syntax" (fun () ->
    let (_, language) =
      Parseable_abstract_syntax'.parse LanguageInteger.abstractSyntax
    in
    match language with
      | Belt.Result.Ok _ -> pass
      | Error msg -> fail msg
  );

  (*
  test "parse concrete syntax" (fun () ->
    let (_, concrete) =
      Parseable_concrete.parse LanguageInteger.concrete
    in
    match concrete with
      | Belt.Result.Ok _ -> pass
      | Error msg -> fail msg
  );
  *)

  let eval_str str =
    let _, tm = Parseable_term'.parse str in
    match tm with
      | Ok tm' -> LanguageInteger.eval_tm tm'
      | Error err -> Error err
  in

  let mk_result_int i = Belt.Result.Ok
    (Binding.Nominal.Primitive (PrimInteger (Bigint.of_int i)))
  in

  testAll "evaluate" [
    expect (eval_str "add(1; 2)") |> toEqual (mk_result_int 3);
    expect (eval_str "sub(1; 2)") |> toEqual (mk_result_int (-1));
    expect (eval_str "abs(101)")  |> toEqual (mk_result_int 101);
    expect (eval_str "abs(-101)") |> toEqual (mk_result_int 101);
    expect (eval_str "neg(101)")  |> toEqual (mk_result_int (-101));
    expect (eval_str "neg(-101)") |> toEqual (mk_result_int 101);
    expect (eval_str "min(1; 2)") |> toEqual (mk_result_int 1);
    expect (eval_str "max(1; 2)") |> toEqual (mk_result_int 2);
  ] Util.id;
)

let () = describe "Document Language" (fun () ->
  test "parse abstract syntax" (fun () ->
    let (_, language) =
      Parseable_abstract_syntax'.parse LanguageDocument.abstractSyntax
    in
    match language with
      | Belt.Result.Ok _ -> pass
      | Error msg -> fail msg
  );
)
