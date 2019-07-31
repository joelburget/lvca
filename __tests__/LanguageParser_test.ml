open Jest
open Expect
open Types

let _ = describe "LanguageParser" (fun () ->
  let expectParse str lang = test ("'" ^ str ^ "'") (fun () ->
    expect (LanguageParser.language_def LanguageLexer.read (Lexing.from_string str))
    |> toEqual lang
  ) in

  let module M = Belt.Map.String in
  let module BL = Belt.List in

  expectParse "bool := true() | false()"
    (Language (M.fromArray [|
      "bool", SortDef
      ([], [
        OperatorDef ("true", Arity ([], []));
        OperatorDef ("false", Arity ([], []));
      ])
    |]));

  let tm_sort = SortAp ("tm", [||]) in
  let tm_valence = FixedValence ([], tm_sort) in
  let ty_sort = SortAp ("ty", [||]) in
  let ty_valence = FixedValence ([], ty_sort) in

  expectParse {|
  ty :=
    | bool()
    | arr(ty(); ty())

  tm :=
    | app(tm(); tm())
    | lam(tm(). tm())
  |}
    (Language (M.fromArray [|
      "ty", SortDef
      ([], [
        OperatorDef ("bool", Arity ([], []));
        OperatorDef ("arr", Arity ([], [ ty_valence; ty_valence ]));
      ]);

      "tm", SortDef
      ([], [
        OperatorDef ("app", Arity ([], [ tm_valence; tm_valence ]));
        OperatorDef ("lam", Arity ([], [ FixedValence ([tm_sort], tm_sort) ]));
      ]);
    |]));
)