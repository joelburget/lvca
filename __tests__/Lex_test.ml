open Jest
open Expect
open Lex

let _ = describe "Lex" (fun () ->
  let lexer1 =
    [ "if",                    "IF";
      "then",                  "THEN";
      "else",                  "ELSE";
      "<|>|<=|>=|==|!=",       "OP";
      "[a-zA-Z][a-zA-Z0-9_]*", "ID";
      "\\d+",                  "NUM";
      "\".*\"",                "LIT";
      "\\s+",                  "WHITE";
    ]
  in

  let mk_tok name start finish = { name; start; finish } in
  let result = lex lexer1 "if a > b then 90 else 91" in
                        (* 012345678901234567890123 *)

  test "lex 1" (fun () ->
    expect result
      |> toEqual (Belt.Result.Ok
      [| mk_tok "IF" 0 2;
         mk_tok "WHITE" 2 3;
         mk_tok "ID" 3 4;
         mk_tok "WHITE" 4 5;
         mk_tok "OP" 5 6;
         mk_tok "WHITE" 6 7;
         mk_tok "ID" 7 8;
         mk_tok "WHITE" 8 9;
         mk_tok "THEN" 9 13;
         mk_tok "WHITE" 13 14;
         mk_tok "NUM" 14 16;
         mk_tok "WHITE" 16 17;
         mk_tok "ELSE" 17 21;
         mk_tok "WHITE" 21 22;
         mk_tok "NUM" 22 24;
      |]
    );
  );

  let lexer2 =
    [ "\\+", "+";
      "\\*", "*";
      "\\(", "(";
      "\\)", ")";
      "\w+", "id";
      "\\s+", "WHITE";
    ]
  in
  let result = lex lexer2 "foo + bar" in
                        (* 012345678 *)

  test "lex 2" (fun () ->
    expect result |> toEqual (Belt.Result.Ok
      [| mk_tok "id" 0 3;
         mk_tok "WHITE" 3 4;
         mk_tok "+" 4 5;
         mk_tok "WHITE" 5 6;
         mk_tok "id" 6 9;
      |]
    );
  );
)
