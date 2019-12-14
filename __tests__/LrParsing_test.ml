open Jest
open Expect
open LrParsing
module M = Belt.Map.Int
module MS = Belt.Map.String
module SI = Belt.Set.Int
module MStack = Belt.MutableStack
module MQueue = Belt.MutableQueue

module Grammar : GRAMMAR = struct
  let grammar = {
    nonterminals = M.fromArray
    [|
       (* E' (note: the grammar we provide is already augmented) *)
       0, { productions = [[Nonterminal 1]] }; (* E' -> E *)
       (* E *)
       1, { productions = [
         [Nonterminal 1; Terminal 1; Nonterminal 2]; (* E -> E + T *)
         [Nonterminal 2];                            (* E -> T *)
         ]
       };
       (* T *)
       2, { productions = [
         [Nonterminal 2; Terminal 2; Nonterminal 3]; (* T -> T * F *)
         [Nonterminal 3]                             (* T -> F *)
         ]
       };
       (* F *)
       3, { productions = [
         [Terminal 3; Nonterminal 1; Terminal 4]; (* F -> (E) *)
         [Terminal 5];                            (* F -> id *)
         ]
       };
    |];
    terminal_nums =
    [|
      "$", 0;
      "+", 1;
      "*", 2;
      "(", 3;
      ")", 4;
      "id", 5;
    |];
    nonterminal_nums =
    [|
      "E'", 0;
      "E", 1;
      "T", 2;
      "F", 3;
    |];
  }
end

let () = describe "LrParsing" (fun () ->

  let module Lr0' = Lr0(Grammar) in

  testAll "mk_item / view_item" [
    expect (view_item @@ mk_item' 0 0)
      |> toEqual { production_num = 0; position = 0 };
    expect (view_item @@ mk_item' 0 1)
      |> toEqual { production_num = 0; position = 1 };
    expect (view_item @@ mk_item' 1 0)
      |> toEqual { production_num = 1; position = 0 };
    expect (view_item @@ mk_item' 1 1)
      |> toEqual { production_num = 1; position = 1 };
  ] Util.id;

  testAll "in_first" [
    (* for terminals X, First(X) = {X} *)
    expect (Lr0'.in_first 1 (Terminal 1)) |> toBe true;
    expect (Lr0'.in_first 0 (Nonterminal 0)) |> toBe false;
    expect (Lr0'.in_first 1 (Nonterminal 0)) |> toBe false;
    expect (Lr0'.in_first 2 (Nonterminal 0)) |> toBe false;
    expect (Lr0'.in_first 3 (Nonterminal 0)) |> toBe true;
    expect (Lr0'.in_first 4 (Nonterminal 0)) |> toBe false;
  ] Util.id;

  testAll "in_follow" [
    (* $ is in the follow set for the start symbol *)
    expect (Lr0'.in_follow 0 0) |> toBe true;
    expect (Lr0'.in_follow 1 0) |> toBe false;
    expect (Lr0'.in_follow 2 0) |> toBe false;
    expect (Lr0'.in_follow 3 0) |> toBe false;
    expect (Lr0'.in_follow 4 0) |> toBe false;

    (* '$', '+', and ')' follow E, '*' and '(' don't *)
    expect (Lr0'.in_follow 0 1) |> toBe true;
    expect (Lr0'.in_follow 1 1) |> toBe true;
    expect (Lr0'.in_follow 2 1) |> toBe false;
    expect (Lr0'.in_follow 3 1) |> toBe false;
    expect (Lr0'.in_follow 4 1) |> toBe true;

    (* '$', '+', '*', and ')' follow T, '(' doesn't *)
    expect (Lr0'.in_follow 0 2) |> toBe true;
    expect (Lr0'.in_follow 1 2) |> toBe true;
    expect (Lr0'.in_follow 2 2) |> toBe true;
    expect (Lr0'.in_follow 3 2) |> toBe false;
    expect (Lr0'.in_follow 4 2) |> toBe true;

    (* '$', '+', '*', and ')' follow F, '(' doesn't *)
    expect (Lr0'.in_follow 0 3) |> toBe true;
    expect (Lr0'.in_follow 1 3) |> toBe true;
    expect (Lr0'.in_follow 2 3) |> toBe true;
    expect (Lr0'.in_follow 3 3) |> toBe false;
    expect (Lr0'.in_follow 4 3) |> toBe true;
  ] Util.id;

  (* I0 *)
  let items0 = [| mk_item' 0 0 |] in
  let expected0 : configuration_set =
    { kernel_items = SI.fromArray items0;
      nonkernel_items = SI.fromArray
        [| mk_item' 1 0;
           mk_item' 2 0;
           mk_item' 3 0;
           mk_item' 4 0;
           mk_item' 5 0;
           mk_item' 6 0;
        |]
    }
  in

  (* I1 *)
  let items1 = [|
    mk_item' 0 1;
    mk_item' 1 1;
  |]
  in

  let expected1 : configuration_set =
    { kernel_items = SI.fromArray items1;
      nonkernel_items = SI.fromArray [||];
    }
  in

  (* I7 *)
  let items7 = [| mk_item' 3 2 |] in
  let expected7 : configuration_set =
    { kernel_items = SI.fromArray items7;
      nonkernel_items = SI.fromArray [| mk_item' 5 0; mk_item' 6 0 |];
    }
  in

  testAll "closure" [
    expect (Lr0'.lr0_closure' @@ SI.fromArray items0)
      |> toEqual expected0;
    expect (Lr0'.lr0_closure' @@ SI.fromArray items1)
      |> toEqual expected1;
    expect (Lr0'.lr0_closure' @@ SI.fromArray items7)
      |> toEqual expected7;
  ] Util.id;

  let lr0_goto_kernel = SI.fromArray [| mk_item' 1 2 |] in

  let goto_nonkernel = SI.fromArray [|
    mk_item' 3 0;
    mk_item' 4 0;
    mk_item' 5 0;
    mk_item' 6 0;
  |]
  in

  testAll "goto" [
    expect (Lr0'.lr0_goto_kernel (SI.fromArray items1) (Terminal 1))
      |> toEqual lr0_goto_kernel;
    expect
      (Lr0'.lr0_closure' @@
       Lr0'.lr0_goto_kernel (SI.fromArray items1) (Terminal 1))
      |> toEqual
      ({ kernel_items = lr0_goto_kernel; nonkernel_items = goto_nonkernel }
        : configuration_set);
  ] Util.id;

  let lr0_item_sets = [|
    SI.fromArray [| mk_item' 0 0 |]; (* 0 *)
    SI.fromArray (* 1 *)
      [| mk_item' 0 1;
         mk_item' 1 1;
      |];
    SI.fromArray (* 2 *)
      [| mk_item' 2 1;
         mk_item' 3 1;
      |];
    SI.fromArray [| mk_item' 4 1; |]; (* 3 *)
    SI.fromArray [| mk_item' 5 1; |]; (* 4 *)
    SI.fromArray [| mk_item' 6 1; |]; (* 5 *)
    SI.fromArray [| mk_item' 1 2; |]; (* 6 *)
    SI.fromArray [| mk_item' 3 2; |]; (* 7 *)
    SI.fromArray (* 8 *)
      [|
        mk_item' 1 1;
        mk_item' 5 2;
      |];
    SI.fromArray (* 9 *)
      [|
        mk_item' 1 3;
        mk_item' 3 1;
      |];
    SI.fromArray [| mk_item' 3 3 |]; (* 10 *)
    SI.fromArray [| mk_item' 5 3 |]; (* 11 *)
  |]
  in

  let expected_lr0_item_sets =
    Belt.MutableSet.fromArray lr0_item_sets ~id:(module ComparableIntSet)
  in

  let normalize = fun items -> items
    |. Belt.MutableSet.toList
    |. L.map SI.toList
  in

  testAll "lr0_items" [
    expect (normalize Lr0'.mutable_lr0_items)
      |> toEqual (normalize expected_lr0_item_sets);
    (* TODO
    expect (M.get Lr0'.items' 1 == Some (SI.fromArray items1)) |> toBe true;
    expect (M.get Lr0'.items' 7 == Some (SI.fromArray items7)) |> toBe true;
    *)
  ] Util.id;

  let state = lr0_item_sets |. Belt.Array.map Lr0'.item_set_to_state in
  let plus_num : terminal_num = 1 in
  let times_num : terminal_num = 2 in
  let lparen_num : terminal_num = 3 in
  let rparen_num : terminal_num = 4 in
  (* TODO: use these up above *)
  let id_num : terminal_num = 5 in
  let e_num : nonterminal_num = 1 in
  let t_num : nonterminal_num = 2 in
  let f_num : nonterminal_num = 3 in
  (* Question: since these are all Nonterminals, what's the correct type of
   * goto? *)
  (* Test for a match with CPTT Figure 4.37 *)
  testAll "lr0_goto_table" [
    expect (Lr0'.lr0_goto_table state.(0) (Nonterminal e_num)) |> toEqual (Some state.(1));
    expect (Lr0'.lr0_goto_table state.(0) (Nonterminal t_num)) |> toEqual (Some state.(2));
    expect (Lr0'.lr0_goto_table state.(0) (Nonterminal f_num)) |> toEqual (Some state.(3));
    expect (Lr0'.lr0_goto_table state.(4) (Nonterminal e_num)) |> toEqual (Some state.(8));
    expect (Lr0'.lr0_goto_table state.(4) (Nonterminal t_num)) |> toEqual (Some state.(2));
    expect (Lr0'.lr0_goto_table state.(4) (Nonterminal f_num)) |> toEqual (Some state.(3));
    (* TODO: test other invalid GOTOs *)
    (* TODO: should this throw? *)
    expect (Lr0'.lr0_goto_table state.(6) (Nonterminal e_num)) |> toEqual None;
    expect (Lr0'.lr0_goto_table state.(6) (Nonterminal t_num)) |> toEqual (Some state.(9));
    expect (Lr0'.lr0_goto_table state.(6) (Nonterminal f_num)) |> toEqual (Some state.(3));
    expect (Lr0'.lr0_goto_table state.(7) (Nonterminal e_num)) |> toEqual None;
    expect (Lr0'.lr0_goto_table state.(7) (Nonterminal t_num)) |> toEqual None;
    expect (Lr0'.lr0_goto_table state.(7) (Nonterminal f_num)) |> toEqual (Some state.(10));
  ] Util.id;

  (* Test for a match with CPTT Figure 4.37 *)
  let action_table_tests =
    [ 0, id_num,     Shift state.(5);
      0, plus_num,   Error None;
      0, times_num,  Error None;
      0, lparen_num, Shift state.(4);
      0, rparen_num, Error None;
      0, 0,          Error None;

      1, id_num,     Error None;
      1, plus_num,   Shift state.(6);
      1, times_num,  Error None;
      1, lparen_num, Error None;
      1, rparen_num, Error None;
      1, 0,          Accept;

      2, id_num,     Error None;
      2, plus_num,   Reduce 2;
      2, times_num,  Shift state.(7);
      2, lparen_num, Error None;
      2, rparen_num, Reduce 2;
      2, 0,          Reduce 2;

      3, id_num,     Error None;
      3, plus_num,   Reduce 4;
      3, times_num,  Reduce 4;
      3, lparen_num, Error None;
      3, rparen_num, Reduce 4;
      3, 0,          Reduce 4;

      4, id_num,     Shift state.(5);
      4, plus_num,   Error None;
      4, times_num,  Error None;
      4, lparen_num, Shift state.(4);
      4, rparen_num, Error None;
      4, 0,          Error None;

      5, id_num,     Error None;
      5, plus_num,   Reduce 6;
      5, times_num,  Reduce 6;
      5, lparen_num, Error None;
      5, rparen_num, Reduce 6;
      5, 0,          Reduce 6;

      6, id_num,     Shift state.(5);
      6, plus_num,   Error None;
      6, times_num,  Error None;
      6, lparen_num, Shift state.(4);
      6, rparen_num, Error None;
      6, 0,          Error None;

      7, id_num,     Shift state.(5);
      7, plus_num,   Error None;
      7, times_num,  Error None;
      7, lparen_num, Shift state.(4);
      7, rparen_num, Error None;
      7, 0,          Error None;

      8, id_num,     Error None;
      8, plus_num,   Shift state.(6);
      8, times_num,  Error None;
      8, lparen_num, Error None;
      8, rparen_num, Shift state.(11);
      8, 0,          Error None;

      9, id_num,     Error None;
      9, plus_num,   Reduce 1;
      9, times_num,  Shift state.(7);
      9, lparen_num, Error None;
      9, rparen_num, Reduce 1;
      9, 0,          Reduce 1;

      10, id_num,     Error None;
      10, plus_num,   Reduce 3;
      10, times_num,  Reduce 3;
      10, lparen_num, Error None;
      10, rparen_num, Reduce 3;
      10, 0,          Reduce 3;

      11, id_num,     Error None;
      11, plus_num,   Reduce 5;
      11, times_num,  Reduce 5;
      11, lparen_num, Error None;
      11, rparen_num, Reduce 5;
      11, 0,          Reduce 5;

    ]
  in
  let action_table_tests' = action_table_tests
    |. Belt.List.map (fun (init_state, terminal_num, action) ->
      expect (Lr0'.lr0_action_table state.(init_state) terminal_num)
        |> toEqual action
    )
  in
  testAll "lr0_action_table" action_table_tests' Util.id;

  let mk_tok name start finish : Lex.token = { name; start; finish } in
  let mk_terminal num start_pos end_pos =
    { production = Either.Left num; children = []; start_pos; end_pos; }
  in
  let mk_wrapper prod_num ({start_pos; end_pos} as child) =
    { production = Either.Right prod_num;
      children = [ child ];
      start_pos;
      end_pos;
    }
  in

  (* foo * bar
   * 0123456789
   *)
  let tokens1 = MQueue.fromArray [|
    mk_tok "id" 0 3;
    mk_tok "*"  4 5;
    mk_tok "id" 6 9;
    mk_tok "$"  9 9;
  |]
  in

  (* foo * bar + baz
   * 0123456789012345
   *)
  let tokens2 = MQueue.fromArray [|
    mk_tok "id" 0 3;
    mk_tok "*"  4 5;
    mk_tok "id" 6 9;
    mk_tok "+"  10 11;
    mk_tok "id" 12 15;
    mk_tok "$"  15 15;
  |]
  in

  (* foo * bar + baz
   * 0123456789012345
   *)
  let tokens3 = MQueue.fromArray [|
    mk_tok "id" 0 3;
    mk_tok "*"  4 5;
  |]
  in

  (* foo + bar
   * 0123456789
   *)
  let tokens4 = MQueue.fromArray [|
    mk_tok "id" 0 3;
    mk_tok "+"  4 5;
    mk_tok "id" 6 9;
    mk_tok "$"  9 9;
  |]
  in

  (* TODO: test failed parses *)
  testAll "parse" [

    expect (Lr0'.parse (* "foo * bar" *) tokens1) |> toEqual (Result.Ok
      (mk_wrapper 2
        { production = Either.Right 3;
          children = [
            mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 0 3;
            mk_terminal times_num 4 5;
            mk_wrapper 6 @@ mk_terminal id_num 6 9;
          ];
          start_pos = 0;
          end_pos = 9;
        }));

    (* Figure 4.38 from CPTT *)
    expect (Lr0'.parse (* "foo * bar + baz" *) tokens2) |> toEqual (Result.Ok
      { production = Either.Right 1;
        children =
          [ mk_wrapper 2
            { production = Either.Right 3;
              children = [
                mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 0 3;
                mk_terminal times_num 4 5;
                mk_wrapper 6 @@ mk_terminal id_num 6 9;
              ];
              start_pos = 0;
              end_pos = 9;
            };
            mk_terminal plus_num 10 11;
            mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 12 15;
          ];
        start_pos = 0;
        end_pos = 15;
      });

    (* Figure 4.38 from CPTT *)
    expect (Lr0'.parse (* "foo *" *) tokens3) |> toEqual
      (Result.Error (4, "parsing invariant violation -- pop failed"));

    expect (Lr0'.parse (* "foo + bar" *) tokens4) |> toEqual (Result.Ok
      { production = Either.Right 1;
        children = [
          mk_wrapper 2 @@ mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 0 3;
          mk_terminal plus_num 4 5;
          mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 6 9;
        ];
        start_pos = 0;
        end_pos = 9;
      });

  ] Util.id;

  test "lex-parse" (fun () ->
    let lexer =
      [ "\\+", "+";
        "\\*", "*";
        "\\(", "(";
        "\\)", ")";
        "\\w+", "id";
      ]
    in
    (* let input = "if 1 < 2 then foo else \"str\"" in *)
    let input = "foo+bar" in
    match Lr0'.lex_and_parse lexer input with
      | Error _err -> fail "lex_and_parse error"
      | Ok _ -> pass
  );
)
