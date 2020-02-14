open Jest
open Expect
open LrParsing
open TestUtil
module SI = Placemat.IntSet
module MStack = Placemat.MutableStack
module MQueue = Placemat.MutableQueue
module Result = Tablecloth.Result

module Grammar : GRAMMAR = struct
  let grammar = {
    nonterminals = Placemat.IntDict.from_array
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
      "E",  1;
      "T",  2;
      "F",  3;
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

  testAll "first_set" [
    expect (Lr0'.first_set [Nonterminal 1; Terminal 1; Nonterminal 2])
      |> toEqual (SI.from_array [|3;5|]);
    expect (Lr0'.first_set [Nonterminal 2])
      |> toEqual (SI.from_array [|3;5|]);
    expect (Lr0'.first_set [Nonterminal 2; Terminal 2; Nonterminal 3])
      |> toEqual (SI.from_array [|3;5|]);
    expect (Lr0'.first_set [Nonterminal 3])
      |> toEqual (SI.from_array [|3;5|]);
    expect (Lr0'.first_set [Terminal 3; Nonterminal 1; Terminal 4])
      |> toEqual (SI.from_array [|3|]);
    expect (Lr0'.first_set [Terminal 5])
      |> toEqual (SI.from_array [|5|]);
  ] Util.id;

  describe "follow_set" (fun () ->
    let show_follow_set = fun follow_set -> follow_set
      |> SI.to_array
      |> Tablecloth.Array.map ~f:Lr0'.string_of_terminal
      |. Js.Array2.joinWith " "
    in
    let test_follow_set nt expected_set = test
      ("follow_set " ^ Lr0'.string_of_nonterminal_num nt)
      (fun () -> expect (Lr0'.follow_set nt)
        |> toBeEquivalent show_follow_set SI.eq (SI.from_array expected_set)
      )
    in

    test_follow_set 0 [| 0 |];
    test_follow_set 1 [| 0; 1; 4 |];
    test_follow_set 2 [| 0; 1; 2; 4 |];
    test_follow_set 3 [| 0; 1; 2; 4 |];
  );

  (* I0 *)
  let items0 = [| mk_item' 0 0 |] in
  let expected0 : configuration_set =
    { kernel_items = SI.from_array items0;
      nonkernel_items = SI.from_array
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
    { kernel_items = SI.from_array items1;
      nonkernel_items = SI.from_array [||];
    }
  in

  (* I7 *)
  let items7 = [| mk_item' 3 2 |] in
  let expected7 : configuration_set =
    { kernel_items = SI.from_array items7;
      nonkernel_items = SI.from_array [| mk_item' 5 0; mk_item' 6 0 |];
    }
  in

  testAll "closure" [
    expect (Lr0'.lr0_closure' @@ SI.from_array items0)
      |> toEqual expected0;
    expect (Lr0'.lr0_closure' @@ SI.from_array items1)
      |> toEqual expected1;
    expect (Lr0'.lr0_closure' @@ SI.from_array items7)
      |> toEqual expected7;
  ] Util.id;

  let lr0_goto_kernel = SI.from_array [| mk_item' 1 2 |] in

  let goto_nonkernel = SI.from_array [|
    mk_item' 3 0;
    mk_item' 4 0;
    mk_item' 5 0;
    mk_item' 6 0;
  |]
  in

  testAll "goto" [
    expect (Lr0'.lr0_goto_kernel (SI.from_array items1) (Terminal 1))
      |> toEqual lr0_goto_kernel;
    expect
      (Lr0'.lr0_closure' @@
       Lr0'.lr0_goto_kernel (SI.from_array items1) (Terminal 1))
      |> toEqual
      ({ kernel_items = lr0_goto_kernel; nonkernel_items = goto_nonkernel }
        : configuration_set);
  ] Util.id;

  let lr0_item_sets = [|
    SI.from_array [| mk_item' 0 0 |]; (* 0 *)
    SI.from_array (* 1 *)
      [| mk_item' 0 1;
         mk_item' 1 1;
      |];
    SI.from_array (* 2 *)
      [| mk_item' 2 1;
         mk_item' 3 1;
      |];
    SI.from_array [| mk_item' 4 1; |]; (* 3 *)
    SI.from_array [| mk_item' 5 1; |]; (* 4 *)
    SI.from_array [| mk_item' 6 1; |]; (* 5 *)
    SI.from_array [| mk_item' 1 2; |]; (* 6 *)
    SI.from_array [| mk_item' 3 2; |]; (* 7 *)
    SI.from_array (* 8 *)
      [|
        mk_item' 1 1;
        mk_item' 5 2;
      |];
    SI.from_array (* 9 *)
      [|
        mk_item' 1 3;
        mk_item' 3 1;
      |];
    SI.from_array [| mk_item' 3 3 |]; (* 10 *)
    SI.from_array [| mk_item' 5 3 |]; (* 11 *)
  |]
  in

  let expected_lr0_item_sets =
    Placemat.MutableSet.from_array lr0_item_sets ~id:(module ComparableIntSet)
  in

  let normalize = fun items -> items
    |> Placemat.MutableSet.to_list
    |> Tablecloth.List.map ~f:Tablecloth.IntSet.to_list
  in

  testAll "lr0_items" [
    expect (normalize Lr0'.mutable_lr0_items)
      |> toEqual (normalize expected_lr0_item_sets);
    (* TODO
    expect (Tablecloth.IntDict.get Lr0'.items' ~key:1 == Some (SI.from_array items1))
      |> toBe true;
    expect (Tablecloth.IntDict.get Lr0'.items' ~key:7 == Some (SI.from_array items7))
      |> toBe true;
    *)
  ] Util.id;

  let state = lr0_item_sets |> Tablecloth.Array.map ~f:Lr0'.item_set_to_state in
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
    |. Tablecloth.List.map ~f:(fun (init_state, terminal_num, action) ->
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

  (* TODO: test failed parses *)
  describe "parse" (fun () ->

    test "foo * bar" (fun () ->
      (* foo * bar
       * 0123456789
       *)
      let tokens1 = MQueue.from_array [|
        mk_tok "id" 0 3;
        mk_tok "*"  4 5;
        mk_tok "id" 6 9;
        mk_tok "$"  9 9;
      |]
      in
      expect (Lr0'.parse tokens1) |> toEqual (Ok
        (mk_wrapper 2
          { production = Either.Right 3;
            children = [
              mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 0 3;
              mk_terminal times_num 4 5;
              mk_wrapper 6 @@ mk_terminal id_num 6 9;
            ];
            start_pos = 0;
            end_pos = 9;
          }))
    );

    (* Figure 4.38 from CPTT *)
    test "foo * bar + baz" (fun () ->
      (* foo * bar + baz
       * 0123456789012345
       *)
      let tokens2 = MQueue.from_array [|
        mk_tok "id" 0 3;
        mk_tok "*"  4 5;
        mk_tok "id" 6 9;
        mk_tok "+"  10 11;
        mk_tok "id" 12 15;
        mk_tok "$"  15 15;
      |]
      in
      expect (Lr0'.parse tokens2) |> toEqual (Ok
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
        })
    );

    (* Figure 4.38 from CPTT *)
    test "foo *" (fun () ->
      (* foo * bar + baz
       * 0123456789012345
       *)
      let tokens3 = MQueue.from_array [|
        mk_tok "id" 0 3;
        mk_tok "*"  4 5;
      |]
      in
      expect (Lr0'.parse tokens3) |> toEqual
        (Error (4, "parsing invariant violation -- pop failed")
          : (parse_error, parse_result) Tablecloth.Result.t)
    );

    test "foo + bar" (fun () ->
      (* foo + bar
       * 0123456789
       *)
      let tokens4 = MQueue.from_array [|
        mk_tok "id" 0 3;
        mk_tok "+"  4 5;
        mk_tok "id" 6 9;
        mk_tok "$"  9 9;
      |]
      in
      expect (Lr0'.parse tokens4) |> toEqual (Ok
        { production = Either.Right 1;
          children = [
            mk_wrapper 2 @@ mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 0 3;
            mk_terminal plus_num 4 5;
            mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 6 9;
          ];
          start_pos = 0;
          end_pos = 9;
        })
    );

  );

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
