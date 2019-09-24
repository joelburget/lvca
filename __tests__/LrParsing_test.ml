open Jest
open Expect
open LrParsing
module M = Belt.Map.Int
module MS = Belt.Map.String
module SI = Belt.Set.Int

module Grammar : GRAMMAR = struct
  let grammar = {
    nonterminals = M.fromArray
    [|
       (* E' *)
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
    num_terminals = 6;
    terminal_names = MS.fromArray
    [|
      "$", 0;
      "+", 1;
      "*", 2;
      "(", 3;
      ")", 4;
      "id", 5;
    |];
    nonterminal_names = MS.fromArray
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
  let expected0 =
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

  let expected1 =
    { kernel_items = SI.fromArray items1;
      nonkernel_items = SI.fromArray [||];
    }
  in

  (* I7 *)
  let items7 = [| mk_item' 3 2 |] in
  let expected7 =
    { kernel_items = SI.fromArray items7;
      nonkernel_items = SI.fromArray [| mk_item' 5 0; mk_item' 6 0 |];
    }
  in

  testAll "closure" [
    expect (Lr0'.closure @@ SI.fromArray items0)
      |> toEqual expected0;
    expect (Lr0'.closure @@ SI.fromArray items1)
      |> toEqual expected1;
    expect (Lr0'.closure @@ SI.fromArray items7)
      |> toEqual expected7;
  ] Util.id;

  let goto_kernel = SI.fromArray [| mk_item' 1 2 |] in

  let goto_nonkernel = SI.fromArray [|
    mk_item' 3 0;
    mk_item' 4 0;
    mk_item' 5 0;
    mk_item' 6 0;
  |]
  in

  testAll "goto" [
    expect (Lr0'.goto_kernel (SI.fromArray items1) (Terminal 1))
      |> toEqual goto_kernel;
    expect (Lr0'.goto (SI.fromArray items1) (Terminal 1))
      |> toEqual
      { kernel_items = goto_kernel; nonkernel_items = goto_nonkernel };
  ] Util.id;

  let item_sets = [|
    SI.fromArray
      [| mk_item' 0 0;
         mk_item' 1 0;
         mk_item' 2 0;
         mk_item' 3 0;
         mk_item' 4 0;
         mk_item' 5 0;
         mk_item' 6 0;
      |];
    SI.fromArray
      [| mk_item' 0 1;
         mk_item' 1 1;
      |];
    SI.fromArray
      [| mk_item' 2 1;
         mk_item' 3 1;
      |];
    SI.fromArray [| mk_item' 4 1; |];
    SI.fromArray
      [| mk_item' 5 1;
         mk_item' 1 0;
         mk_item' 2 0;
         mk_item' 3 0;
         mk_item' 4 0;
         mk_item' 5 0;
         mk_item' 6 0;
      |];
    SI.fromArray [| mk_item' 6 1; |];
    SI.fromArray
      [| mk_item' 1 2;
         mk_item' 3 0;
         mk_item' 4 0;
         mk_item' 5 0;
         mk_item' 6 0;
      |];
    SI.fromArray
      [| mk_item' 3 2;
         mk_item' 5 0;
         mk_item' 6 0;
      |];
    SI.fromArray
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

  let expected_item_sets =
    Belt.MutableSet.fromArray item_sets ~id:(module ComparableSet)
  in

  let normalize = fun items -> items
    |. Belt.MutableSet.toList
    |. L.map (fun items' -> items' |. SI.toList)
  in

  testAll "items" [
    expect (normalize Lr0'.items)
      |> toEqual (normalize expected_item_sets);
    (* TODO
    expect (M.get Lr0'.items' 1 == Some (SI.fromArray items1)) |> toBe true;
    expect (M.get Lr0'.items' 7 == Some (SI.fromArray items7)) |> toBe true;
    *)
  ] Util.id;

  let state = item_sets |. Belt.Array.map Lr0'.item_set_to_state in
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
  testAll "goto_table" [
    expect (Lr0'.goto_table state.(0) (Nonterminal e_num)) |> toEqual state.(1);
    expect (Lr0'.goto_table state.(0) (Nonterminal t_num)) |> toEqual state.(2);
    expect (Lr0'.goto_table state.(0) (Nonterminal f_num)) |> toEqual state.(3);
    expect (Lr0'.goto_table state.(4) (Nonterminal e_num)) |> toEqual state.(8);
    expect (Lr0'.goto_table state.(4) (Nonterminal t_num)) |> toEqual state.(2);
    expect (Lr0'.goto_table state.(4) (Nonterminal f_num)) |> toEqual state.(3);
    (* TODO: test other invalid GOTOs *)
    (* TODO: should this throw? *)
    expect (fun () -> Lr0'.goto_table state.(6) (Nonterminal e_num)) |> toThrow;
    expect (Lr0'.goto_table state.(6) (Nonterminal t_num)) |> toEqual state.(9);
    expect (Lr0'.goto_table state.(6) (Nonterminal f_num)) |> toEqual state.(3);
    expect (fun () -> Lr0'.goto_table state.(7) (Nonterminal e_num)) |> toThrow;
    expect (fun () -> Lr0'.goto_table state.(7) (Nonterminal t_num)) |> toThrow;
    expect (Lr0'.goto_table state.(7) (Nonterminal f_num)) |> toEqual state.(10);
  ] Util.id;

  (* Test for a match with CPTT Figure 4.37 *)
  testAll "action_table" [
    expect (Lr0'.action_table state.(0) id_num)
      |> toEqual (Shift state.(5));
    expect (Lr0'.action_table state.(0) plus_num)
      |> toEqual Error;
    expect (Lr0'.action_table state.(0) times_num)
      |> toEqual Error;
    expect (Lr0'.action_table state.(0) lparen_num)
      |> toEqual (Shift state.(4));

    expect (Lr0'.action_table state.(1) plus_num)
      |> toEqual (Shift state.(6));
    expect (Lr0'.action_table state.(1) 0)
      |> toEqual Accept;

    expect (Lr0'.action_table state.(2) plus_num)
      |> toEqual (Reduce (1, 1));
    expect (Lr0'.action_table state.(2) times_num)
      |> toEqual (Shift state.(7));
    expect (Lr0'.action_table state.(2) rparen_num)
      |> toEqual (Reduce (1, 1));
    expect (Lr0'.action_table state.(2) 0)
      |> toEqual (Reduce (1, 1));

    (* XXX: the book uses production numbers instead of nonterminal, count pairs *)
    expect (Lr0'.action_table state.(3) plus_num)
      |> toEqual (Reduce (2, 1));
    expect (Lr0'.action_table state.(3) times_num)
      |> toEqual (Reduce (2, 1));
    expect (Lr0'.action_table state.(3) rparen_num)
      |> toEqual (Reduce (2, 1));
    expect (Lr0'.action_table state.(3) 0)
      |> toEqual (Reduce (2, 1));

    expect (Lr0'.action_table state.(4) id_num)
      |> toEqual (Shift state.(5));
    expect (Lr0'.action_table state.(4) lparen_num)
      |> toEqual (Shift state.(4));

    expect (Lr0'.action_table state.(5) plus_num)
      |> toEqual (Reduce (3, 1));
    expect (Lr0'.action_table state.(5) times_num)
      |> toEqual (Reduce (3, 1));
    expect (Lr0'.action_table state.(5) rparen_num)
      |> toEqual (Reduce (3, 1));
    expect (Lr0'.action_table state.(5) 0)
      |> toEqual (Reduce (3, 1));

    expect (Lr0'.action_table state.(6) id_num)
      |> toEqual (Shift state.(5));
    expect (Lr0'.action_table state.(6) lparen_num)
      |> toEqual (Shift state.(4));

    expect (Lr0'.action_table state.(7) id_num)
      |> toEqual (Shift state.(5));
    expect (Lr0'.action_table state.(7) lparen_num)
      |> toEqual (Shift state.(4));
  ] Util.id;
)
