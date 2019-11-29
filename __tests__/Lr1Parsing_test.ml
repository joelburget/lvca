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
       (* S' (note: the grammar we provide is already augmented) *)
       0, { productions = [[Nonterminal 1]] }; (* S' -> S *)
       (* S *)
       1, { productions = [[Nonterminal 2; Nonterminal 2]] }; (* S -> C C *)
       (* C *)
       2, { productions = [
         [Terminal 1; Nonterminal 2]; (* C -> c C *)
         [Terminal 2]                 (* C -> d *)
         ]
       };
    |];

    terminal_nums =
    [|
      "$", 0;
      "c", 1;
      "d", 2;
    |];
    nonterminal_nums =
    [|
      "S'", 0;
      "S",  1;
      "C",  2;
    |];
  }
end

let () = describe "LrParsing" (fun () ->

  (* TODO: separate Lr0 / Lr1 modules *)
  let module Lr0' = Lr0(Grammar) in

  let mk_arr items = S.fromArray items ~id:(module LookaheadItemCmp) in

  (* CPTT Fig 4.41 *)
  let lr1_item_sets : lookahead_item_set array = [|
    mk_arr (* 0 *)
      [| { item = mk_item' 0 0; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 1 0; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 1; 2 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 1; 2 |] };
      |];
    mk_arr (* 1 *)
      [| { item = mk_item' 0 1; lookahead_set = SI.fromArray [| 0 |] };
      |];
    mk_arr (* 2 *)
      [| { item = mk_item' 1 1; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 0 |] };
      |];
    mk_arr (* 3 *)
      [| { item = mk_item' 2 1; lookahead_set = SI.fromArray [| 1; 2 |] };
         { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 1; 2 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 1; 2 |] };
      |];
    mk_arr (* 4 *)
      [| { item = mk_item' 3 1; lookahead_set = SI.fromArray [| 1; 2 |] } |];
    mk_arr (* 5 *)
      [| { item = mk_item' 1 2; lookahead_set = SI.fromArray [| 0 |] } |];
    mk_arr (* 6 *)
      [| { item = mk_item' 2 1; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 0 |] };
      |];
    mk_arr (* 7 *)
      [| { item = mk_item' 3 1; lookahead_set = SI.fromArray [| 0 |] } |];
    mk_arr (* 8 *)
      [| { item = mk_item' 2 2; lookahead_set = SI.fromArray [| 1; 2 |] } |];
    mk_arr (* 9 *)
      [| { item = mk_item' 2 2; lookahead_set = SI.fromArray [| 0 |] } |];
  |]
  in

  let expected_lr1_item_sets =
    Belt.MutableSet.fromArray lr1_item_sets ~id:(module LookaheadItemSetCmp)
  in

  let normalize = fun items -> items
    |. Belt.MutableSet.toList
    |. L.map S.toList
  in

  let actual_items = Lr0'.mutable_lr1_items
    |. MSet.toArray
    |. Belt.Array.map Lr0'.string_of_lookahead_item_set
    |. Js.Array2.joinWith "\n\n"
  in

  let expected_items = expected_lr1_item_sets
    |. MSet.toArray
    |. Belt.Array.map Lr0'.string_of_lookahead_item_set
    |. Js.Array2.joinWith "\n\n"
  in

  Js.log2 "actual_items" actual_items;
  Js.log2 "expected_items" expected_items;

  let actual_closure = Lr0'.lr1_closure' @@
    lookahead_item_set_from_array
      [| { item = mk_item' 0 0; lookahead_set = SI.fromArray [| 0 |] } |]
  in

  actual_closure
    |> LrParsing.simplify_lookahead_config_set
    |> Lr0'.string_of_lookahead_item_set
    |> Js.log2 "actual_closure";

  let expected_closure =
        { kernel_items = lookahead_item_set_from_array
          [| { item = mk_item' 0 0; lookahead_set = SI.fromArray [| 0 |] } |];
          nonkernel_items = lookahead_item_set_from_array
          [|
             { item = mk_item' 1 0; lookahead_set = SI.fromArray [| 0 |] };
             { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 1; 2 |] };
             { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 1; 2 |] };
          |]
        }
  in

  expected_closure
    |> LrParsing.simplify_lookahead_config_set
    |> Lr0'.string_of_lookahead_item_set
    |> Js.log2 "expected_closure";

  testAll "closure" [
    expect actual_closure |> toEqual expected_closure
  ] Util.id;

  (*
  testAll "lr1_items" [
    expect (normalize Lr0'.mutable_lr1_items)
      |> toEqual (normalize expected_lr1_item_sets);
  ] Util.id;
  *)

)
