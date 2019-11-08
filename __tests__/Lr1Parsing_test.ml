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
      [| { item = mk_item' 2 2; lookahead_set = SI.fromArray [| 0 |] } |];
    mk_arr (* 9 *)
      [| { item = mk_item' 2 2; lookahead_set = SI.fromArray [| 1; 2 |] } |];
  |]
  in

  let expected_lr1_item_sets =
    Belt.MutableSet.fromArray lr1_item_sets ~id:(module LookaheadItemSetCmp)
  in

  let normalize = fun items -> items
    |. Belt.MutableSet.toList
    |. L.map S.toList
  in

  (* TODO: move to module *)
  let string_of_lookahead_item = fun { item; lookahead_set } ->
    let lookahead_nums = lookahead_set
      |. SI.toArray
      |. Belt.Array.map string_of_int
      |. Js.Array2.joinWith " "
    in
    Printf.sprintf "[%s, %s]" (Lr0'.string_of_item item) lookahead_nums
  in

  let string_of_lookahead_item_set = fun lookahead_item_set ->
    lookahead_item_set
      |. S.toArray
      |. Belt.Array.map string_of_lookahead_item
      |. Js.Array2.joinWith "\n"
  in

  let actual_items = Lr0'.mutable_lr1_items
    |. MSet.toArray
    |. Belt.Array.map string_of_lookahead_item_set
    |. Js.Array2.joinWith "\n\n"
  in

  Js.log actual_items;

  testAll "lr1_items" [
    expect (normalize Lr0'.mutable_lr1_items)
      |> toEqual (normalize expected_lr1_item_sets);
  ] Util.id;
)
