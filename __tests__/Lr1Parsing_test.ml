open Jest
open Expect
open LrParsing
open TestUtil
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
  let mk_config_set kernel_items nonkernel_items =
    { kernel_items = mk_arr kernel_items;
      nonkernel_items = mk_arr nonkernel_items;
    }
  in

  let lookahead_item_set_set =
    Belt.Set.fromArray  ~id:(module LrParsing.LookaheadItemSetCmp)
  in

  (* CPTT Fig 4.41 *)
  let lr1_config_sets : lookahead_configuration_set array = [|
    mk_config_set (* 0 *)
      [| { item = mk_item' 0 0; lookahead_set = SI.fromArray [| 0 |] } |]
      [| { item = mk_item' 1 0; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 1; 2 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 1; 2 |] };
      |];
    mk_config_set (* 1 *)
      [| { item = mk_item' 0 1; lookahead_set = SI.fromArray [| 0 |] } |]
      [||];
    mk_config_set (* 2 *)
      [| { item = mk_item' 1 1; lookahead_set = SI.fromArray [| 0 |] } |]
      [| { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 0 |] };
      |];
    mk_config_set (* 3 *)
      [| { item = mk_item' 2 1; lookahead_set = SI.fromArray [| 1; 2 |] } |]
      [| { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 1; 2 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 1; 2 |] };
      |];
    mk_config_set (* 4 *)
      [| { item = mk_item' 3 1; lookahead_set = SI.fromArray [| 1; 2 |] } |]
      [||];
    mk_config_set (* 5 *)
      [| { item = mk_item' 1 2; lookahead_set = SI.fromArray [| 0 |] } |]
      [||];
    mk_config_set (* 6 *)
      [| { item = mk_item' 2 1; lookahead_set = SI.fromArray [| 0 |] } |]
      [| { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 0 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 0 |] };
      |];
    mk_config_set (* 7 *)
      [| { item = mk_item' 3 1; lookahead_set = SI.fromArray [| 0 |] } |]
      [||];
    mk_config_set (* 8 *)
      [| { item = mk_item' 2 2; lookahead_set = SI.fromArray [| 1; 2 |] } |]
      [||];
    mk_config_set (* 9 *)
      [| { item = mk_item' 2 2; lookahead_set = SI.fromArray [| 0 |] } |]
      [||];
  |]
  in

  testAll "lr1_items" [
    (* First check the kernels are as expected *)
    expect (lr1_config_sets
      |. Belt.Array.map (fun config_set -> config_set.kernel_items)
      |. lookahead_item_set_set
    ) |> toBeEquivalent Belt.Set.eq
      (Lr0'.mutable_lr1_items
        |. Belt.MutableSet.toArray
        |. lookahead_item_set_set
    );

    (* Then verify all closures are as expected *)
    expect (lr1_config_sets
      |. Belt.Array.map simplify_lookahead_config_set
      |. lookahead_item_set_set
    ) |> toBeEquivalent Belt.Set.eq
      (Lr0'.mutable_lr1_items
        |. Belt.MutableSet.toArray
        |. Belt.Array.map Lr0'.lr1_closure
        |. lookahead_item_set_set
    );
  ] Util.id;

)
