open Jest
open Expect
open LrParsing
open LalrParsing
open TestUtil
module M = Belt.Map.Int
module MS = Belt.Map.String
module SI = Belt.Set.Int
module MStack = Belt.MutableStack
module MQueue = Belt.MutableQueue

module MutableLookaheadItemSetCmp = Belt.Id.MakeComparable(struct
  type t = mutable_lookahead_item_set
  let cmp x y = M.cmp x y Belt.MutableSet.Int.cmp
end)

module PropagationCmp = Belt.Id.MakeComparable(struct
  type t = LrParsing.state * LrParsing.item
  let cmp (s1, i1) (s2, i2) = match Pervasives.compare s1 s2 with
    | 0 -> Pervasives.compare i1 i2
    | c -> c
end)

module GenerationCmp = Belt.Id.MakeComparable(struct
  type t = LrParsing.state * lookahead_item
  let cmp (s1, i1) (s2, i2) = match Pervasives.compare s1 s2 with
    | 0 -> Pervasives.compare i1 i2
    | c -> c
end)

module Grammar1 : GRAMMAR = struct
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

(* CPTT Example 4.61 *)
module Grammar2 : GRAMMAR = struct
  let grammar = {
    nonterminals = M.fromArray
    [|
       (* S' (note: the grammar we provide is already augmented) *)
       0, { productions = [[Nonterminal 1]] }; (* S' -> S *)
       (* S *)
       1, { productions = [
         [Nonterminal 2; Terminal 1; Nonterminal 3]; (* L = R *)
         [Nonterminal 3]; (* R *)
         ]
       };
       (* L *)
       2, { productions = [
         [Terminal 2; Nonterminal 3]; (* * R *)
         [Terminal 3]                 (* id *)
         ]
       };
       (* R *)
       3, { productions = [[Nonterminal 2]] }; (* L *)
    |];

    terminal_nums =
    [|
      "$", 0;
      "=", 1;
      "*", 2;
      "id", 3;
    |];
    nonterminal_nums =
    [|
      "S'", 0;
      "S",  1;
      "L",  2;
      "R",  3;
    |];
  }
end

type lookahead_item_sets =
  (lookahead_item_set, LookaheadItemSetCmp.identity) Belt.Set.t

let () = describe "LrParsing" (fun () ->

  (* TODO: separate Lr0 / Lr1 modules *)
  let module Grammar1LR = Lr0(Grammar1) in
  let module Grammar2LR = Lr0(Grammar2) in
  let module Grammar2Lalr = Lalr1(Grammar2) in

  let mk_arr items = S.fromArray items ~id:(module LookaheadItemCmp) in
  let mk_config_set kernel_items nonkernel_items =
    { kernel_items = mk_arr kernel_items;
      nonkernel_items = mk_arr nonkernel_items;
    }
  in

  let lookahead_item_set_set
    : lookahead_item_set array -> lookahead_item_sets =
    Belt.Set.fromArray ~id:(module LookaheadItemSetCmp)
  in

  (*
  (* CPTT Fig 4.41 *)
  let gram1_lr1_config_sets : lookahead_configuration_set array = [|
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

  testAll "lookahead_lr0_items" [
    (* First check the kernels are as expected *)
    expect
      (Grammar1LR.mutable_lookahead_lr0_items
        |. Belt.MutableSet.toArray
        |. lookahead_item_set_set
    ) |> toBeEquivalent (fun _ -> "TODO: show lookahead_lr0_items") Belt.Set.eq
      (gram1_lr1_config_sets
        |. Belt.Array.map (fun config_set -> config_set.kernel_items)
        |. lookahead_item_set_set
      );

    (* Then verify all closures are as expected *)
    expect
      (* TODO use lookahead_lr0_items *)
      (Grammar1LR.mutable_lookahead_lr0_items
        |. Belt.MutableSet.toArray
        |. Belt.Array.map Grammar1LR.lr1_closure
        |. lookahead_item_set_set
    ) |> toBeEquivalent (fun _ -> "TODO: show closures") Belt.Set.eq
      (gram1_lr1_config_sets
        |. Belt.Array.map simplify_lookahead_config_set
        |. lookahead_item_set_set
      );
  ] Util.id;
  *)

  let mk_item_set pruduction_num position =
    SI.fromArray [| mk_item' pruduction_num position |]
  in

  (* CPTT Figure 4.44 *)
  let expected_gram2_lr0_kernels : (state * item_set) array = [|
    0, mk_item_set 0 0; (* S' -> . S *)
    1, mk_item_set 0 1; (* S' -> S . *)
    (* S -> L . = R
     * R -> L .
     *)
    2, SI.fromArray [| mk_item' 1 1; mk_item' 5 1 |];
    3, mk_item_set 2 1; (* S -> R . *)
    4, mk_item_set 3 1; (* L -> * . R *)
    5, mk_item_set 4 1; (* L -> id . *)
    6, mk_item_set 1 2; (* S -> L = . R *)
    7, mk_item_set 3 2; (* L -> * R . *)
    8, mk_item_set 5 1; (* R -> L . *)
    9, mk_item_set 1 3; (* S -> L = R . *)
  |]
  in

  let mk_set : item_set array -> item_set_set
    = Belt.Set.fromArray ~id:(module LrParsing.ComparableIntSet)
  in

  (*
  let actual_gram2_lr0_kernels : item_set_set
    = Grammar2LR.lookahead_lr0_items
      |. Belt.Map.Int.valuesToArray
      |. Belt.Array.map (fun lookahead_item_set -> lookahead_item_set
        |. Belt.Set.toArray
        |. Belt.Array.map (fun lookahead_item -> lookahead_item.item)
        |. SI.fromArray
      )
      |. mk_set
  in

  testAll "grammar 2 lr0 items" [
    expect actual_gram2_lr0_kernels
      |> toBeEquivalent (fun _ -> "TODO: show lr0 kernels") Belt.Set.eq (mk_set
        (Belt.Array.map expected_gram2_lr0_kernels (fun (_, k) -> k)))
  ] Util.id;
  *)

  let book_state_mapping = expected_gram2_lr0_kernels
    |. Belt.Array.map
      (fun (i, item_set) -> (i, Grammar2LR.item_set_to_state item_set))
    |. M.fromArray
  in

  let book_state : int array
    = Belt.Array.makeBy (M.size book_state_mapping)
      (fun i -> book_state_mapping |. M.getExn i)
  in

  let no_terminal_num = 4 in (* # *)

  let lookahead_item_set = lookahead_item_set_from_array
    [|
      { item = mk_item' 0 0; lookahead_set = SI.fromArray [| no_terminal_num |] }
    |]
  in

  test "grammar 2 closure" (fun () ->
    (* CPTT Example 4.64 *)
    let expected_closure = lookahead_item_set_from_array
      [|
        (* S' -> . S, # *)
        { item = mk_item' 0 0; lookahead_set = SI.fromArray [| no_terminal_num |] };
        (* S -> . L = R, # *)
        { item = mk_item' 1 0; lookahead_set = SI.fromArray [| no_terminal_num |] };
        (* S -> . R, # *)
        { item = mk_item' 2 0; lookahead_set = SI.fromArray [| no_terminal_num |] };
        (* L -> . * R, #/= *)
        { item = mk_item' 3 0; lookahead_set = SI.fromArray [| no_terminal_num; 1 |] };
        (* L -> . id, #/= *)
        { item = mk_item' 4 0; lookahead_set = SI.fromArray [| no_terminal_num; 1 |] };
        (* R -> . L, # *)
        { item = mk_item' 5 0; lookahead_set = SI.fromArray [| no_terminal_num |] };
      |]
    in
    expect (Grammar2Lalr.lr1_closure lookahead_item_set)
      |> toBeEquivalent (fun _ -> "TODO: show lr_closure") S.eq expected_closure;
  );

  let { spontaneous_generation; propagation } =
    Grammar2Lalr.generate_lookaheads
      (SI.fromArray [| mk_item' 0 0 |])
      (mk_item' 0 0)
  in
  let expected_propagation =
    [| book_state.(1), mk_item' 0 1; (* S' -> S . *)
       book_state.(2), mk_item' 1 1; (* S -> L . = R *)
       book_state.(3), mk_item' 2 1; (* S -> R . *)
       book_state.(4), mk_item' 3 1; (* L -> * . R *)
       book_state.(5), mk_item' 4 1; (* L -> id . *)
       book_state.(2), mk_item' 5 1; (* R -> L . *)
    |]
  in

  let expected_generation =
    [| (* L -> * . R, = *)
       book_state.(4), { item = mk_item' 3 1; lookahead_set = SI.fromArray [| 1 |] };
       (* L -> id ., = *)
       book_state.(5), { item = mk_item' 4 1; lookahead_set = SI.fromArray [| 1 |] };
    |]
  in

  let string_of_propagation = fun propagation -> propagation
    |. Belt.Array.map (fun (n, item) ->
      Printf.sprintf "%n -> %s" n (Grammar2LR.string_of_item item))
    |. Js.Array2.joinWith "\n"
  in

  (*
  Printf.printf "expected_propagation\n%s\n" (string_of_propagation expected_propagation);
  Printf.printf "propagation\n%s\n" (string_of_propagation propagation);

  Printf.printf "spontaneous_generation:\n%s\n"
    (Grammar2LR.string_of_lookahead_item_set spontaneous_generation);
  Printf.printf "expected_generation:\n%s\n"
    (Grammar2LR.string_of_lookahead_item_set expected_generation);
    *)

  let equivalent_propagation : (state * item) array -> (state * item) array -> bool
    = fun p1 p2 -> Belt.Set.eq
      (Belt.Set.fromArray ~id:(module PropagationCmp) p1)
      (Belt.Set.fromArray ~id:(module PropagationCmp) p2)
  in

  let equivalent_generation : (state * lookahead_item) array -> (state * lookahead_item) array -> bool
    = fun g1 g2 -> Belt.Set.eq
      (Belt.Set.fromArray ~id:(module GenerationCmp) g1)
      (Belt.Set.fromArray ~id:(module GenerationCmp) g2)
  in

  let string_of_generation : (state * lookahead_item) array -> string
    = fun generation -> generation
      |. Belt.Array.map (fun (state, lookahead_item) ->
        Printf.sprintf "%n: %s" state (Grammar2Lalr.string_of_lookahead_item lookahead_item)
      )
      |. Js.Array2.joinWith "\n"
  in

  describe "generate_lookaheads" (fun () ->
    test "propagation" (fun () ->
      expect propagation
        |> toBeEquivalent string_of_propagation equivalent_propagation expected_propagation
    );
    test "spontaneous_generation" (fun () ->
      expect spontaneous_generation
        |> toBeEquivalent string_of_generation equivalent_generation expected_generation
    );
  );

  let mk = fun item lookahead -> M.fromArray
    [| item, Belt.MutableSet.Int.fromArray lookahead |]
  in

  let expected_lalr1_items
    : (mutable_lookahead_item_set, MutableLookaheadItemSetCmp.identity) Belt.Set.t
    = Belt.Set.fromArray
      ~id:(module MutableLookaheadItemSetCmp)
      [| mk (mk_item' 0 0) [| 0 |];
         mk (mk_item' 0 1) [| 0 |];
         (M.fromArray
           [| mk_item' 1 1, Belt.MutableSet.Int.fromArray [| 0 |];
              mk_item' 5 1, Belt.MutableSet.Int.fromArray [| 0 |];
           |]);
         mk (mk_item' 2 1) [| 0 |];
         mk (mk_item' 3 1) [| 0; 1 |];
         mk (mk_item' 4 1) [| 0; 1 |];
         mk (mk_item' 1 2) [| 0 |];
         mk (mk_item' 3 2) [| 0; 1 |];
         mk (mk_item' 5 1) [| 0; 1 |];
         mk (mk_item' 1 3) [| 0 |];
      |]
  in

  let lalr1_items_set
    : (mutable_lookahead_item_set, MutableLookaheadItemSetCmp.identity) Belt.Set.t
    = Grammar2Lalr.mutable_lalr1_items
    |. Belt.Map.Int.valuesToArray
    |. Belt.Set.fromArray ~id:(module MutableLookaheadItemSetCmp)
  in

  let string_of_lalr1_items_set = fun lalr1_items_set -> lalr1_items_set
    |. Belt.Set.toArray
    |. Belt.Array.map (fun mutable_lookahead_item_set -> mutable_lookahead_item_set
      |. M.toArray
      |. Belt.Array.map (fun (item, mutable_lookahead) ->
        let lookahead_set = mutable_lookahead
          |. Belt.MutableSet.Int.toArray
          |. SI.fromArray
        in
        let lookahead_item = { item; lookahead_set } in
        Grammar2Lalr.string_of_lookahead_item lookahead_item
      )
      |. Js.Array2.joinWith "\n"
    )
    |. Js.Array2.joinWith "\n\n"
  in

  testAll "mutable_lalr1_items" [
    expect lalr1_items_set
      |> toBeEquivalent string_of_lalr1_items_set Belt.Set.eq
        expected_lalr1_items
  ] Util.id;

)
