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

(* CPTT Example 4.54 *)
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

  let module Grammar1LR = Lr0(Grammar1) in
  let module Grammar2LR = Lr0(Grammar2) in
  let module Grammar1Lalr = Lalr1(Grammar1) in
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
      [| { item = mk_item' 2 1; lookahead_set = SI.fromArray [| 0; 1; 2 |] } |]
      [| { item = mk_item' 2 0; lookahead_set = SI.fromArray [| 0; 1; 2 |] };
         { item = mk_item' 3 0; lookahead_set = SI.fromArray [| 0; 1; 2 |] };
      |];
    mk_config_set (* 4 *)
      [| { item = mk_item' 3 1; lookahead_set = SI.fromArray [| 0; 1; 2 |] } |]
      [||];
    mk_config_set (* 5 *)
      [| { item = mk_item' 1 2; lookahead_set = SI.fromArray [| 0 |] } |]
      [||];
    mk_config_set (* 6 *)
      [| { item = mk_item' 2 2; lookahead_set = SI.fromArray [| 0; 1; 2 |] } |]
      [||];
  |]
  in

  let show_lookahead_item_sets : lookahead_item_sets -> string
    = fun set -> set
    |. Belt.Set.toArray
    |. Belt.Array.map Grammar1Lalr.string_of_lookahead_item_set
    |. Js.Array2.joinWith "\n\n"
  in

  describe "lalr1_items" (fun () ->
    (* First check the kernels are as expected *)
    test "kernels" (fun () ->
      expect (Grammar1Lalr.lalr1_items
        |. Belt.Map.Int.valuesToArray
        |. lookahead_item_set_set
      ) |> toBeEquivalent show_lookahead_item_sets Belt.Set.eq
        (gram1_lr1_config_sets
          |. Belt.Array.map (fun config_set -> config_set.kernel_items)
          |. lookahead_item_set_set
      )
    );

    (* Then verify all closures are as expected *)
    test "closures" (fun () ->
      expect (Grammar1Lalr.lalr1_items
        |. Belt.Map.Int.valuesToArray
        |. Belt.Array.map Grammar1Lalr.lr1_closure
        |. lookahead_item_set_set
      ) |> toBeEquivalent show_lookahead_item_sets Belt.Set.eq
        (gram1_lr1_config_sets
          |. Belt.Array.map simplify_lookahead_config_set
          |. lookahead_item_set_set
      )
    );
  );

  let mk_item_set pruduction_num position =
    SI.fromArray [| mk_item' pruduction_num position |]
  in

  (* CPTT Figure 4.44 *)
  let expected_grammar2_lalr1_kernels : (state * item_set) array = [|
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

  let actual_grammar2_lalr1_kernels : item_set_set
    = Grammar2Lalr.lalr1_items
      |. Belt.Map.Int.valuesToArray
      |. Belt.Array.map (fun lookahead_item_set -> lookahead_item_set
        |. Belt.Set.toArray
        |. Belt.Array.map (fun lookahead_item -> lookahead_item.item)
        |. SI.fromArray
      )
      |. mk_set
  in

  let show_item_set_set : LrParsing.item_set_set -> string
    = fun sets -> sets
    |. Belt.Set.toArray
    |. Belt.Array.map Grammar2LR.string_of_item_set
    |. Js.Array2.joinWith "\n"
  in

  testAll "grammar 2 lalr1 kernel items" [
    expect actual_grammar2_lalr1_kernels
      |> toBeEquivalent show_item_set_set Belt.Set.eq (mk_set
        (Belt.Array.map expected_grammar2_lalr1_kernels (fun (_, k) -> k)))
  ] Util.id;

  let book_state_mapping = Belt.Map.Int.(expected_grammar2_lalr1_kernels
    |. fromArray
    |. map Grammar2LR.item_set_to_state
  )
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

  (* cdd
   * 0123
   *)
  let tokens1 = MQueue.fromArray [|
    mk_tok "c" 0 1;
    mk_tok "d" 1 2;
    mk_tok "d" 2 3;
    mk_tok "$" 3 3;
  |]
  in

  let s'_num : nonterminal_num = 0 in
  let s_num : nonterminal_num = 1 in
  let c_num : nonterminal_num = 2 in

  let state = gram1_lr1_config_sets
    |. Belt.Array.map (fun config_set -> Grammar1LR.item_set_to_state @@
      lookahead_item_set_to_item_set config_set.kernel_items
    )
  in

  let expect_no_goto = fun start_state nt_num ->
    expect (Grammar1Lalr.lalr1_goto_table state.(start_state) (Nonterminal nt_num))
      |> toEqual None
  in
  let expect_goto = fun start_state nt_num result_state ->
    expect (Grammar1Lalr.lalr1_goto_table state.(start_state) (Nonterminal nt_num))
      |> toEqual (Some state.(result_state))
  in

  testAll "lalr1_goto_table" [
    (* first, four gotos that are actually valid *)
    expect_goto 0 s_num 1;
    expect_goto 0 c_num 2;
    expect_goto 2 c_num 5;
    expect_goto 3 c_num 6;
    (* the rest of the table is empty *)
    expect_no_goto 0 s'_num;
    expect_no_goto 1 s'_num;
    expect_no_goto 2 s'_num;
    expect_no_goto 3 s'_num;
    expect_no_goto 4 s'_num;
    expect_no_goto 5 s'_num;
    expect_no_goto 6 s'_num;
    expect_no_goto 1 s_num;
    expect_no_goto 2 s_num;
    expect_no_goto 3 s_num;
    expect_no_goto 4 s_num;
    expect_no_goto 5 s_num;
    expect_no_goto 6 s_num;
    expect_no_goto 1 c_num;
    expect_no_goto 4 c_num;
    expect_no_goto 5 c_num;
    expect_no_goto 6 c_num;
  ] Util.id;

  let c_num : terminal_num = 1 in
  let d_num : terminal_num = 2 in

  let action_table_tests =
    [ 0, c_num, Shift state.(3);
      0, d_num, Shift state.(4);
      0, 0,     Error None;
      1, c_num, Error None;
      1, c_num, Error None;
      1, 0,     Accept;
      2, c_num, Shift state.(3);
      2, d_num, Shift state.(4);
      2, 0,     Error None;
      3, c_num, Shift state.(3);
      3, d_num, Shift state.(4);
      3, 0,     Error None;
      4, c_num, Reduce state.(3);
      4, d_num, Reduce state.(3);
      4, 0,     Reduce state.(3);
      5, c_num, Error None;
      5, d_num, Error None;
      5, 0,     Reduce state.(1);
      6, c_num, Reduce state.(2);
      6, d_num, Reduce state.(2);
      6, 0,     Reduce state.(2);
    ]
  in

  let action_table_tests' = action_table_tests
    |. Belt.List.map (fun (init_state, terminal_num, action) ->
      expect (Grammar1Lalr.lalr1_action_table state.(init_state) terminal_num)
        |> toEqual action
    )
  in
  testAll "lalr1_action_table" action_table_tests' Util.id;

  testAll "parse" [

    expect (Grammar1Lalr.parse (* "cdd" *) tokens1) |> toEqual (Result.Ok
      { production = Either.Right 1;
        children = [
          { production = Either.Right 2;
            children = [
              mk_terminal c_num 0 1;
              mk_wrapper 3 @@ mk_terminal d_num 1 2;
            ];
            start_pos = 0;
            end_pos = 2;
          };
          mk_wrapper 3 @@ mk_terminal d_num 2 3;
        ];
        start_pos = 0;
        end_pos = 3;
      });

  ] Util.id;

)