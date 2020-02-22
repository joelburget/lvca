open Core_kernel
open LrParsing
open LalrParsing
module M = Int.Map
module MS = String.Map
module SI = Int.Set

module Propagation = struct
  module T = struct
    type t = LrParsing.state * LrParsing.item [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
end

module Generation = struct
  module T = struct
    type t = LrParsing.state * LookaheadItem.t [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
end

(* CPTT Example 4.54 *)
module Grammar1 : GRAMMAR = struct
  let grammar = {
    nonterminals = M.of_alist_exn
    [
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
    ];

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
    nonterminals = M.of_alist_exn
    [
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
    ];

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

module Grammar3 : GRAMMAR = struct
  let grammar = {
    nonterminals = M.of_alist_exn
    [
       0, { productions = [[Nonterminal 1]] }; (* arith' -> arith *)
       1, { productions = [
         [Nonterminal 1; Terminal 1; Nonterminal 1]; (* arith + arith *)
         [Terminal 2; Nonterminal 1; Terminal 3]; (* (arith) *)
         [Terminal 4]; (* id *)
         ]
       };
    ];

    terminal_nums =
    [|
      "$", 0;
      "+", 1;
      "(", 2;
      ")", 3;
      "id", 4;
    |];

    nonterminal_nums =
    [|
      "arith'", 0;
      "arith",  1;
    |];
  }
end

type lookahead_item_sets =
  (LookaheadItemSet.t, LookaheadItemSet.comparator_witness) Set.t

module Grammar1LR = Lr0(Grammar1)
module Grammar2LR = Lr0(Grammar2)
module Grammar1Lalr = Lalr1(Grammar1)
module Grammar2Lalr = Lalr1(Grammar2)
module Grammar3Lalr = Lalr1(Grammar3)

let mk_arr items = Set.of_list (module LookaheadItem) items
let mk_config_set kernel_items nonkernel_items =
  { LalrParsing.kernel_items = mk_arr kernel_items;
    LalrParsing.nonkernel_items = mk_arr nonkernel_items;
  }

let lookahead_item_set_set
  : LookaheadItemSet.t list -> lookahead_item_sets =
  Set.of_list (module LookaheadItemSet)

let gram1_lr1_config_sets : lookahead_configuration_set array = [|
  mk_config_set (* 0 *)
    [ { item = mk_item' 0 0; lookahead_set = SI.of_list [ 0 ] } ]
    [ { item = mk_item' 1 0; lookahead_set = SI.of_list [ 0 ] };
       { item = mk_item' 2 0; lookahead_set = SI.of_list [ 1; 2 ] };
       { item = mk_item' 3 0; lookahead_set = SI.of_list [ 1; 2 ] };
    ];
  mk_config_set (* 1 *)
    [ { item = mk_item' 0 1; lookahead_set = SI.of_list [ 0 ] } ]
    [];
  mk_config_set (* 2 *)
    [ { item = mk_item' 1 1; lookahead_set = SI.of_list [ 0 ] } ]
    [ { item = mk_item' 2 0; lookahead_set = SI.of_list [ 0 ] };
       { item = mk_item' 3 0; lookahead_set = SI.of_list [ 0 ] };
    ];
  mk_config_set (* 3 *)
    [ { item = mk_item' 2 1; lookahead_set = SI.of_list [ 0; 1; 2 ] } ]
    [ { item = mk_item' 2 0; lookahead_set = SI.of_list [ 0; 1; 2 ] };
       { item = mk_item' 3 0; lookahead_set = SI.of_list [ 0; 1; 2 ] };
    ];
  mk_config_set (* 4 *)
    [ { item = mk_item' 3 1; lookahead_set = SI.of_list [ 0; 1; 2 ] } ]
    [];
  mk_config_set (* 5 *)
    [ { item = mk_item' 1 2; lookahead_set = SI.of_list [ 0 ] } ]
    [];
  mk_config_set (* 6 *)
    [ { item = mk_item' 2 2; lookahead_set = SI.of_list [ 0; 1; 2 ] } ]
    [];
|]

let show_lookahead_item_sets : lookahead_item_sets -> string
  = fun set -> set
  |> Set.to_list
  |> List.map ~f:Grammar1Lalr.string_of_lookahead_item_set
  |> String.concat ~sep:"\n\n"

let%test_module "lalr1_items" = (module struct
    (* First check the kernels are as expected *)
 let%test "kernels" = Set.equal
   (Grammar1Lalr.lalr1_items
     |> Int.Map.data
     |> lookahead_item_set_set
   )
   (gram1_lr1_config_sets
     |> Array.to_list
     |> List.map ~f:(fun { LalrParsing.kernel_items; _ } -> kernel_items)
     |> lookahead_item_set_set
   )

 (* Then verify all closures are as expected *)
 let%test "closures" = Set.equal
   (Grammar1Lalr.lalr1_items
     |> Int.Map.data
     |> List.map ~f:Grammar1Lalr.lr1_closure
     |> lookahead_item_set_set
   )
   (gram1_lr1_config_sets
     |> Array.to_list
     |> List.map ~f:simplify_lookahead_config_set
     |> lookahead_item_set_set
   )
end)

let mk_item_set pruduction_num position =
  SI.of_list [ mk_item' pruduction_num position ]

(* CPTT Figure 4.44 *)
let expected_grammar2_lalr1_kernels : (state * item_set) list = [
  0, mk_item_set 0 0; (* S' -> . S *)
  1, mk_item_set 0 1; (* S' -> S . *)
  (* S -> L . = R
   * R -> L .
   *)
  2, SI.of_list [ mk_item' 1 1; mk_item' 5 1 ];
  3, mk_item_set 2 1; (* S -> R . *)
  4, mk_item_set 3 1; (* L -> * . R *)
  5, mk_item_set 4 1; (* L -> id . *)
  6, mk_item_set 1 2; (* S -> L = . R *)
  7, mk_item_set 3 2; (* L -> * R . *)
  8, mk_item_set 5 1; (* R -> L . *)
  9, mk_item_set 1 3; (* S -> L = R . *)
]

let mk_set : item_set list -> item_set_set
  = Set.of_list (module LrParsing.IntSet)

let actual_grammar2_lalr1_kernels : item_set_set
  = Grammar2Lalr.lalr1_items
    |> Int.Map.data
    |> List.map ~f:(fun lookahead_item_set -> lookahead_item_set
      |> Set.to_list
      |> List.map ~f:(fun lookahead_item -> lookahead_item.item)
      |> SI.of_list
    )
    |> mk_set

let show_item_set_set : LrParsing.item_set_set -> string
  = fun sets -> sets
  |> Set.to_list
  |> List.map ~f:Grammar2LR.string_of_item_set
  |> String.concat ~sep:"\n"

let%test "grammar 2 lalr1 kernel items" = Set.equal
  actual_grammar2_lalr1_kernels
  (expected_grammar2_lalr1_kernels
    |> List.map ~f:(fun (_, k) -> k)
    |> mk_set)

let book_state_mapping = expected_grammar2_lalr1_kernels
  |> Int.Map.of_alist_exn
  |> Int.Map.map ~f:Grammar2LR.item_set_to_state

let book_state : int array
  = Array.init
    (M.length book_state_mapping)
    ~f:(fun i -> book_state_mapping
      |> Fn.flip Int.Map.find i
      |> get_option' (fun () -> "expected key in book states")
    )

let no_terminal_num = 4 (* # *)

let lookahead_item_set = lookahead_item_set_from_array
  [|
    { item = mk_item' 0 0; lookahead_set = SI.of_list [ no_terminal_num ] }
  |]

let%test "grammar 2 closure" =
  (* CPTT Example 4.64 *)
  let expected_closure = lookahead_item_set_from_array
    [|
      (* S' -> . S, # *)
      { item = mk_item' 0 0; lookahead_set = SI.of_list [ no_terminal_num ] };
      (* S -> . L = R, # *)
      { item = mk_item' 1 0; lookahead_set = SI.of_list [ no_terminal_num ] };
      (* S -> . R, # *)
      { item = mk_item' 2 0; lookahead_set = SI.of_list [ no_terminal_num ] };
      (* L -> . * R, #/= *)
      { item = mk_item' 3 0; lookahead_set = SI.of_list [ no_terminal_num; 1 ] };
      (* L -> . id, #/= *)
      { item = mk_item' 4 0; lookahead_set = SI.of_list [ no_terminal_num; 1 ] };
      (* R -> . L, # *)
      { item = mk_item' 5 0; lookahead_set = SI.of_list [ no_terminal_num ] };
    |]
  in Set.equal (Grammar2Lalr.lr1_closure lookahead_item_set) expected_closure

let { spontaneous_generation; propagation } =
  Grammar2Lalr.generate_lookaheads
    (SI.of_list [ mk_item' 0 0 ])
    (mk_item' 0 0)

let expected_propagation =
  [| book_state.(1), mk_item' 0 1; (* S' -> S . *)
     book_state.(2), mk_item' 1 1; (* S -> L . = R *)
     book_state.(3), mk_item' 2 1; (* S -> R . *)
     book_state.(4), mk_item' 3 1; (* L -> * . R *)
     book_state.(5), mk_item' 4 1; (* L -> id . *)
     book_state.(2), mk_item' 5 1; (* R -> L . *)
  |]

let expected_generation = LookaheadItem.(
  [| (* L -> * . R, = *)
     book_state.(4), { item = mk_item' 3 1; lookahead_set = SI.of_list [ 1 ] };
     (* L -> id ., = *)
     book_state.(5), { item = mk_item' 4 1; lookahead_set = SI.of_list [ 1 ] };
  |])

let string_of_propagation = fun propagation -> propagation
  |> Array.map ~f:(fun (n, item) ->
    Printf.sprintf "%n -> %s" n (Grammar2LR.string_of_item item))
  |> String.concat_array ~sep:"\n"

let equivalent_propagation : (state * item) array -> (state * item) array -> bool
  = fun p1 p2 -> Set.equal
    (Set.of_array (module Propagation) p1)
    (Set.of_array (module Propagation) p2)

let equivalent_generation
  : (state * LookaheadItem.t) array -> (state * LookaheadItem.t) array -> bool
  = fun g1 g2 -> Set.equal
    (Set.of_array (module Generation) g1)
    (Set.of_array (module Generation) g2)

let string_of_generation : (state * LookaheadItem.t) array -> string
  = fun generation -> generation
    |> Array.map ~f:(fun (state, lookahead_item) ->
      Printf.sprintf "%n: %s" state (Grammar2Lalr.string_of_lookahead_item lookahead_item)
    )
    |> String.concat_array ~sep:"\n"

let%test_module "generate_lookaheads" = (module struct
  let%test "propagation" =
    equivalent_propagation propagation expected_propagation
  let%test "spontaneous_generation" =
    equivalent_generation spontaneous_generation expected_generation
end)

(*
let expected_lalr1_items
  : (MutableLookaheadItemSet.t, MutableLookaheadItemSet.comparator_witness)
    Set.t
  = let mk = fun item lookahead -> M.of_list
      [ item, Int.Table.of_alist_exn lookahead ]
    in
    Set.of_list
    (module MutableLookaheadItemSet)
    [ mk (mk_item' 0 0) [ 0 ];
       mk (mk_item' 0 1) [ 0 ];
       (M.of_alist_exn
         [ mk_item' 1 1, Int.Hash_set.of_list [ 0 ];
           mk_item' 5 1, Int.Hash_set.of_list [ 0 ];
         ]);
       mk (mk_item' 2 1) [ 0 ];
       mk (mk_item' 3 1) [ 0; 1 ];
       mk (mk_item' 4 1) [ 0; 1 ];
       mk (mk_item' 1 2) [ 0 ];
       mk (mk_item' 3 2) [ 0; 1 ];
       mk (mk_item' 5 1) [ 0; 1 ];
       mk (mk_item' 1 3) [ 0 ];
    ]

let lalr1_items_set
  : (MutableLookaheadItemSet.t, MutableLookaheadItemSet.comparator_witness)
    Set.t
  = Grammar2Lalr.mutable_lalr1_items
  |> Int.Map.data
  |> Set.of_array (module MutableLookaheadItemSet)
*)

let string_of_lalr1_items_set = fun lalr1_items_set -> lalr1_items_set
  |> Set.to_array
  |> Array.map ~f:(fun mutable_lookahead_item_set -> mutable_lookahead_item_set
    |> M.to_alist
    |> List.map ~f:(fun (item, mutable_lookahead) ->
      let lookahead_set = mutable_lookahead
        |> Hash_set.to_list
        |> SI.of_list
      in
      let lookahead_item = { LookaheadItem.item; lookahead_set } in
      Grammar2Lalr.string_of_lookahead_item lookahead_item
    )
    |> String.concat ~sep:"\n"
  )
  |> String.concat_array ~sep:"\n\n"

(* let%test "mutable_lalr1_items" = Set.equal lalr1_items_set expected_lalr1_items *)

let mk_tok name start finish : Lex.token = { name; start; finish }
let mk_terminal num start_pos end_pos =
  { production = Either.First num; children = []; start_pos; end_pos; }
let mk_wrapper prod_num ({ start_pos; end_pos; _ } as child) =
  { production = Either.Second prod_num;
    children = [ child ];
    start_pos;
    end_pos;
  }

let state = gram1_lr1_config_sets
  |> Array.map ~f:(fun { LalrParsing.kernel_items; _ } ->
    Grammar1LR.item_set_to_state @@
      lookahead_item_set_to_item_set kernel_items
  )

let (=) = Caml.(=)

let expect_no_goto = fun start_state nt_num ->
  Grammar1Lalr.lalr1_goto_table state.(start_state) (Nonterminal nt_num) = None
let expect_goto = fun start_state nt_num result_state ->
  Grammar1Lalr.lalr1_goto_table state.(start_state) (Nonterminal nt_num)
    = Some state.(result_state)

let s'_num : nonterminal_num = 0
let s_num : nonterminal_num = 1
let c_num : nonterminal_num = 2

let%test_module "lalr1_goto_table" = (module struct
  (* first, four gotos that are actually valid *)
  let%test "" = expect_goto 0 s_num 1
  let%test "" = expect_goto 0 c_num 2
  let%test "" = expect_goto 2 c_num 5
  let%test "" = expect_goto 3 c_num 6
  (* the rest of the table is empty *)
  let%test "" = expect_no_goto 0 s'_num
  let%test "" = expect_no_goto 1 s'_num
  let%test "" = expect_no_goto 2 s'_num
  let%test "" = expect_no_goto 3 s'_num
  let%test "" = expect_no_goto 4 s'_num
  let%test "" = expect_no_goto 5 s'_num
  let%test "" = expect_no_goto 6 s'_num
  let%test "" = expect_no_goto 1 s_num
  let%test "" = expect_no_goto 2 s_num
  let%test "" = expect_no_goto 3 s_num
  let%test "" = expect_no_goto 4 s_num
  let%test "" = expect_no_goto 5 s_num
  let%test "" = expect_no_goto 6 s_num
  let%test "" = expect_no_goto 1 c_num
  let%test "" = expect_no_goto 4 c_num
  let%test "" = expect_no_goto 5 c_num
  let%test "" = expect_no_goto 6 c_num
end)

let c_num : terminal_num = 1
let d_num : terminal_num = 2

let%test_module "lalr1_action_table" = (module struct

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

  let action_table_tests' = fun () -> List.iter action_table_tests
    ~f:(fun (init_state, terminal_num, action) ->
      (Printf.printf "%n %s -> %s\n"
        init_state
        (Grammar1LR.string_of_terminal terminal_num)
        (Grammar1LR.string_of_action action)
      )
      (* Grammar1Lalr.lalr1_action_table state.(init_state) terminal_num action *)
    )

  let%expect_test _ = action_table_tests' (); [%expect]
end)

let%test_module "parse" = (module struct

  let%test "cdd" =
    (* cdd
     * 0123
     *)
    let tokens1 = Queue.of_list [
      mk_tok "c" 0 1;
      mk_tok "d" 1 2;
      mk_tok "d" 2 3;
      mk_tok "$" 3 3;
    ]
    in
    Grammar1Lalr.parse (* "cdd" *) tokens1 = (Ok
      { production = Either.Second 1;
        children = [
          { production = Either.Second 2;
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
      })

  let%test "**foo = *bar" =
    let eq_num = 1 in
    let star_num = 2 in
    let id_num = 3 in

    (* **foo = *bar
     * 012345678901
     *)
    let tokens2 = Queue.of_list [
      mk_tok "*" 0 1;
      mk_tok "*" 1 2;
      mk_tok "id" 2 5;
      mk_tok "=" 6 7;
      mk_tok "*" 8 9;
      mk_tok "id" 9 12;
      mk_tok "$" 12 12;
    ]
    in

    Grammar2Lalr.parse tokens2 = (Ok
      { production = Either.Second 1;
        children = [
          { production = Either.Second 3;
            children = [
              mk_terminal star_num 0 1;
              mk_wrapper 5 @@
                { production = Either.Second 3;
                  children = [
                    mk_terminal star_num 1 2;
                    mk_wrapper 5 @@
                      { production = Either.Second 4;
                        children = [
                          mk_terminal id_num 2 5;
                        ];
                        start_pos = 2;
                        end_pos = 5;
                      };
                  ];
                  start_pos = 1;
                  end_pos = 5;
                };
            ];
            start_pos = 0;
            end_pos = 5;
          };
          mk_terminal eq_num 6 7;
          mk_wrapper 5 @@
            { production = Either.Second 3;
              children = [
                mk_terminal star_num 8 9;
                mk_wrapper 5
                  { production = Either.Second 4;
                    children = [
                      mk_terminal id_num 9 12;
                    ];
                    start_pos = 9;
                    end_pos = 12;
                  };
              ];
              start_pos = 8;
              end_pos = 12;
            };
        ];
        start_pos = 0;
        end_pos = 12;
      })

  let plus_num = 1
  let lparen_num = 2
  let rparen_num = 3
  let id_num = 4

  let%test "(x + y)" =

    (* (x + y)
     * 0123456
     *)
    let tokens3 = Queue.of_list [
      mk_tok "("  0 1;
      mk_tok "id" 1 2;
      mk_tok "+"  3 4;
      mk_tok "id" 5 6;
      mk_tok ")"  6 7;
      mk_tok "$"  7 7;
    ]
    in

    Grammar3Lalr.parse tokens3 = (Ok
      { production = Either.Second 2;
        children = [
          mk_terminal lparen_num 0 1;
          { production = Either.Second 1;
            children = [
              mk_wrapper 3 @@ mk_terminal id_num 1 2;
              mk_terminal plus_num 3 4;
              mk_wrapper 3 @@ mk_terminal id_num 5 6;
            ];
            start_pos = 1;
            end_pos = 6;
          };
          mk_terminal rparen_num 6 7;
        ];
        start_pos = 0;
        end_pos = 7;
      })

  let%test "x + (y + z)" =
    (* x + (y + z)
     * 01234567890
     *)
    let tokens4 = Queue.of_list [
      mk_tok "id" 0  1;
      mk_tok "+"  2  3;
      mk_tok "("  4  5;
      mk_tok "id" 5  6;
      mk_tok "+"  7  8;
      mk_tok "id" 9  10;
      mk_tok ")"  10 11;
      mk_tok "$"  11 11;
    ]
    in
    Grammar3Lalr.parse tokens4 = (Ok
      { production = Either.Second 1;
        children = [
          mk_wrapper 3 @@ mk_terminal id_num 0 1;
          mk_terminal plus_num 2 3;
          { production = Either.Second 2;
            children = [
              mk_terminal lparen_num 4 5;
              { production = Either.Second 1;
                children = [
                  mk_wrapper 3 @@ mk_terminal id_num 5 6;
                  mk_terminal plus_num 7 8;
                  mk_wrapper 3 @@ mk_terminal id_num 9 10;
                ];
                start_pos = 5;
                end_pos = 10;
              };
              mk_terminal rparen_num 10 11;
            ];
            start_pos = 4;
            end_pos = 11;
          };
        ];
        start_pos = 0;
        end_pos = 11;
      })

end)
