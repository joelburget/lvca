open Core_kernel
open LrParsing
module SI = Int.Set

module Grammar : GRAMMAR = struct
  let grammar = AugmentedGrammar {
    nonterminals =
    [|
       (* note: the grammar we provide is already augmented *)
       "E'", 0, { productions = [[Nonterminal 1]] }; (* E' -> E *)

       "E", 1, { productions = [
         [Nonterminal 1; Terminal 1; Nonterminal 2]; (* E -> E + T *)
         [Nonterminal 2];                            (* E -> T *)
         ]
       };

       "T", 2, { productions = [
         [Nonterminal 2; Terminal 2; Nonterminal 3]; (* T -> T * F *)
         [Nonterminal 3]                             (* T -> F *)
         ]
       };

       "F", 3, { productions = [
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
  }
end
module Lr0' = Lr0(Grammar)

let%test_module "LrParsing mk_item / view_item" = (module struct
  let (=) = Caml.(=)
  let%test "" = view_item (mk_item' 0 0)
    = { production_num = 0; position = 0 }
  let%test "" = view_item (mk_item' 0 1)
    = { production_num = 0; position = 1 }
  let%test "" = view_item (mk_item' 1 0)
    = { production_num = 1; position = 0 }
  let%test "" = view_item (mk_item' 1 1)
    = { production_num = 1; position = 1 }
end)

let%test_module "LrParsing first_set" = (module struct
  let (=) = Set.equal
  let%test "" = (Lr0'.first_set [Nonterminal 1; Terminal 1; Nonterminal 2])
    = (SI.of_list [3;5])
  let%test "" = (Lr0'.first_set [Nonterminal 2])
    = (SI.of_list [3;5])
  let%test "" = (Lr0'.first_set [Nonterminal 2; Terminal 2; Nonterminal 3])
    = (SI.of_list [3;5])
  let%test "" = (Lr0'.first_set [Nonterminal 3])
    = (SI.of_list [3;5])
  let%test "" = (Lr0'.first_set [Terminal 3; Nonterminal 1; Terminal 4])
    = (SI.of_list [3])
  let%test "" = (Lr0'.first_set [Terminal 5])
    = (SI.of_list [5])
end)

let%test_module "follow_set" = (module struct
 (*
  let show_follow_set = fun follow_set -> follow_set
    |> SI.to_array
    |> Array.map ~f:Lr0'.string_of_terminal
    |> String.concat_array ~sep:" "
  *)

  let test_follow_set nt expected_set =
    Set.equal (Lr0'.follow_set nt) (SI.of_list expected_set)

  let%test "" = test_follow_set 0 [ 0 ]
  let%test "" = test_follow_set 1 [ 0; 1; 4 ]
  let%test "" = test_follow_set 2 [ 0; 1; 2; 4 ]
  let%test "" = test_follow_set 3 [ 0; 1; 2; 4 ]
end)

(* I0 *)
let items0 = [ mk_item' 0 0 ]

(* I1 *)
let items1 = [
  mk_item' 0 1;
  mk_item' 1 1;
]

(* I7 *)
let items7 = [ mk_item' 3 2 ]

let%test_module "closure" = (module struct
  let expected0 : configuration_set =
    { kernel_items = SI.of_list items0;
      nonkernel_items = SI.of_list
        [ mk_item' 1 0;
           mk_item' 2 0;
           mk_item' 3 0;
           mk_item' 4 0;
           mk_item' 5 0;
           mk_item' 6 0;
        ]
    }

  let expected1 : configuration_set =
    { kernel_items = SI.of_list items1;
      nonkernel_items = SI.of_list [];
    }

  let expected7 : configuration_set =
    { kernel_items = SI.of_list items7;
      nonkernel_items = SI.of_list [ mk_item' 5 0; mk_item' 6 0 ];
    }

  let (=) = ConfigurationSet.equal
  let%test "" = Lr0'.lr0_closure' (SI.of_list items0) = expected0
  let%test "" = Lr0'.lr0_closure' (SI.of_list items1) = expected1
  let%test "" = Lr0'.lr0_closure' (SI.of_list items7) = expected7
end)

let%test_module "goto" = (module struct
  let lr0_goto_kernel = SI.of_list [ mk_item' 1 2 ]

  let goto_nonkernel = SI.of_list [
    mk_item' 3 0;
    mk_item' 4 0;
    mk_item' 5 0;
    mk_item' 6 0;
  ]

  let%test "" = Set.equal
    (Lr0'.lr0_goto_kernel (SI.of_list items1) (Terminal 1))
    lr0_goto_kernel

  let%test "" =  ConfigurationSet.equal
    (Lr0'.lr0_closure' @@
     Lr0'.lr0_goto_kernel (SI.of_list items1) (Terminal 1))
    ({ kernel_items = lr0_goto_kernel; nonkernel_items = goto_nonkernel }
      : configuration_set)
end)

let lr0_item_sets = [
  SI.of_list [ mk_item' 0 0 ]; (* 0 *)
  SI.of_list (* 1 *)
    [ mk_item' 0 1;
      mk_item' 1 1;
    ];
  SI.of_list (* 2 *)
    [ mk_item' 2 1;
      mk_item' 3 1;
    ];
  SI.of_list [ mk_item' 4 1; ]; (* 3 *)
  SI.of_list [ mk_item' 5 1; ]; (* 4 *)
  SI.of_list [ mk_item' 6 1; ]; (* 5 *)
  SI.of_list [ mk_item' 1 2; ]; (* 6 *)
  SI.of_list [ mk_item' 3 2; ]; (* 7 *)
  SI.of_list (* 8 *)
    [
      mk_item' 1 1;
      mk_item' 5 2;
    ];
  SI.of_list (* 9 *)
    [
      mk_item' 1 3;
      mk_item' 3 1;
    ];
  SI.of_list [ mk_item' 3 3 ]; (* 10 *)
  SI.of_list [ mk_item' 5 3 ]; (* 11 *)
]

let%test_module "lr0_items" = (module struct
  let expected_lr0_item_sets =
    Util.MutableSet.of_list (module IntSet) lr0_item_sets

  let normalize = fun items -> items
    |> Util.MutableSet.to_list
    |> List.map ~f:Int.Set.to_list

  let%test "" = Caml.(
    normalize Lr0'.mutable_lr0_items
    =
    normalize expected_lr0_item_sets
  )
end)

let state = lr0_item_sets
  |> List.map ~f:Lr0'.item_set_to_state
  |> Array.of_list
let plus_num : terminal_num = 1
let times_num : terminal_num = 2
let lparen_num : terminal_num = 3
let rparen_num : terminal_num = 4
(* TODO: use these up above *)
let id_num : terminal_num = 5
let e_num : nonterminal_num = 1
let t_num : nonterminal_num = 2
let f_num : nonterminal_num = 3

let%test_module "lr0 goto table" = (module struct
  let (=) = Caml.(=)
  (* Question: since these are all Nonterminals, what's the correct type of
   * goto? *)
  (* Test for a match with CPTT Figure 4.37 *)
  let%test "" = Lr0'.lr0_goto_table state.(0) (Nonterminal e_num) = Some state.(1)
  let%test "" = Lr0'.lr0_goto_table state.(0) (Nonterminal t_num) = Some state.(2)
  let%test "" = Lr0'.lr0_goto_table state.(0) (Nonterminal f_num) = Some state.(3)
  let%test "" = Lr0'.lr0_goto_table state.(4) (Nonterminal e_num) = Some state.(8)
  let%test "" = Lr0'.lr0_goto_table state.(4) (Nonterminal t_num) = Some state.(2)
  let%test "" = Lr0'.lr0_goto_table state.(4) (Nonterminal f_num) = Some state.(3)
  (* TODO: test other invalid GOTOs *)
  (* TODO: should this throw? *)
  let%test "" = Lr0'.lr0_goto_table state.(6) (Nonterminal e_num) = None
  let%test "" = Lr0'.lr0_goto_table state.(6) (Nonterminal t_num) = Some state.(9)
  let%test "" = Lr0'.lr0_goto_table state.(6) (Nonterminal f_num) = Some state.(3)
  let%test "" = Lr0'.lr0_goto_table state.(7) (Nonterminal e_num) = None
  let%test "" = Lr0'.lr0_goto_table state.(7) (Nonterminal t_num) = None
  let%test "" = Lr0'.lr0_goto_table state.(7) (Nonterminal f_num) = Some state.(10)
end)

let%test_module "lr0 action table" = (module struct
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

  let%test "action_table_tests" = action_table_tests
    |> List.for_all ~f:(fun (init_state, terminal_num, action) -> Caml.(
      Lr0'.lr0_action_table state.(init_state) terminal_num = action
    ))
end)

let%test_module "parse" = (module struct
  let (=) = Caml.(=)
  let mk_tok name start finish : Lex.token = { name; start; finish }
  let mk_terminal num start_pos end_pos =
    { production = Either.First num; children = []; start_pos; end_pos; }
  let mk_wrapper prod_num ({ start_pos; end_pos; _ } as child) =
    { production = Either.Second prod_num;
      children = [ child ];
      start_pos;
      end_pos;
    }

  (* TODO: test failed parses *)
  let%test "foo * bar" =
    (* foo * bar
     * 0123456789
     *)
    let tokens1 = Queue.of_array [|
      mk_tok "id" 0 3;
      mk_tok "*"  4 5;
      mk_tok "id" 6 9;
      mk_tok "$"  9 9;
    |]
    in
    Lr0'.parse tokens1 = (Ok
      (mk_wrapper 2
        { production = Either.Second 3;
          children = [
            mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 0 3;
            mk_terminal times_num 4 5;
            mk_wrapper 6 @@ mk_terminal id_num 6 9;
          ];
          start_pos = 0;
          end_pos = 9;
        }))

  (* Figure 4.38 from CPTT *)
  let%test "foo * bar + baz" =
    (* foo * bar + baz
     * 0123456789012345
     *)
    let tokens2 = Queue.of_array [|
      mk_tok "id" 0 3;
      mk_tok "*"  4 5;
      mk_tok "id" 6 9;
      mk_tok "+"  10 11;
      mk_tok "id" 12 15;
      mk_tok "$"  15 15;
    |]
    in
    Lr0'.parse tokens2 = (Ok
      { production = Either.Second 1;
        children =
          [ mk_wrapper 2
            { production = Either.Second 3;
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

  (* Figure 4.38 from CPTT *)
  let%test "foo *" =
    (* foo * bar + baz
     * 0123456789012345
     *)
    let tokens3 = Queue.of_array [|
      mk_tok "id" 0 3;
      mk_tok "*"  4 5;
    |]
    in
    Lr0'.parse tokens3 =
      (Error (4, "parsing invariant violation -- pop failed")
        : (parse_result, parse_error) Result.t)

  let%test "foo + bar" =
    (* foo + bar
     * 0123456789
     *)
    let tokens4 = Queue.of_array [|
      mk_tok "id" 0 3;
      mk_tok "+"  4 5;
      mk_tok "id" 6 9;
      mk_tok "$"  9 9;
    |]
    in
    Lr0'.parse tokens4 = (Ok
      { production = Either.Second 1;
        children = [
          mk_wrapper 2 @@ mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 0 3;
          mk_terminal plus_num 4 5;
          mk_wrapper 4 @@ mk_wrapper 6 @@ mk_terminal id_num 6 9;
        ];
        start_pos = 0;
        end_pos = 9;
      })

  let%test_unit "lex-parse" =
    let lexer = Regex.(
      [ "+", ReString "+";
        "*", ReString "*";
        "(", ReString "(";
        ")", ReString ")";
        "id", RePlus Classes.lower_alpha;
      ])
    in

    let input = "foo+bar" in
    match Lr0'.lex_and_parse lexer input with
      | Error (First { start_pos; end_pos; message }) -> failwith
        (Printf.sprintf "lexer error at chars %n-%n: %s"
          start_pos end_pos message)
      | Error (Second (char_no, msg)) -> failwith
        (Printf.sprintf "parse error at char %n: %s" char_no msg)
      | Ok _ -> ()
end)
