open Core_kernel
open ConcreteSyntax
module Parse_concrete = Parsing.Incremental(Parsing.Parseable_concrete_syntax)

let nt_capture capture = ConcreteSyntax.NonterminalCapture capture

let mk_terminal_capture content trailing_trivia =
  ConcreteSyntax.TerminalCapture
    { ConcreteSyntax.leading_trivia = ""; content; trailing_trivia }

let mk_tree
  :  ConcreteSyntax.tree_info
  -> ConcreteSyntax.formatted_capture array
  -> ConcreteSyntax.formatted_tree
  = fun tree_info children -> { tree_info; children }

let description = {|
ARR    := "->"
ADD    := "+"
SUB    := "-"
MUL    := "*"
DIV    := "/"
LPAREN := "("
RPAREN := ")"
FUN    := "fun"
NAME   := /[a-z][a-zA-Z0-9]*/

arith :=
  | LPAREN arith RPAREN { $2          }
  | NAME                { var($1)     }
  // > arith _       arith { app($1; $2) } %left
  > arith _ MUL _ arith { mul($1; $3) } %left
  | arith _ DIV _ arith { div($1; $3) } %left
  > arith _ ADD _ arith { add($1; $3) } %left
  | arith _ SUB _ arith { sub($1; $3) } %left
  > FUN _ NAME _ ARR _ arith { fun(var($2). $4) }
|}

let simplified_description = {|
ADD    := "+"
SUB    := "-"
NAME   := /[a-z][a-zA-Z0-9]*/

arith := arith_1 { $1 }
arith_1 :=
  | arith_2 ADD arith_1 { add($1; $3) }
  | arith_2 SUB arith_1 { add($1; $3) }
  | arith_2 { $1 }

arith_2 := NAME { var($1) }
|}

let arith = Types.SortAp ("arith", [||])
let arith' = Types.FixedValence ([], arith)
let sort_defs = Types.(SortDefs (Core_kernel.String.Map.of_alist_exn [
  "arith", SortDef ([], [
    OperatorDef ("mul", Arity ([], [ arith'; arith' ]));
    OperatorDef ("div", Arity ([], [ arith'; arith' ]));
    OperatorDef ("add", Arity ([], [ arith'; arith' ]));
    OperatorDef ("sub", Arity ([], [ arith'; arith' ]));
    OperatorDef ("app", Arity ([], [ arith'; arith' ]));
    OperatorDef ("fun", Arity ([], [ Types.FixedValence([arith], arith) ]));
  ])
]))

let concrete =
  let pre_terminal_rules, sort_rules = match Parse_concrete.parse description with
    | Error msg -> failwith msg
    | Ok desc -> desc
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules

let simplified_concrete =
  let pre_terminal_rules, sort_rules =
    match Parse_concrete.parse simplified_description with
      | Error msg -> failwith msg
      | Ok desc -> desc
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules

let%test_module "derived nonterminals" = (module struct
  let%expect_test _ =
    Array.iter (derived_nonterminal_rules concrete.nonterminal_rules)
      ~f:(fun nt_operators ->
        print_string (string_of_nonterminal_operators nt_operators)
      );
    [%expect{|
      arith:
        _ -> arith_1 { $1 }
      arith_1:
        6 -> FUN NAME ARR arith { fun(var($2). $4) }
        _ -> arith_2 { $1 }
      arith_2:
        4 -> arith_2 ADD arith_3 { add($1; $3) }
        5 -> arith_2 SUB arith_3 { sub($1; $3) }
        _ -> arith_3 { $1 }
      arith_3:
        2 -> arith_3 MUL arith_4 { mul($1; $3) }
        3 -> arith_3 DIV arith_4 { div($1; $3) }
        _ -> arith_4 { $1 }
      arith_4:
        0 -> LPAREN arith RPAREN { $2 }
        1 -> NAME { var($1) } |}]

  let LrParsing.AugmentedGrammar grammar as ag, _, _ = to_grammar concrete "arith"
  module Lr0 = LrParsing.Lr0(struct let grammar = ag end)
  module Lalr = LalrParsing.Lalr1(struct let grammar = ag end)

  let%expect_test _ =
    print_string (Lr0.string_of_grammar grammar);
    [%expect{|
      _root (0):
      arith
      arith (1):
      arith_1
      arith_1 (2):
      FUN NAME ARR arith
      arith_2
      arith_2 (3):
      arith_2 ADD arith_3
      arith_2 SUB arith_3
      arith_3
      arith_3 (4):
      arith_3 MUL arith_4
      arith_3 DIV arith_4
      arith_4
      arith_4 (5):
      LPAREN arith RPAREN
      NAME

      $ <-> 0
      SPACE <-> 1
      ARR <-> 2
      ADD <-> 3
      SUB <-> 4
      MUL <-> 5
      DIV <-> 6
      LPAREN <-> 7
      RPAREN <-> 8
      FUN <-> 9
      NAME <-> 10 |}]

  let%expect_test _ =
    let module Lex = Placemat.Lex in
    let lexer = lexer_of_desc concrete in
    (match Lex.lex lexer "x + y" with
      | Error { message; _ } -> print_string message
      | Ok tokens ->
          print_string (Lex.string_of_tokens tokens));
    [%expect{| NAME SPACE ADD SPACE NAME |}]

  (* x + y
   * 012345
   *)
  let%expect_test _ =
    let tokens = Queue.of_list Placemat.Lex.(
      [ { name  = "NAME"; start = 0; finish = 1};
        { name  = "ADD"; start = 2; finish = 3};
        { name  = "NAME"; start = 4; finish = 5};
        { name  = "$"; start = 5; finish = 5};
      ])
    in
    (match Lalr.parse tokens with
      | Ok _ -> print_string "ok"
      | Error (_, msg) -> print_string msg);
    [%expect{| ok |}]
end)

let%test_module "simplified_concrete" = (module struct
  let LrParsing.AugmentedGrammar grammar as ag, _, _ =
    to_grammar simplified_concrete "arith"
  module Lr0 = LrParsing.Lr0(struct let grammar = ag end)
  module Lalr = LalrParsing.Lalr1(struct let grammar = ag end)

  let%expect_test _ =
    print_string (Lr0.string_of_grammar grammar);
    [%expect{|
      _root (0):
      arith
      arith (1):
      arith_1
      arith_1 (2):
      arith_2 ADD arith_1
      arith_2 SUB arith_1
      arith_2
      arith_2 (3):
      NAME

      $ <-> 0
      SPACE <-> 1
      ADD <-> 2
      SUB <-> 3
      NAME <-> 4 |}]

  (* x + y
   * 012345
   *)
  let%expect_test _ =
    let tokens = Queue.of_list Placemat.Lex.(
      [ { name  = "NAME"; start = 0; finish = 1};
        { name  = "ADD";  start = 2; finish = 3};
        { name  = "NAME"; start = 4; finish = 5};
        { name  = "$";    start = 5; finish = 5};
      ])
    in
    (match Lalr.parse tokens with
      | Ok _ -> print_string "ok"
      | Error (_, msg) -> print_string msg);
    [%expect{| ok |}]
end)

let add_no = 4
let sub_no = 5
let mul_no = 2
(* let div_no = 3 *)
let fun_no = 6
let mk_op no = mk_tree ("arith", no)
let mk_var = mk_tree ("arith", 1)
let mk_parens = mk_tree ("arith", 0)

let tree1 = mk_op add_no
  [| nt_capture (mk_var [| mk_terminal_capture "x" " " |]);
     mk_terminal_capture "+" " ";
     nt_capture (mk_var [| mk_terminal_capture "y" "" |]);
  |]

let tree2 = mk_op sub_no
  [|
    nt_capture (mk_op add_no
      [| nt_capture (mk_var [| mk_terminal_capture "x" " " |]);
         mk_terminal_capture "+" " ";
         nt_capture (mk_var [| mk_terminal_capture "y" " " |]);
      |]
    );
    mk_terminal_capture "-" " ";
    nt_capture (mk_var [| mk_terminal_capture "z" "" |]);
  |]

let tree3 = mk_op add_no
  [| nt_capture (mk_var [| mk_terminal_capture "x" " " |]);
     mk_terminal_capture "+" " ";
     nt_capture (mk_op mul_no
       [| nt_capture (mk_var [| mk_terminal_capture "y" " " |]);
          mk_terminal_capture "*" " ";
          nt_capture (mk_var [| mk_terminal_capture "z" "" |]);
       |]
     );
  |]

let tree3_parens = mk_op add_no
  [| nt_capture (mk_var [| mk_terminal_capture "x" " " |]);
     mk_terminal_capture "+" " ";
     nt_capture (mk_parens
       [|
         mk_terminal_capture "(" "";
         nt_capture (mk_op mul_no
           [| nt_capture (mk_var [| mk_terminal_capture "y" " " |]);
              mk_terminal_capture "*" " ";
              nt_capture (mk_var [| mk_terminal_capture "z" "" |]);
           |]
         );
         mk_terminal_capture ")" "";
       |]
     );
  |]

let tree4 = mk_op fun_no
  [| mk_terminal_capture "fun" " ";
     mk_terminal_capture "x" " ";
     mk_terminal_capture "->" " ";
     nt_capture (mk_var [| mk_terminal_capture "x" "" |]);
  |]

let tree4' = mk_op fun_no
  [| mk_terminal_capture "fun" " ";
     mk_terminal_capture "x" "";
     mk_terminal_capture "->" "";
     nt_capture (mk_var [| mk_terminal_capture "x" "" |]);
  |]

let tree1_ast =
  Binding.Nominal.(Operator ("add",
    [ Scope ([], Var "x");
      Scope ([], Var "y");
    ]))

let tree2_ast = Binding.Nominal.(Operator ("sub",
  [ Scope ([], tree1_ast);
    Scope ([], Var "z");
  ]))

let tree3_ast = Binding.Nominal.(Operator ("add",
  [ Scope ([], Var "x");
    Scope ([], Operator ("mul",
    [ Scope ([], Var "y");
      Scope ([], Var "z");
    ]));
  ]))

let tree4_ast =
  Binding.Nominal.(Operator ("fun",
    [ Scope ([Var "x"], Var "x");
    ]))

let%test_module "ConcreteSyntax" = (module struct
  let (=) = Caml.(=)

  let parse_print str = match parse concrete "arith" str with
    | Ok tree -> print_string (to_string tree)
    | Error msg -> print_string msg

  let%test_unit "language parses" = match Parse_concrete.parse description with
    | Ok _concrete -> ()
    | Error msg    -> failwith msg

  let%test "of_ast tree1" =
    of_ast concrete "arith" 80 tree1_ast = tree1
  let%test "of_ast tree4" =
    of_ast concrete "arith" 80 tree4_ast = tree4

  let%test "to_ast tree1" = to_ast concrete tree1 = Ok tree1_ast
  let%test "to_ast tree4" = to_ast concrete tree4 = Ok tree4_ast

  let%expect_test "to_string tree1" =
    print_string (to_string tree1);
    [%expect{|x + y|}]
  let%expect_test "to_string tree4" =
    print_string (to_string tree4);
    [%expect{|fun x -> x|}]

  let%expect_test {|parse "x + y"|} =
    parse_print "x + y";
    [%expect{| x + y |}]

  let%test {|parse "x+y"|} =
    parse concrete "arith" "x+y" = Ok (remove_spaces tree1)

  let%expect_test {|parse "x+y"|} =
    parse_print "x+y";
    [%expect{| x+y |}]

  let%expect_test {|parse "x+y-z"|} =
    parse_print "x+y-z";
    [%expect{| x+y-z |}]
  let%test {|parse "x+y-z"|} =
    parse concrete "arith" "x+y-z" = Ok (remove_spaces tree2)
  let%expect_test {|parse "x + y - z"|} =
    parse_print "x + y - z";
    [%expect{| x + y - z |}]
  let%test {|parse "x + y - z"|} =
    parse concrete "arith" "x + y - z" = Ok tree2

  let%expect_test {|parse "x + y * z"|} =
    parse_print "x + y * z";
    [%expect{| x + y * z |}]
  let%test {|parse "x + y * z"|} =
    parse concrete "arith" "x + y * z" = Ok tree3
  let%expect_test {|parse "x + (y * z)"|} =
    parse_print "x + (y * z)";
    [%expect{| x + (y * z) |}]

  let%test {|parse "x + (y * z)"|} =
    parse concrete "arith" "x + (y * z)" = Ok tree3_parens
  let%expect_test {|parse "x+(y*z)"|} =
    parse_print "x+(y*z)";
    [%expect{| x+(y*z) |}]
  let%test {|parse "x+(y*z)"|} =
    parse concrete "arith" "x+(y*z)" = Ok (remove_spaces tree3_parens)

  let%expect_test {|parse "fun x -> x"|} =
    parse_print "fun x -> x";
    [%expect{| fun x -> x |}]
  let%test {|parse "fun x -> x"|} =
    parse concrete "arith" "fun x -> x" = Ok tree4
  let%expect_test {|parse "fun x->x"|} =
    parse_print "fun x->x";
    [%expect{| fun x->x |}]
  let%test {|parse "fun x->x"|} =
    parse concrete "arith" "fun x->x" = Ok tree4'
end)

let expect_round_trip_tree tree = equivalent (tree
    |> to_ast concrete
    |> Result.map ~f:(of_ast concrete "arith" 80)
    |> Result.ok_or_failwith
  ) tree

let expect_round_trip_ast tm = Caml.((tm
    |> of_ast concrete "arith" 80
    |> to_ast concrete
  ) = Ok tm)

let%test_module "round trip tree -> ast -> tree" = (module struct
  let%test "tree1" = expect_round_trip_tree tree1
  let%test "tree2" = expect_round_trip_tree tree2
  let%test "tree3" = expect_round_trip_tree tree3
  let%test "tree4" = expect_round_trip_tree tree4
end)

let%test_module "round trip ast -> tree -> ast" = (module struct
  let%test "tree1_ast" = expect_round_trip_ast tree1_ast
  let%test "tree2_ast" = expect_round_trip_ast tree2_ast
  let%test "tree3_ast" = expect_round_trip_ast tree3_ast
  let%test "tree4_ast" = expect_round_trip_ast tree4_ast
end)
