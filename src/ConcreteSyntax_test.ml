open Core_kernel
open ConcreteSyntax
module Parse_abstract = Parsing.Incremental (Parsing.Parseable_abstract_syntax)
module Parse_concrete = Parsing.Incremental (Parsing.Parseable_concrete_syntax)

let nt_capture capture = ConcreteSyntax.NonterminalCapture capture

let mk_terminal_capture content trailing_trivia =
  ConcreteSyntax.TerminalCapture
    { ConcreteSyntax.leading_trivia = ""; content; trailing_trivia }
;;

let mk_tree
    :  ConcreteSyntax.tree_info -> ConcreteSyntax.formatted_capture array
    -> ConcreteSyntax.formatted_tree
  =
 fun tree_info children -> { tree_info; children }
;;

let abstract_description =
  {|
arith :=
  | fun(arith(). arith())
  | add(arith(); arith())
  | sub(arith(); arith())
  | mul(arith(); arith())
  | div(arith(); arith())
  | app(arith(); arith())
  |}
;;

let description =
  {|
ARR    := "->"
ADD    := "+"
SUB    := "-"
MUL    := "*"
DIV    := "/"
LPAREN := "("
RPAREN := ")"
FUN    := "fun"
NAME   := /[a-z][a-zA-Z0-9]*/

arith := arith_1 { $1 }

arith_1 :=
  | [ FUN _ NAME _ ARR _ arith_1 ] { fun(var($2). $4) }
  | arith_2 { $1 }

arith_2 :=
  | [ arith_2 _ ADD _ arith_3 ] { add($1; $3) }
  | [ arith_2 _ SUB _ arith_3 ] { sub($1; $3) }
  | arith_3 { $1 }

arith_3 :=
  | [ arith_3 _ MUL _ arith_4 ] { mul($1; $3) }
  | [ arith_3 _ DIV _ arith_4 ] { div($1; $3) }
  | arith_4 { $1 }

arith_4 :=
  | [ arith_4 _ arith_5 ] { app($1; $2) }
  | arith_5 { $1 }

arith_5 :=
  | NAME                      { var($1) }
  | [ LPAREN arith_1 RPAREN ] { $2      }
|}
;;

let simplified_description =
  {|
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
;;

let boxes_description =
  {|
HBOX := "hbox"
VBOX := "vbox"
HOVBOX := "hovbox"
HVBOX := "hvbox"
COLON := ":"
NUMBER := /[0-9]+/

numbers :=
  | NUMBER { cons(integer($1); nil()) }
  | NUMBER _ numbers { cons(integer($1); $2) }

tm :=
  | [<v> HBOX   COLON _ [<h>   numbers] ] { hbox($3) }
  | [<v> VBOX   COLON _ [<v>   numbers] ] { vbox($3) }
  | [<v> HOVBOX COLON _ [<hov> numbers] ] { hovbox($3) }
  | [<v> HVBOX  COLON _ [<hv>  numbers] ] { hvbox($3) }
  |}
;;

let int_list_desc =
  {|
INT := /[0-9]+/

list :=
  | INT int_list
  |}
;;

let arith = Types.SortAp ("arith", [||])
let arith' = Types.FixedValence ([], arith)

let { Types.sort_defs; _ } = match Parse_abstract.parse abstract_description with
  | Error msg -> failwith msg
  | Ok lang -> lang
;;

let concrete =
  let pre_terminal_rules, sort_rules =
    match Parse_concrete.parse description with
    | Error msg -> failwith msg
    | Ok desc -> desc
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
;;

let simplified_concrete =
  let pre_terminal_rules, sort_rules =
    match Parse_concrete.parse simplified_description with
    | Error msg -> failwith msg
    | Ok desc -> desc
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
;;

let boxes_concrete =
  let pre_terminal_rules, sort_rules =
    match Parse_concrete.parse boxes_description with
    | Error msg -> failwith msg
    | Ok desc -> desc
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
;;

let%test_module "simplified_concrete" =
  (module struct
    let (LrParsing.AugmentedGrammar grammar as ag), _ =
      to_grammar simplified_concrete "arith"
    ;;

    module Lr0 = LrParsing.Lr0 (struct
      let grammar = ag
    end)

    module Lalr = LalrParsing.Lalr1 (struct
      let grammar = ag
    end)

    let%expect_test _ =
      print_string (Lr0.string_of_grammar grammar);
      [%expect
        {|
      _root (0):
        0: arith
      arith (1):
        1: arith_1
      arith_1 (2):
        2: arith_2 ADD arith_1
        3: arith_2 SUB arith_1
        4: arith_2
      arith_2 (3):
        5: NAME

      $ <-> 0
      SPACE <-> 1
      ADD <-> 2
      SUB <-> 3
      NAME <-> 4 |}]
    ;;

    (* x + y
     * 012345
     *)
    let%expect_test _ =
      let tokens =
        Queue.of_list
          Lex.
            [ { name = "NAME"; start = 0; finish = 1 }
            ; { name = "ADD"; start = 2; finish = 3 }
            ; { name = "NAME"; start = 4; finish = 5 }
            ; { name = "$"; start = 5; finish = 5 }
            ]
      in
      (match Lalr.parse tokens with
      | Ok _ -> print_string "ok"
      | Error (_, msg) -> print_string msg);
      [%expect {| ok |}]
    ;;
  end)
;;

let arith_1 n = mk_tree ("arith_1", n)
let arith_2 n = mk_tree ("arith_2", n)
let arith_3 n = mk_tree ("arith_3", n)
let arith_4 n = mk_tree ("arith_4", n)
let arith_5 n = mk_tree ("arith_5", n)

let wrap_arith : formatted_tree -> formatted_tree
  = fun child -> mk_tree ("arith", 0) [| nt_capture child |]
let wrap_arith_1 child = mk_tree ("arith_1", 1) [| nt_capture child |]
let wrap_arith_2 child = mk_tree ("arith_2", 2) [| nt_capture child |]
let wrap_arith_3 child = mk_tree ("arith_3", 2) [| nt_capture child |]
let wrap_arith_4 child = mk_tree ("arith_4", 1) [| nt_capture child |]

let mk_fun = arith_1 0
let mk_add = arith_2 0
let mk_sub = arith_2 1
let mk_mul = arith_3 0
(* let mk_div = arith_3 1 *)
(* let mk_app = arith_4 0 *)
let mk_var = arith_5 0
let mk_parens = arith_5 1

let tree1 =
  wrap_arith @@ wrap_arith_1 @@ mk_add
    [| nt_capture @@ wrap_arith_2 @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "x" " " |]
     ; mk_terminal_capture "+" " "
     ; nt_capture @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "y" "" |]
    |]
;;

let tree2 =
  wrap_arith @@ wrap_arith_1 @@ mk_sub
    [| nt_capture
         (mk_add
            [| nt_capture @@ wrap_arith_2 @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "x" " " |]
             ; mk_terminal_capture "+" " "
             ; nt_capture @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "y" " " |]
            |])
     ; mk_terminal_capture "-" " "
     ; nt_capture (wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "z" "" |])
    |]
;;

let tree3 =
  wrap_arith @@ wrap_arith_1 @@ mk_add
    [| nt_capture @@ wrap_arith_2 @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "x" " " |]
     ; mk_terminal_capture "+" " "
     ; nt_capture
         (mk_mul
            [| nt_capture @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "y" " " |]
             ; mk_terminal_capture "*" " "
             ; nt_capture @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "z" "" |]
            |])
    |]
;;

let tree3_parens =
  wrap_arith @@ wrap_arith_1 @@ mk_add
    [| nt_capture @@ wrap_arith_2 @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "x" " " |]
     ; mk_terminal_capture "+" " "
     ; nt_capture @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_parens
        [| mk_terminal_capture "(" ""
         ; nt_capture @@ wrap_arith_1 @@ wrap_arith_2 @@ mk_mul
            [| nt_capture @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "y" " " |]
             ; mk_terminal_capture "*" " "
             ; nt_capture @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "z" "" |]
            |]
         ; mk_terminal_capture ")" ""
        |]
    |]
;;

let tree4 =
  wrap_arith @@ mk_fun
    [| mk_terminal_capture "fun" " "
     ; mk_terminal_capture "x" " "
     ; mk_terminal_capture "->" " "
     ; nt_capture @@ wrap_arith_1 @@ wrap_arith_2 @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "x" "" |]
    |]
;;

let tree4' =
  wrap_arith @@ mk_fun
    [| mk_terminal_capture "fun" " "
     ; mk_terminal_capture "x" ""
     ; mk_terminal_capture "->" ""
     ; nt_capture @@ wrap_arith_1 @@ wrap_arith_2 @@ wrap_arith_3 @@ wrap_arith_4 @@ mk_var [| mk_terminal_capture "x" "" |]
    |]
;;

let tree1_ast =
  Binding.Nominal.(Operator ("add", [ Scope ([], Var "x"); Scope ([], Var "y") ]))
;;

let tree2_ast =
  Binding.Nominal.(Operator ("sub", [ Scope ([], tree1_ast); Scope ([], Var "z") ]))
;;

let tree3_ast =
  Binding.Nominal.(
    Operator
      ( "add"
      , [ Scope ([], Var "x")
        ; Scope ([], Operator ("mul", [ Scope ([], Var "y"); Scope ([], Var "z") ]))
        ] ))
;;

let tree4_ast = Binding.Nominal.(Operator ("fun", [ Scope ([ Var "x" ], Var "x") ]))

let of_ast' = of_ast sort_defs concrete arith "arith"

let%test_module "ConcreteSyntax" =
  (module struct
    let ( = ) = Caml.( = )

    let parse_print str =
      match parse concrete "arith" str with
      | Ok tree -> print_string (to_string tree)
      | Error msg -> print_string msg
    ;;

    let%test_unit "language parses" =
      match Parse_concrete.parse description with
      | Ok _concrete -> ()
      | Error msg -> failwith msg
    ;;

    let%test "of_ast tree1" = of_ast' 80 tree1_ast = tree1
    let%test "of_ast tree4" =
      let x = of_ast' 80 tree4_ast in
      (* Printf.printf "%s\n" (to_debug_string x); *)
      (* Printf.printf "%s\n" (to_debug_string tree4); *)
      x = tree4
    let%test "to_ast tree1" = to_ast concrete tree1 = Ok tree1_ast
    let%test "to_ast tree4" = to_ast concrete tree4 = Ok tree4_ast


    let%expect_test "to_string tree1" =
      print_string (to_string tree1);
      [%expect {|x + y|}]
    ;;

    let%expect_test "to_string tree4" =
      print_string (to_string tree4);
      [%expect {|fun x -> x|}]
    ;;

    let%expect_test {|parse "x + y"|} =
      parse_print "x + y";
      [%expect {| x + y |}]
    ;;

    let%test {|parse "x+y"|} = parse concrete "arith" "x+y" = Ok (remove_spaces tree1)

    let%expect_test {|parse "x+y"|} =
      parse_print "x+y";
      [%expect {| x+y |}]
    ;;

    let%expect_test {|parse "x+y-z"|} =
      parse_print "x+y-z";
      [%expect {| x+y-z |}]
    ;;

    let%test {|parse "x+y-z"|} = parse concrete "arith" "x+y-z" = Ok (remove_spaces tree2)

    let%expect_test {|parse "x + y - z"|} =
      parse_print "x + y - z";
      [%expect {| x + y - z |}]
    ;;

    let%test {|parse "x + y - z"|} = parse concrete "arith" "x + y - z" = Ok tree2

    let%expect_test {|parse "x + y * z"|} =
      parse_print "x + y * z";
      [%expect {| x + y * z |}]
    ;;

    let%test {|parse "x + y * z"|} = parse concrete "arith" "x + y * z" = Ok tree3

    let%expect_test {|parse "x + (y * z)"|} =
      parse_print "x + (y * z)";
      [%expect {| x + (y * z) |}]
    ;;

    let%test {|parse "x + (y * z)"|} =
      parse concrete "arith" "x + (y * z)" = Ok tree3_parens
    ;;

    let%expect_test {|parse "x+(y*z)"|} =
      parse_print "x+(y*z)";
      [%expect {| x+(y*z) |}]
    ;;

    let%test {|parse "x+(y*z)"|} =
      parse concrete "arith" "x+(y*z)" = Ok (remove_spaces tree3_parens)
    ;;

    let%expect_test {|parse "fun x -> x"|} =
      parse_print "fun x -> x";
      [%expect {| fun x -> x |}]
    ;;

    let%test {|parse "fun x -> x"|} = parse concrete "arith" "fun x -> x" = Ok tree4

    let%expect_test {|parse "fun x->x"|} =
      parse_print "fun x->x";
      [%expect {| fun x->x |}]
    ;;

    let%test {|parse "fun x->x"|} = parse concrete "arith" "fun x->x" = Ok tree4'
  end)
;;

let expect_round_trip_tree tree =
  equivalent
    (tree
    |> to_ast concrete
    |> Result.map ~f:(of_ast' 80)
    |> Result.ok_or_failwith)
    tree
;;

let expect_round_trip_ast tm =
  Caml.(tm |> of_ast' 80 |> to_ast concrete = Ok tm)
;;

let%test_module "round trip tree -> ast -> tree" =
  (module struct
    let%test "tree1" = expect_round_trip_tree tree1
    let%test "tree2" = expect_round_trip_tree tree2
    let%test "tree3" = expect_round_trip_tree tree3
    let%test "tree4" = expect_round_trip_tree tree4
  end)
;;

let%test_module "round trip ast -> tree -> ast" =
  (module struct
    let%test "tree1_ast" = expect_round_trip_ast tree1_ast
    let%test "tree2_ast" = expect_round_trip_ast tree2_ast
    let%test "tree3_ast" = expect_round_trip_ast tree3_ast
    let%test "tree4_ast" = expect_round_trip_ast tree4_ast
  end)
;;

let%test_module "pretty-printing" =
  (module struct
    let%expect_test "tree3_ast 5" =
      print_string (to_string (of_ast' 5 tree3_ast));
      [%expect{|
        x
          +
          y
            *
            z |}]
    let%expect_test "tree3_ast 7" =
      print_string (to_string (of_ast' 7 tree3_ast));
      [%expect{|
        x
          +
          y * z |}]
    let%expect_test "tree3_ast 9" =
      print_string (to_string (of_ast' 9 tree3_ast));
      [%expect{| x + y * z |}]
  end)
;;

(* Temporarily disabled until I decide pretty-printing next steps
let%test_module "box test" =
  (module struct
    open Result.Let_syntax

    let parse_print size str =
      let result =
        let%bind tree = parse boxes_concrete "tm" str in
        let%map ast = to_ast boxes_concrete tree in
        print_string @@ to_string @@ of_ast boxes_concrete "tm" size ast
      in
      match result with
        | Ok () -> ()
        | Error msg -> print_string msg
    ;;

    let%expect_test "hbox: 1 2 3 4 5" =
      parse_print 14 "hbox: 1 2 3 4 5";
      [%expect{|
        hbox:
          1 2 3 4 5 |}]

    let%expect_test "vbox: 1 2 3 4 5" =
      parse_print 14 "vbox: 1 2 3 4 5";
      [%expect{|
        vbox:
          1
          2
          3
          4
          5 |}]

    let%expect_test "hovbox: 1 2 3 4 5" =
      parse_print 16 "hovbox: 1 2 3 4 5";
      [%expect{|
        hovbox: 1 2 3 4
                5 |}]

    let%expect_test "hvbox: 1 2 3 4 5" =
      parse_print 15 "hvbox: 1 2 3 4 5";
      [%expect{|
        hvbox:
          1
          2
          3
          4
          5 |}]

  end)
  *)

  (*
let%test_module "pattern test" =
  (module struct
    let parse_print str =
      match parse pattern_concrete "tm" str with
      | Ok tree -> print_string (to_string tree)
      | Error msg -> print_string msg
    ;;

    let test_str = {|
      match tm with
        | Concat("foo", "bar") -> "foobar"
        | Concat(a, b) -> "not foobar"
        | Add(1, 2) -> "3"
        | Add(a, b) -> "not 3"
        | Append([1], [2]) -> "1, 2"
        | Append([x], [y]) -> "not 1, 2"
        | Append(x, y) -> "really not 1, 2"
        | _ -> "no clue"
      |}

    let%expect_test = parse_print test_str; [%expect]
    let%expect _ =

    let maybe_ast = test_str
      |> parse pattern_concrete "tm"
      |> Result.map (to_ast pattern_concrete)
    in

    let expected_ast = ...

    maybe_ast = Ok expected_ast
  end)
  *)

let%test_module "validation test" =
  (module struct
    let print_check desc_str =
      let pre_terminal_rules, sort_rules =
        match Parse_concrete.parse desc_str with
        | Error msg -> failwith msg
        | Ok desc -> desc
      in
      let concrete_desc =
        ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
      in
      print_string (match check_description_validity concrete_desc with
        | None -> ""
        | Some (InvalidGrammar msg) -> msg)
      ;;

    let prelude_print_check desc_str =
      let desc_str' = {|
          FOO := "foo"
          BAR := "bar"
          BAZ := "baz"
          VAR := /[a-z][a-zA-Z0-9_]*/
      |} ^ desc_str
      in print_check desc_str'

    let%expect_test _ =
      print_check {|
        FOO := ""
        FOO := ""
        tm := FOO { op() }
        |};
      [%expect{| Duplicate terminal definition: FOO |}]

    let%expect_test _ =
      print_check {|
        FOO := ""
        tm := FOO { op() }
        |};
      [%expect{| Regex accepts empty strings: |}]

    let%expect_test _ =
      print_check {|
        FOO := /x?/
        tm := FOO { op() }
        |};
      [%expect{| Uncaptured regex which is not a string literal: /x?/ |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO VAR { op() }";
      [%expect{| Uncaptured regex which is not a string literal: /[a-z][a-zA-Z0-9_]*/ |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO bar BAZ { op() }";
      [%expect{| uncaptured nonterminal: bar |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO bar BAZ { op($2) }";
      [%expect{| |}]

    let%expect_test _ =
      prelude_print_check "tm := [ FOO bar BAZ { op($2) }";
      [%expect{| At least one group is not closed (there are more open box markers ('[') than close box markers (']')) |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO bar BAZ ] { op($2) }";
      [%expect{| Invalid box structure (saw a close box marker (']') before its opening marker ('['))! |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO BAZ { op() }";
      [%expect{| |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO QUUX { op() }";
      [%expect{| Named terminal QUUX does not exist |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO bar BAZ { op($2; $2) }";
      [%expect{| tokens captured more than once: $2 |}]

    let%expect_test _ =
      prelude_print_check
        "tm := FOO bar BAZ QUUX { op(string($1); $2; var($3). integer($4)) }";
      [%expect{| |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO bar BAZ { op($1; $2) }";
      [%expect{| Terminals can only be captured by `var`, `integer`, and `string`: $1 (FOO) |}]

    let%expect_test _ =
      prelude_print_check "tm := FOO bar BAZ { op($2; $4) }";
      [%expect{| Couldn't find captured token $4 |}]

  end)
