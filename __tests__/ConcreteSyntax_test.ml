open Jest
open Expect
let to_ast, to_string, of_ast, parse, equivalent, remove_spaces =
  ConcreteSyntax.(to_ast, to_string, of_ast, parse, equivalent, remove_spaces)
module Parse_concrete = Parsing.Incremental(Parsing.Parseable_concrete_syntax)
open TestUtil
type sort_name = Types.sort_name
type formatted_tree = ConcreteSyntax.formatted_tree

let nt_capture capture = ConcreteSyntax.NonterminalCapture capture

let mk_terminal_capture content trailing_trivia =
  ConcreteSyntax.TerminalCapture
    { ConcreteSyntax.leading_trivia = ""; content; trailing_trivia }

let mk_tree
  :  ConcreteSyntax.tree_info
  -> ConcreteSyntax.formatted_capture array
  -> ConcreteSyntax.formatted_tree
  = fun tree_info children -> { tree_info; children }

  (*
(** lift [equivalent] to result *)
let equivalent'
  :  (formatted_tree, string) Result.t
  -> (formatted_tree, string) Result.t
  -> bool
  = fun x y -> match x, y with
  | Ok x', Ok y' -> equivalent x' y'
  | Error x', Error y' -> x' = y'
  | _ -> false

(** lift [to_string] to result *)
let to_string' : (formatted_tree, string) Result.t -> string
  = function
    | Ok tree -> to_string tree
    | Error msg -> msg
    *)

let _ = describe "ConcreteSyntax" (fun () ->
  let description = {|
  ARR    := "->"
  ADD    := "+"
  SUB    := "-"
  MUL    := "*"
  DIV    := "/"
  LPAREN := "("
  RPAREN := ")"
  NAME   := /[a-z][a-zA-Z0-9]*/

  arith :=
    | LPAREN arith RPAREN { $2          }
    | NAME                { var($1)     }
    > arith _       arith { app($1; $2) } %left
    > arith _ MUL _ arith { mul($1; $3) } %left
    | arith _ DIV _ arith { div($1; $3) } %left
    > NAME  _ ARR _ arith { fun($1. $3) }
    > arith _ ADD _ arith { add($1; $3) } %left
    | arith _ SUB _ arith { sub($1; $3) } %left
  |}
  in

  let arith = Types.SortAp ("arith", [||]) in
  let arith' = Types.FixedValence ([], arith) in
  let sort_defs = Types.(SortDefs (Tablecloth.StrDict.fromList [
    "arith", SortDef ([], [
      OperatorDef ("mul", Arity ([], [ arith'; arith' ]));
      OperatorDef ("div", Arity ([], [ arith'; arith' ]));
      OperatorDef ("add", Arity ([], [ arith'; arith' ]));
      OperatorDef ("sub", Arity ([], [ arith'; arith' ]));
      OperatorDef ("app", Arity ([], [ arith'; arith' ]));
      OperatorDef ("fun", Arity ([], [ Types.FixedValence([arith], arith) ]));
    ])
  ]))
  in

  test "language parses" (fun () ->
    match Parse_concrete.parse description with
      | Ok _concrete -> pass
      | Error msg    -> fail msg
  );

  match Parse_concrete.parse description with
    | Error msg -> failwith msg
    | Ok (pre_terminal_rules, sort_rules) ->
      let concrete =
        ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
      in

      let add_no = 6 in
      let sub_no = 7 in
      let mul_no = 3 in
      let fun_no = 5 in
      let mk_op no = mk_tree ("arith", no) in
      let mk_var = mk_tree ("arith", 6) in
      let mk_parens = mk_tree ("arith", 7) in

      let tree1 = mk_op add_no
        [| nt_capture (mk_var [| mk_terminal_capture "x" " " |]);
           mk_terminal_capture "+" " ";
           nt_capture (mk_var [| mk_terminal_capture "y" "" |]);
        |]
      in

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
      in

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
      in

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
      in

      let tree4 = mk_op fun_no
        [| nt_capture (mk_var [| mk_terminal_capture "x" " " |]);
           mk_terminal_capture "->" " ";
           nt_capture (mk_var [| mk_terminal_capture "x" "" |]);
        |]
      in

      let tree4' = mk_op fun_no
        [| mk_terminal_capture "x" " ";
           mk_terminal_capture "->" " ";
           mk_terminal_capture "x" "";
        |]
      in

      let tree1_ast =
        Binding.Nominal.(Operator ("add",
          [ Scope ([], Var "x");
            Scope ([], Var "y");
          ]))
      in

      let tree2_ast = Binding.Nominal.(Operator ("sub",
        [ Scope ([], tree1_ast);
          Scope ([], Var "z");
        ]))
      in

      let tree3_ast = Binding.Nominal.(Operator ("add",
        [ Scope ([], Var "x");
          Scope ([], Operator ("mul",
          [ Scope ([], Var "y");
            Scope ([], Var "z");
          ]));
        ]))
      in

      let tree4_ast =
        Binding.Nominal.(Operator ("fun",
          [ Scope ([Var "x"], Var "x");
          ]))
      in

      Todo.test "of_ast";
      Todo.test "to_ast";
      Todo.test "to_string";
      (*
      testAll "of_ast" [
        (* TODO: should have spaces *)
        expect (of_ast sort_defs concrete "arith" 80 tree1_ast)
          |> toEqual tree1;
        (*
        expect (of_ast sort_defs concrete "arith" 80 tree4_ast)
          |> toEqual tree4;
          *)
      ] Util.id;

      testAll "to_ast" [
        expect (to_ast concrete tree1)
          |> toEqual (Ok tree1_ast);
        expect (to_ast concrete tree4)
          |> toEqual (Ok tree4_ast);
      ] Util.id;

      testAll "to_string" [
        expect (to_string tree1) |> toEqual "x + y";
        expect (to_string tree4) |> toEqual "x -> x";
      ] Util.id;
      *)

      (*
      Js.log (Js.Json.stringifyAny (Ok tree3));
      Js.log (Js.Json.stringifyAny (parse concrete "arith" "x + y * z"));
      *)

      describe "parse" (fun () ->
        test "x + y" (fun () ->
          expect (parse concrete "arith" "x + y")
            |> toEqual (Ok tree1);
        );
        (*
        test "x+y" (fun () ->
          expect (parse concrete "arith" "x+y")
            |> toEqual (Ok (remove_spaces tree1));
        );

        test "x+y-z" (fun () ->
          expect (parse concrete "arith" "x+y-z")
            |> toEqual (Ok (remove_spaces tree2));
        );
        (*
        test "x + y - z" (fun () ->
          expect (parse concrete "arith" "x + y - z")
            |> toEqual (Ok tree2);
        );
        *)

        test "x + y * z" (fun () ->
          expect (parse concrete "arith" "x + y * z") |> toEqual (Ok tree3);
        );
        *)

          (*
          expect (parse concrete "arith" "x + (y * z)")
            |> toEqual (Ok tree3);
          expect (parse concrete "arith" "x+(y*z)")
            |> toEqual (Ok (remove_spaces tree3));
            *)

          (*
          expect (parse concrete "arith" "x -> x")
            |> toEqual (Ok tree4);
          expect (parse concrete "arith" "x->x")
            |> toEqual (Ok (remove_spaces tree4));
            *)

        );

      Todo.test "round trip tree -> ast -> tree";
      Todo.test "round trip ast -> tree -> ast";
      (*
      let expect_round_trip_tree tree = expect (tree
          |> to_ast concrete
          |. Result.map (of_ast sort_defs concrete "arith" 80)
          |. Result.getExn
        ) |> toBeEquivalent to_string equivalent tree
      in

      let expect_round_trip_ast tm = expect (tm
          |> of_ast sort_defs concrete "arith" 80
          |> to_ast concrete
        ) |> toEqual (Ok tm)
      in

      testAll "round trip tree -> ast -> tree"
        [ expect_round_trip_tree tree1;
          expect_round_trip_tree tree2;
          (* expect_round_trip_tree tree3; *)
          (* expect_round_trip_tree tree4; *)
        ] Util.id;

      testAll "round trip ast -> tree -> ast"
        [ expect_round_trip_ast tree1_ast;
          expect_round_trip_ast tree2_ast;
          (* expect_round_trip_ast tree3_ast; *)
          (* expect_round_trip_ast tree4_ast; *)
        ] Util.id;
        *)
)
