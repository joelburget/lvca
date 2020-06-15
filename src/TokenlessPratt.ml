open Base
module Format = Caml.Format
module Printf = Caml.Printf

type token =
  { name : string
  ; content : string
  ; lbp : int
  }

(* TODO: move lbp from lexer to parser? *)
type token_rule =
  { name : string
  ; re : Re.t
  ; lbp : int
  }
type lexer = token_rule list
type compiled_lexer = (string * Re.re * int ) list

let mk_rule = fun name re lbp -> { name; re; lbp }
let token_rules =
  [ mk_rule "+" (Re.char '+') 10
  ; mk_rule "-" (Re.char '-') 10
  ; mk_rule "*" (Re.char '*') 20
  ; mk_rule "/" (Re.char '/') 20
  ; mk_rule "(" (Re.char '(') 0
  ; mk_rule ")" (Re.char ')') 0
  ; mk_rule "lit" (Re.rep1 Re.digit) 0
  ]

let compiled_lexer = List.map token_rules
  ~f:(fun { name; re; lbp } -> name, Re.(compile (seq [ bol; re ])), lbp)

let find_match : string -> (token * string) option
  = fun input -> List.find_map compiled_lexer
    ~f:(fun (name, re, lbp) ->
      try
        let grp = Re.exec re input in
        let content = Re.Group.get grp 0 in
        let output = input
          |> String.subo ~pos:(String.length content)
          |> String.lstrip
        in
        Some ({ name; content; lbp }, output)
      with
        Caml.Not_found -> None)

exception ParseError of string

let next_token_exn : string -> token * string
  = fun input -> match find_match input with
    | None -> raise
      (ParseError (Printf.sprintf {|couldn't find a match at "%s"|} input))
    | Some (token, output) -> token, output

(* Consume all tokens with higher binding power than rbp *)
let rec expression_exn
  : ?rbp:int -> string -> string * NonBinding.term
  = fun ?rbp:(rbp=0) str -> match str with
    | "" -> raise
      (ParseError "Parsing an expression, unexpectedly reached the end of the string!")

    | _ -> (
      let rec go : NonBinding.term -> string -> string * NonBinding.term
        = fun accum str -> match str with
        | "" -> str, accum
        | _ ->
          let tok, str' = next_token_exn str in
          if tok.lbp > rbp
          then
            let str'', left = led_exn accum tok str' in
            go left str''
          else str, accum
      in

      let token, str' = next_token_exn str in
      let tokens', left = nud_exn token str' in
      go left tokens'
    )

and nud_exn : token -> string -> string * NonBinding.term
  = fun token str -> match token.name with
    | "-" ->
      let str', expr = expression_exn str in
      str', NonBinding.Operator ("neg", [expr])
    | "lit" ->
      str, Operator ("lit", [Primitive (PrimInteger (Bigint.of_string token.content))])
    | "(" ->
       let str', expr = expression_exn str in
       let token, str'' = next_token_exn str' in
       if String.(token.name = ")")
       then str'', expr
       else raise (ParseError (Printf.sprintf {|nud: expected ")", got "%s"|} token.name))
    | _ ->
      raise (ParseError (Printf.sprintf
      {|nud: unexpected token %s ("%s" doesn't have a null denotation handler)|}
      token.name token.content))

and led_exn : NonBinding.term -> token -> string -> string * NonBinding.term
  = fun lhs token str -> match token.name with
    | "+" ->
      let str', rhs = expression_exn ~rbp:token.lbp str in
      str', Operator ("add", [lhs; rhs])
    | "-" ->
      let str', rhs = expression_exn ~rbp:token.lbp str in
      str', Operator ("sub", [lhs; rhs])
    | "*" ->
      let str', rhs = expression_exn ~rbp:token.lbp str in
      str', Operator ("mul", [lhs; rhs])
    | "/" ->
      let str', rhs = expression_exn ~rbp:token.lbp str in
      str', Operator ("div", [lhs; rhs])
    | _ ->
      raise (ParseError (Printf.sprintf {|led: unexpected token %s ("%s")|}
      token.name token.content))

let%test_module "Pratt" = (module struct

  let print_parse str =
    try
      let _, tm = expression_exn str in
      NonBinding.pp Format.std_formatter tm
    with
      ParseError msg -> Printf.printf "Parse error: %s" msg

  let%expect_test _ =
    print_parse "1+2";
    [%expect {| add(lit(1); lit(2)) |}]

  let%expect_test _ =
    print_parse "1 + 2";
    [%expect {| add(lit(1); lit(2)) |}]

  let%expect_test _ =
    print_parse "-2";
    [%expect {| neg(lit(2)) |}]

  let%expect_test _ =
    print_parse "1 + 2 * 3";
    [%expect {| add(lit(1); mul(lit(2); lit(3))) |}]

  let%expect_test _ =
    print_parse "(1 + 2) *     33";
    [%expect {| mul(add(lit(1); lit(2)); lit(33)) |}]

  let%expect_test _ =
    print_parse "1 + 2 *";
    [%expect {| Parse error: Parsing an expression, unexpectedly reached the end of the string! |}]

  let%expect_test _ =
    print_parse "*";
    [%expect {| Parse error: nud: unexpected token * ("*" doesn't have a null denotation handler) |}]
end);;
