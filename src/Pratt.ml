open Base
open Binding
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
  ; lbp : int option
  }
type lexer = token_rule list
type compiled_lexer = (string * Re.re * int option) list

let find_match : compiled_lexer -> string -> (string * string * int option) option
  = fun lexer input -> List.find_map lexer
    ~f:(fun (re_name, re, lbp) ->
      try
        let grp = Re.exec re input in
        let substr = Re.Group.get grp 0 in
        Some (re_name, substr, lbp)
      with
        Caml.Not_found -> None)

type parse_result = (Nominal.term, [ `ParseError of string ]) Result.t

type expression_parser = ?rbp:int -> token list -> token list * parse_result

type led_parser =
  expression_parser -> Nominal.term -> token -> token list -> token list * parse_result
type nud_parser =
  expression_parser ->                 token -> token list -> token list * parse_result

type token_parser =
  { nud : nud_parser option
  ; led : led_parser option
  }

type parser = token_parser Util.String.Map.t

(* Consume all tokens with higher binding power than rbp *)
let rec expression
  : parser -> ?rbp:int -> token list -> token list * parse_result
  = fun parser ?rbp:(rbp=0) tokens -> match tokens with
    | [] -> tokens, Error (`ParseError "Parsing an expression, ran out of tokens!")
    | token :: tokens ->

      let rec go : Nominal.term -> token list -> token list * parse_result
        = fun accum tokens -> match tokens with
        | [] -> tokens, Ok accum
        | tok :: tokens' ->
          if tok.lbp > rbp
          then
            let tokens'', result = led parser accum tok tokens' in
            match result with
              | Ok left -> go left tokens''
              | err -> tokens'', err
          else tok :: tokens', Ok accum
      in

      let tokens', result = nud parser token tokens in
      match result with
        | Ok left -> go left tokens'
        | err -> tokens', err

and nud : parser -> token -> token list -> token list * parse_result
  = fun parser token tokens -> match Map.find parser token.name with
    | Some token_parser -> (match token_parser.nud with
      | Some handler -> handler (expression parser) token tokens
      | None -> tokens, Error (`ParseError (Printf.sprintf
        {|nud: unexpected token %s ("%s" doesn't have a null denotation handler)|}
        token.name token.content)))
    | None -> tokens,
      Error (`ParseError (Printf.sprintf {|nud: unexpected token %s ("%s")|}
      token.name token.content))

and led : parser -> Nominal.term -> token -> token list -> token list * parse_result
  = fun parser lhs token tokens -> match Map.find parser token.name with
    | Some token_parser -> (match token_parser.led with
      | Some handler -> handler (expression parser) lhs token tokens
      | None -> tokens, Error (`ParseError (Printf.sprintf
        {|led: unexpected token %s ("%s" doesn't have a left denotation handler)|}
        token.name token.content)))
    | None -> tokens,
      Error (`ParseError (Printf.sprintf {|led: unexpected token %s ("%s")|}
      token.name token.content))

let parse
  : parser -> token list -> (Nominal.term, [> `ParseError of string ]) Result.t
  = fun parser toks -> match expression parser toks with
    | [], result -> result
    | _, Ok _ -> Error (`ParseError "leftover tokens")
    | _, Error err -> Error err

let lex : lexer -> string -> (token list, [> `LexError of string ]) Result.t
  = fun lexer ->
    let rec go str  = match str with
      | "" -> Ok []
      | _ ->
        let lexer' = List.map lexer ~f:(fun { name; re; lbp } ->
          name, Re.(compile (seq [ bol; re ])), lbp)
        in
        match find_match lexer' str with
        | None -> Error (`LexError (Printf.sprintf
          {|couldn't find a token matching the beginning of this string: "%s"|} str
        ))
        | Some (name, content, lbp) ->
          let str' = String.subo str ~pos:(String.length content) in
          match lbp with
            | None -> go str'
            | Some lbp -> (match go str' with
              | Ok toks -> Ok ({ name; content; lbp } :: toks)
              | Error msg -> Error msg)
    in
    go

let lex_and_parse
  : parser -> lexer -> string -> (Nominal.term, [> `ParseError of string | `LexError of string ]) Result.t
  = fun parser lexer input ->
    let open Result.Let_syntax in
    let%bind tokens = lex lexer input in
    (parse parser tokens
      :  (Nominal.term, [ `ParseError of string ]) Result.t
      :> (Nominal.term, [> `ParseError of string | `LexError of string ]) Result.t)

let%test_module "Pratt" = (module struct

  (*
  let language_desc = {|
  import {integer} from "builtin";

  expr :=
    | add(expr(); expr())
    | sub(expr(); expr())
    | mul(expr(); expr())
    | div(expr(); expr())
    | neg(expr())
    | lit(integer())
  |}
  *)

  let mk_rule = fun name re lbp -> { name; re; lbp }
  let lexer =
    [ mk_rule "+" (Re.char '+') (Some 10)
    ; mk_rule "-" (Re.char '-') (Some 10)
    ; mk_rule "*" (Re.char '*') (Some 20)
    ; mk_rule "/" (Re.char '/') (Some 20)
    ; mk_rule "(" (Re.char '(') (Some 0)
    ; mk_rule ")" (Re.char ')') (Some 0)
    ; mk_rule "lit" (Re.rep1 Re.digit) (Some 0)
    ; mk_rule "space" Re.space None
    ]

  let parser : parser =
    let parse_infix op_name : led_parser option = Some (fun expression lhs token tokens ->
      let tokens', rhs = expression ~rbp:token.lbp tokens in
      tokens', Result.map rhs ~f:(fun rhs ->
        Nominal.Operator (op_name, [Scope ([], lhs); Scope ([], rhs)])))
    in
    Util.String.Map.of_alist_exn
    [ "-", { nud = Some (fun expression _token tokens ->
               let tokens', expr = expression tokens in
               match expr with
                 | Ok expr -> tokens', Ok (Nominal.Operator ("neg", [Scope ([], expr)]))
                 | err -> tokens', err
               )
           ; led = parse_infix "sub"
           }
    ; "lit", { nud = Some (fun _expression token tokens ->
                 tokens, Ok (Operator ("lit", [Scope ([],
                   Primitive (PrimInteger (Bigint.of_string token.content))
                 )])))
             ; led = None
             }
    ; "(", { nud = Some (fun expression _token tokens ->
         let tokens', expr = expression tokens in
         match expr with
           | Ok expr -> (match tokens' with
             | tok :: toks when String.(tok.name = ")") -> toks, Ok expr
             | tok :: _ -> tokens', Error (`ParseError (Printf.sprintf
               {|nud: expected ")", got "%s"|} tok.name))
             | [] -> tokens', Error (`ParseError {|expected ")", got empty token list|})
           )
           | err -> tokens', err)
         ; led = None
         }
    ; "+", { nud = None; led = parse_infix "add" }
    ; "*", { nud = None; led = parse_infix "mul" }
    ; "/", { nud = None; led = parse_infix "div" }
    ]

  let print_parse toks = match lex_and_parse parser lexer toks with
    | Ok tm -> Nominal.pp_term Format.std_formatter tm
    | Error (`ParseError msg) -> Printf.printf "Parse error: %s" msg
    | Error (`LexError msg) -> Printf.printf "Lex error: %s" msg

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
    [%expect {| Parse error: Parsing an expression, ran out of tokens! |}]

  let%expect_test _ =
    print_parse "*";
    [%expect {| Parse error: nud: unexpected token * ("*" doesn't have a null denotation handler) |}]
end);;
