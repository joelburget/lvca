open Base
open Lvca_syntax

module Description = struct
  module ParseAbstract = AbstractSyntax.Parse(ParseUtil.Angstrom.CComment)
  (* module ParseDynamics = Core.Parse(ParseUtil.Angstrom.CComment) *)

  let abstract_syntax : AbstractSyntax.t =
    Angstrom.parse_string ~consume:All ParseAbstract.t
    {|import {integer} from "lvca/builtin"

  expr :=
    | lit(integer())    // an expression can be a literal integer
    | add(expr(); expr()) // or the addition of two expressions

  type := int() // there's only one type in the language
    |}
    |> Result.ok_or_failwith
  ;;

  let parser_str =
    {|
  fix (parser ->
    let space = (' ' | '\n' | '\t')* in
    let lit = satisfy (c -> is_digit c) in
    let atom =
      | lit space -> { lit }
      | '(' space parser space ')' space -> { parser })
    in
    l=atom r=('+' space parser)? -> {
      match r with
        | some(r') -> add(l; r')
        | none() -> l
    }
  )
    |}
  ;;

  let statics =
    {|
  ----------------------
  ctx >> lit(_) => int()

  -------------------------
  ctx >> add(_; _) => int()
    |}
  ;;

  let expr_name = "expr";;

  (*
  let dynamics_str = {|
  let rec dynamics = \(expr : expr()) -> match expr with {
    | add(a; b) ->
        let a' = dynamics a in
        let b' = dynamics b in
        {add(a'; b')}
    | lit(i) -> i
  }
  in dynamics
  |}

  let dynamics : Core.term
    = Angstrom.parse_string ~consume:All
      Angstrom.(ParseUtil.Angstrom.whitespace *> ParseDynamics.term) dynamics_str
    |> Result.ok_or_failwith
  ;;
  *)
end

(* Write by hand first, later assert the generated parser is equivalent *)
module AngstromParse(Comment : ParseUtil.Angstrom.Comment_int) = struct
  module Parsers = ParseUtil.Angstrom.Mk(Comment)
  let chainl1, whitespace = ParseUtil.Angstrom.(chainl1, whitespace)
  let char, integer_lit, parens = Parsers.(char, integer_lit, parens)
  let choice, fix, lift3, pos, return, (>>|), (<|>), ( *> ) =
    Angstrom.(choice, fix, lift3, pos, return, (>>|), (<|>), ( *> ))

  let lit : Range.t NonBinding.term Angstrom.t
    = lift3
        (fun p1 str p2 ->
          let range = Range.mk p1 p2 in
          NonBinding.(Operator
            ( range
            , "lit"
            , [ [ Primitive (range, Primitive.PrimInteger (Bigint.of_string str)) ] ]
            )))
      pos
      integer_lit
      pos

  let location = NonBinding.location

  let t : Range.t NonBinding.term Angstrom.t
    = fix (fun t ->
      let atom = lit <|> parens t in
      let add = char '+' *> return
        (fun x y -> NonBinding.Operator
          ( Range.(location x <> location y)
          , "add"
          , [[x]; [y]]
          )) in
      chainl1 atom add)

  let whitespace_t = whitespace *> t
end
;;

let pp =
  (* re prec: ambient precedence level: either 0 or 1. A (+) in an ambient precedence of 1
     needs parens.
  *)
  let rec pp' prec ppf tm = match tm with

    | NonBinding.Operator (_, "add", [[a];[b]]) ->
      Caml.Format.pp_open_stag ppf (Caml.Format.String_tag (NonBinding.hash tm));
      begin
        if prec > 0
        then Fmt.pf ppf "(%a + %a)" (pp' 0) a (pp' 1) b
        else Fmt.pf ppf "%a + %a" (pp' 0) a (pp' 1) b
      end;
      Caml.Format.pp_close_stag ppf ()
    | Operator (_, "lit", [[Primitive (_, PrimInteger i)]]) ->
      Caml.Format.pp_open_stag ppf (Caml.Format.String_tag (NonBinding.hash tm));
      Bigint.pp ppf i;
      Caml.Format.pp_close_stag ppf ()
    | tm ->
      Fmt.failwith "Invalid Hutton's Razor term %a" NonBinding.pp tm

  in pp' 0

let rec eval_tm : _ NonBinding.term -> (Bigint.t, string) Result.t
  = function
    | Operator (_, "add", [[a];[b]]) ->
      begin
        match eval_tm a, eval_tm b with
          | Ok a', Ok b' -> Ok Bigint.(a' + b')
          | Error msg, _ | _, Error msg -> Error msg
      end
    | Operator (_, "lit", [[Primitive (_, PrimInteger i)]]) -> Ok i
    | tm -> Error ("found un-evaluable term: " ^ NonBinding.to_string tm)


let eval_str : string -> (Bigint.t, string) Result.t
  = let module Parse = AngstromParse(ParseUtil.Angstrom.NoComment) in
    fun str ->
    match Angstrom.parse_string ~consume:All Parse.whitespace_t str with
      | Error str -> Error str
      | Ok tm -> eval_tm tm

      (*
let eval_2 : string -> (Bigint.t, string) Result.t
  = let module Parse = AngstromParse(ParseUtil.Angstrom.NoComment) in
    fun str ->
    match Angstrom.parse_string ~consume:All Parse.whitespace_t str with
      | Error str -> Error str
      | Ok tm ->
        begin
          match Core.(eval (CoreApp (Description.dynamics, Term (NonBinding.to_nominal tm)))) with
            | Error (msg, tm) -> Error (msg ^ ": " ^ Core.to_string tm)
            | Ok (Primitive (PrimInteger i)) -> Ok i
            | _ -> Error "unexpected non-integer result"
        end
*)

let range_stag_funs = Caml.Format.
  { mark_open_stag =
    begin
      function
        | String_tag str -> Printf.sprintf "<%s>" (String.subo str ~len:6)
        | _ -> ""
    end
  ; mark_close_stag =
    begin
      function
        | String_tag str -> Printf.sprintf "</%s>" (String.subo str ~len:6)
        | _ -> ""
    end
  ; print_open_stag = (fun _ -> ())
  ; print_close_stag = (fun _ -> ())
  }

let pp_range ppf fmt =
  let pp_set_formatter_stag_functions, pp_get_mark_tags, pp_set_mark_tags, kfprintf =
    Caml.Format.(pp_set_formatter_stag_functions, pp_get_mark_tags, pp_set_mark_tags,
      kfprintf)
  in
  pp_set_formatter_stag_functions ppf range_stag_funs;
  let mark_tags = pp_get_mark_tags ppf () in
  pp_set_mark_tags ppf true;
  kfprintf (fun ppf -> pp_set_mark_tags ppf mark_tags)
    ppf fmt ;;

let%test_module "Hutton's Razor" = (module struct
  module Parse = AngstromParse(ParseUtil.Angstrom.NoComment)
  let parse str = Angstrom.parse_string ~consume:All Parse.whitespace_t str
  let print_representations str = match parse str with
    | Error str -> Caml.print_string str
    | Ok tm ->
      Fmt.pr "%a\n" NonBinding.pp tm;
      Fmt.pr "%a\n" NonBinding.pp_range tm;
      Fmt.pr "%a\n" pp tm;
      pp_range Caml.Format.std_formatter "%a" pp tm

  let%expect_test _ = print_representations "1";
    [%expect{|
      lit(1)
      lit{0,1}(1{0,1})
      1
      <66d708>1</66d708>
    |}]

  let%expect_test _ = print_representations "1 + 2";
    [%expect{|
      add(lit(1); lit(2))
      add{0,5}(lit{0,2}(1{0,2}); lit{4,5}(2{4,5}))
      1 + 2
      <564912><66d708>1</66d708> + <aaa76b>2</aaa76b></564912>
    |}]

  let%expect_test _ =
    print_representations "1 + 2 + 3";
    [%expect{|
      add(add(lit(1); lit(2)); lit(3))
      1 + 2 + 3
      <4196bd><564912><66d708>1</66d708> + <aaa76b>2</aaa76b></564912> + <9c6d16>3</9c6d16></4196bd>
    |}]

  let%expect_test _ =
    print_representations "1 + (2 + 3)";
    [%expect{|
      add(lit(1); add(lit(2); lit(3)))
      1 + (2 + 3)
      <7a73df><66d708>1</66d708> + <1e2d31>(<aaa76b>2</aaa76b> + <9c6d16>3</9c6d16>)</1e2d31></7a73df>
    |}]

  let print_eval : string -> unit
    = fun str -> Caml.print_string (match eval_str str with
      | Error msg -> msg
      | Ok i -> Bigint.to_string i)

  let%expect_test _ = print_eval "1"; [%expect{| 1 |}]
  let%expect_test _ = print_eval "1 + 2"; [%expect{| 3 |}]
  let%expect_test _ = print_eval "1 + 2 + 3"; [%expect{| 6 |}]
  let%expect_test _ = print_eval "1 + (2 + 3)"; [%expect{| 6 |}]
end)
;;
