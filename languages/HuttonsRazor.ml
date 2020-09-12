open Base
open Lvca_syntax

module Description = struct
  module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

  (* let abstract_syntax : AbstractSyntax.t = ParseUtil.parse_string ParseAbstract.t {| {
     integer }

     expr := | lit(integer()) // an expression can be a literal integer | add(expr();
     expr()) // or the addition of two expressions

     type := int() // there's only one type in the language |} |> Result.ok_or_failwith ;; *)

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

  let expr_name = "expr"

  (* let dynamics_str = {| let rec dynamics = \(expr : expr()) -> match expr with { |
     add(a; b) -> let a' = dynamics a in let b' = dynamics b in {add(a'; b')} | lit(i) ->
     i } in dynamics |}

     let dynamics : Core.term = ParseUtil.parse_string ParseDynamics.whitespace_term
     dynamics_str |> Result.ok_or_failwith ;; *)
end

(* Write by hand first, later assert the generated parser is equivalent *)
module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let lit : OptRange.t NonBinding.term Parsers.t =
    integer_lit
    >>|| fun ~pos str ->
    let tm =
      NonBinding.(
        Operator
          ( pos
          , "lit"
          , [ [ Primitive (pos, Primitive.PrimInteger (Bigint.of_string str)) ] ] ))
    in
    tm, pos
  ;;

  let t : OptRange.t NonBinding.term Parsers.t =
    fix (fun t ->
        let atom = attach_pos (lit <|> parens t) in
        let plus = char '+' in
        let f (l, rng1) (r, rng2) =
          let rng = OptRange.union rng1 rng2 in
          NonBinding.Operator (rng, "add", [ [ l ]; [ r ] ]), rng
        in
        atom
        >>= fun init -> many (plus *> atom) >>| fun lst -> List.fold lst ~init ~f |> fst)
  ;;

  let whitespace_t = junk *> t
end

let pp =
  (* re prec: ambient precedence level: either 0 or 1. A (+) in an ambient precedence of 1
     needs parens. *)
  let open Caml.Format in
  let with_stag ppf stag f =
    pp_open_stag ppf stag;
    f ();
    pp_close_stag ppf ()
  in
  let rec pp' prec ppf tm =
    match tm with
    | NonBinding.Operator (_, "add", [ [ a ]; [ b ] ]) ->
      with_stag
        ppf
        (String_tag (NonBinding.hash tm))
        (fun () ->
          if prec > 0
          then Fmt.pf ppf "(%a + %a)" (pp' 0) a (pp' 1) b
          else Fmt.pf ppf "%a + %a" (pp' 0) a (pp' 1) b)
    | Operator (_, "lit", [ [ Primitive (_, PrimInteger i) ] ]) ->
      with_stag ppf (String_tag (NonBinding.hash tm)) (fun () -> Bigint.pp ppf i)
    | tm -> Fmt.failwith "Invalid Hutton's Razor term %a" NonBinding.pp tm
  in
  pp' 0
;;

let rec eval_tm : _ NonBinding.term -> (Bigint.t, string) Result.t = function
  | Operator (_, "add", [ [ a ]; [ b ] ]) ->
    (match eval_tm a, eval_tm b with
    | Ok a', Ok b' -> Ok Bigint.(a' + b')
    | Error msg, _ | _, Error msg -> Error msg)
  | Operator (_, "lit", [ [ Primitive (_, PrimInteger i) ] ]) -> Ok i
  | tm -> Error ("found un-evaluable term: " ^ NonBinding.to_string tm)
;;

let eval_str : string -> (Bigint.t, string) Result.t =
  let module Parse = Parse (ParseUtil.NoComment) in
  fun str ->
    match ParseUtil.parse_string Parse.whitespace_t str with
    | Error str -> Error str
    | Ok tm -> eval_tm tm
;;

(* let eval_2 : string -> (Bigint.t, string) Result.t = let module Parse =
   AngstromParse(ParseUtil.NoComment) in fun str -> match ParseUtil.parse_string
   Parse.whitespace_t str with | Error str -> Error str | Ok tm -> begin match Core.(eval
   (CoreApp (Description.dynamics, Term (NonBinding.to_nominal tm)))) with | Error (msg,
   tm) -> Error (msg ^ ": " ^ Core.to_string tm) | Ok (Primitive (PrimInteger i)) -> Ok i
   | _ -> Error "unexpected non-integer result" end *)

let ident_stag_funs =
  Caml.Format.
    { mark_open_stag =
        (function
        | Range.Stag rng -> Printf.sprintf "<%s>" (Range.to_string rng)
        | String_tag str -> Printf.sprintf "<%s>" (String.subo str ~len:6)
        | _ -> "")
    ; mark_close_stag =
        (function
        | Range.Stag rng -> Printf.sprintf "</%s>" (Range.to_string rng)
        | String_tag str -> Printf.sprintf "</%s>" (String.subo str ~len:6)
        | _ -> "")
    ; print_open_stag = (fun _ -> ())
    ; print_close_stag = (fun _ -> ())
    }
;;

let%test_module "Hutton's Razor" =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let parse str = ParseUtil.parse_string Parse.whitespace_t str

    let () =
      let open Caml.Format in
      pp_set_margin std_formatter 200;
      set_formatter_stag_functions ident_stag_funs;
      set_tags true;
      set_mark_tags true
    ;;

    let print_representations str =
      match parse str with
      | Error str -> Caml.print_string str
      | Ok tm ->
        Fmt.pr "%a\n" NonBinding.pp tm;
        Fmt.pr "%a\n" NonBinding.pp_range tm;
        Fmt.pr "%a" pp tm
    ;;

    let%expect_test _ =
      print_representations "1";
      [%expect
        {|
      lit(1)
      <0-1>lit(<0-1>1</0-1>)</0-1>
      <66d708>1</66d708>
    |}]
    ;;

    let%expect_test _ =
      print_representations "1 + 2";
      (*012345*)
      [%expect
        {|
      add(lit(1); lit(2))
      <0-5>add(<0-1>lit(<0-1>1</0-1>)</0-1>; <4-5>lit(<4-5>2</4-5>)</4-5>)</0-5>
      <564912><66d708>1</66d708> + <aaa76b>2</aaa76b></564912>
    |}]
    ;;

    let%expect_test _ =
      print_representations "1 + 2 + 3";
      (*0123456789*)
      [%expect
        {|
      add(add(lit(1); lit(2)); lit(3))
      <0-9>add(<0-5>add(<0-1>lit(<0-1>1</0-1>)</0-1>; <4-5>lit(<4-5>2</4-5>)</4-5>)</0-5>; <8-9>lit(<8-9>3</8-9>)</8-9>)</0-9>
      <4196bd><564912><66d708>1</66d708> + <aaa76b>2</aaa76b></564912> + <9c6d16>3</9c6d16></4196bd>
    |}]
    ;;

    let%expect_test _ =
      print_representations "1 + (2 + 3)";
      (*012345678901*)
      [%expect
        {|
      add(lit(1); add(lit(2); lit(3)))
      <0-11>add(<0-1>lit(<0-1>1</0-1>)</0-1>; <5-10>add(<5-6>lit(<5-6>2</5-6>)</5-6>; <9-10>lit(<9-10>3</9-10>)</9-10>)</5-10>)</0-11>
      <7a73df><66d708>1</66d708> + <1e2d31>(<aaa76b>2</aaa76b> + <9c6d16>3</9c6d16>)</1e2d31></7a73df>
    |}]
    ;;

    let print_eval : string -> unit =
     fun str ->
      let msg = match eval_str str with Error msg -> msg | Ok i -> Bigint.to_string i in
      Caml.print_string msg
   ;;

    let%expect_test _ =
      print_eval "1";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      print_eval "1 + 2";
      [%expect {| 3 |}]
    ;;

    let%expect_test _ =
      print_eval "1 + 2 + 3";
      [%expect {| 6 |}]
    ;;

    let%expect_test _ =
      print_eval "1 + (2 + 3)";
      [%expect {| 6 |}]
    ;;
  end)
;;
