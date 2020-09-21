open Base
open Lvca_syntax
open Lvca_core
module Format = Caml.Format

let abstract_syntax_str =
  {|
// global: { char, string, n_term, c_term }

parser :=
  // primitive parsers
  | char(char())
  | string(string())
  | satisfy(char(). c_term())
  | fail(c_term())

  | let(parser(); parser(). parser())

  // combinators
  | option(parser())
  | count(parser(); c_term())
  | many(parser())
  | many1(parser())
  | fix(parser(). parser())

  // alternative
  | alt(parser(); parser())

  // monad
  | return(c_term())

  | sequence(n_term()*. c_term(); parser()*)
|}
;;

module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

let abstract_syntax : AbstractSyntax.t =
  abstract_syntax_str
  |> ParseUtil.parse_string ParseAbstract.whitespace_t
  |> Result.ok_or_failwith
;;

type c_term = OptRange.t Core.term
type n_term = (OptRange.t, Primitive.t) Nominal.term

type t =
  (* primitive parsers *)
  | Char of char
  | String of string
  | Satisfy of string * c_term
  | Fail of c_term
  | Let of string * t * t
  (* combinators *)
  | Option of t
  | Count of t * c_term
  | Many of t
  | Many1 of t
  | Fix of string * t
  (* alternative *)
  | Alt of t * t
  | Return of c_term
  | Sequence of string list * c_term * t list
  | Identifier of string

let rec pp (* Format.formatter -> t -> unit *) : t Fmt.t =
 fun ppf ->
  let core = Core.pp in
  let pf = Fmt.pf in
  function
  | Char char -> pf ppf "'%c'" char
  | String str -> pf ppf {|"%s"|} str
  | Satisfy (name, tm) -> pf ppf "satisfy (%s -> %a)" name core tm
  | Let (name, named, body) -> pf ppf "let %s = %a in %a" name pp named pp body
  | Fail tm -> pf ppf "fail %a" core tm
  | Option t -> pf ppf "%a?" pp t
  | Count (p, t) -> pf ppf "%a{%a}" pp p core t
  | Many t -> pf ppf "%a*" pp t
  | Many1 t -> pf ppf "%a+" pp t
  | Fix (name, t) -> pf ppf "fix (%s -> %a)" name pp t
  | Alt (t1, t2) -> pf ppf "alt %a %a" pp t1 pp t2
  | Return tm -> pf ppf "return %a" core tm
  | Sequence (names, p, ps) ->
    pf
      ppf
      (* TODO: consistent style between sequence, fix, and satisfy *)
      "sequence (%a. %a) [%a]"
      Fmt.(list ~sep:(any ".@ ") string)
      names
      core
      p
      Fmt.(list ~sep:comma pp)
      ps
  | Identifier name -> pf ppf "%s" name
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  type term = t

  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let keywords : string list = [ "satisfy"; "let"; "in"; "fail"; "sequence"; "fix" ]
  let keyword : string Parsers.t = keywords |> List.map ~f:string |> choice
  let operators : string list = [ "?"; "*"; "+"; "|"; "=" ]
  let operator : string Parsers.t = operators |> List.map ~f:string |> choice

  let t : c_term Parsers.t -> term Parsers.t =
   fun term ->
    fix (fun parser ->
        let parse_token =
          choice
            [ (fun c -> Char c) <$> char_lit
            ; (fun s -> String s) <$> string_lit
            ; parens parser
            ; (fun name -> Identifier name) <$> Parsers.identifier
            ]
        in
        choice
          [ (string "let"
            *> lift3
                 (fun name bound body -> Let (name, bound, body))
                 Parsers.identifier
                 (string "=" *> parser)
                 (string "in" *> parser)) <?> "let"
          ; (string "satisfy"
            *> parens
                 (lift3
                    (fun name _arr tm -> Satisfy (name, tm))
                    Parsers.identifier
                    (string "->")
                    term) <?> "satisfy")
          ; (string "fail" *> term >>| fun tm -> Fail tm) <?> "fail"
          ; (string "fix"
            *> parens
                 (lift3
                    (fun name _arr tm -> Fix (name, tm))
                    Parsers.identifier
                    (string "->")
                    parser)) <?> "fix"
          ; (string "sequence"
            *> lift2
            (fun (names, body) components -> Sequence (names, body, components))
            (parens (lift2
              (fun names body -> names, body)
              (many1 (Parsers.identifier <* char '.'))
              term))
            (brackets (sep_by1 (char ',') parser)) <?> "sequence")
          ; (parse_token
            >>= fun tok ->
            option
              tok
              (choice
                 [ (char '?' >>| fun _ -> Option tok) <?> "?"
                 ; (char '*' >>| fun _ -> Many tok) <?> "*"
                 ; (char '+' >>| fun _ -> Many1 tok) <?> "+"
                 ; char '|' *> (parser >>| fun rhs -> Alt (tok, rhs)) <?> "|"
                 ]))
          ])
    <?> "parser"
 ;;
end

let mk_list : pos:OptRange.t -> n_term list -> n_term * OptRange.t =
 fun ~pos lst ->
  let tm = Nominal.Operator (pos, "list", [ Nominal.Scope ([], lst) ]) in
  tm, pos
;;

let mk_some : OptRange.t -> n_term -> n_term =
 fun pos tm -> Nominal.Operator (pos, "some", [ Scope ([], [ tm ]) ])
;;

type ctx_entry =
  | BoundChar of char
  | BoundParser of (OptRange.t, Primitive.t) Nominal.term ParseUtil.t

(* TODO: this is hacky *)
let thin_term_only_ctx
  : ctx_entry Lvca_util.String.Map.t -> n_term Lvca_util.String.Map.t
  = Map.filter_map ~f:(function
      | BoundChar c -> Some (Nominal.Primitive (None, Primitive.PrimChar c))
      | BoundParser _ -> None)
;;

(* Translate our parser type into an angstrom parser *)
let rec translate : t -> n_term ParseUtil.t = fun tm ->
  let module Parsers = ParseUtil.Mk (ParseUtil.CComment) in
  let open Parsers in
  let mk_err () = failwith "TODO: error" in
  let rec translate' ctx = function
    | Char c ->
      char c
      >>|| (fun ~pos c -> Nominal.Primitive (pos, Primitive.PrimChar c), pos)
      <?> Printf.sprintf {|char '%s'|} (String.make 1 c)
    | String str ->
      string str
      >>|| (fun ~pos str -> Nominal.Primitive (pos, Primitive.PrimString str), pos)
      <?> Printf.sprintf {|string "%s"|} str
    | Satisfy (name, tm) ->
      let f c =
        let ctx' = Map.set ctx ~key:name ~data:(BoundChar c) in
        match Core.eval_ctx_exn (thin_term_only_ctx ctx') tm with
        | Operator (_, "true", []) -> true
        | _ -> false
      in
      satisfy f
      >>|| (fun ~pos c -> Nominal.Primitive (pos, Primitive.PrimChar c), pos)
      <?> Printf.sprintf {|satisfy(\%s. ...)|} name
    | Let (name, named, body) ->
      let ctx' = Map.set ctx ~key:name ~data:(BoundParser (translate named)) in
      translate' ctx' body <?> name
    | Fail tm ->
      (match Core.eval_ctx_exn (thin_term_only_ctx ctx) tm with
      | Primitive (_, PrimString msg) -> fail msg
      | _ -> mk_err ())
    | Option p ->
      option None ((fun tm -> Some tm) <$> translate' ctx p)
      >>|| (fun ~pos opt_tm ->
             let result =
               match opt_tm with
               | Some tm -> mk_some pos tm
               | None -> Operator (pos, "none", [])
             in
             result, pos)
      <?> "option"
    | Count (p, n_tm) ->
      let n =
        match Core.eval_ctx_exn (thin_term_only_ctx ctx) n_tm with
        | Primitive (_, PrimInteger i) ->
          (match Bigint.to_int i with Some n -> n | None -> mk_err ())
        | _ -> mk_err ()
      in
      count n (translate' ctx p) >>|| mk_list <?> "count"
    | Many t -> many (translate' ctx t) >>|| mk_list <?> "many"
    | Many1 t -> many1 (translate' ctx t) >>|| mk_list <?> "many1"
    (* TODO: do we even want explicit fix? or should this be done implicitly? *)
    | Fix (name, p)
    -> fix (fun p' -> translate' (Map.set ctx ~key:name ~data:(BoundParser p')) p)
    | Alt (p1, p2) -> translate' ctx p1 <|> translate' ctx p2 <?> "alt"
    | Return tm -> (match tm with Term tm -> return tm | _ -> mk_err ())
    | Sequence (names, p, ps) -> (match List.zip names ps with
      | Unequal_lengths -> mk_err ()
      | Ok named_parsers ->
        let rec go ctx = function
          | [] -> return @@ Core.eval_ctx_exn (thin_term_only_ctx ctx) p
          | (key, parser) :: named_parsers'
          -> translate parser >>= fun data ->
             (* XXX data is wrong *)
             go (Map.set ctx ~key ~data:(BoundParser (return data))) named_parsers'
        in
        go ctx named_parsers
    )
    | Identifier name ->
      (match Map.find ctx name with
      | Some (BoundParser p) -> p <?> name
      | None | Some (BoundChar _) -> mk_err ())
  in
  translate' Lvca_util.String.Map.empty tm
;;

let parse : t -> string -> (n_term, string) Result.t =
 fun parser -> ParseUtil.parse_string (translate parser)
;;

let%test_module "Parsing" =
  (module struct
    module ParseCore = Core.Parse (ParseUtil.CComment)
    module ParseParser = Parse (ParseUtil.CComment)

    let () =
      Format.set_formatter_stag_functions Range.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let parse_print : string -> string -> unit =
     fun parser_str str ->
      match ParseUtil.parse_string (ParseParser.t ParseCore.term) parser_str with
      | Error msg -> Caml.print_string ("failed to parse parser desc: " ^ msg)
      | Ok parser ->
        (match parse parser str with
        | Error msg -> Caml.print_string ("failed to parse: " ^ msg)
        | Ok tm -> Fmt.pr "%a\n" (Nominal.pp_term_range Primitive.pp) tm)
   ;;

    let%expect_test _ =
      parse_print {|"str"|} "str";
      [%expect {| <0-3>"str"</0-3> |}]
    ;;

    let%expect_test _ =
      parse_print {|"str"|} "foo";
      [%expect {| failed to parse: string "str": string |}]
    ;;

    let%expect_test _ =
      parse_print {|"str"*|} "strstrstr";
      [%expect
        {| <0-9>list(<0-3>"str"</0-3>, <3-6>"str"</3-6>, <6-9>"str"</6-9>)</0-9> |}]
    ;;

    let%expect_test _ =
      parse_print {|"str"+|} "strstrstr";
      [%expect
        {| <0-9>list(<0-3>"str"</0-3>, <3-6>"str"</3-6>, <6-9>"str"</6-9>)</0-9> |}]
    ;;

    let%expect_test _ =
      parse_print {|"str" | "foo"|} "str";
      [%expect {| <0-3>"str"</0-3> |}]
    ;;

    let%expect_test _ =
      parse_print {|"str" | "foo"|} "foo";
      [%expect {| <0-3>"foo"</0-3> |}]
    ;;

    let sat_parser =
      {|satisfy (x -> match x with {
      | 'c' -> {true()}
      | _ -> {false()}
    })
    |}
    ;;

    let%expect_test _ =
      parse_print sat_parser "c";
      [%expect {| <0-1>'c'</0-1> |}]
    ;;

    let%expect_test _ =
      parse_print sat_parser "d";
      [%expect {| failed to parse: satisfy(\x. ...): satisfy: 'd' |}]
    ;;

    let%expect_test _ =
      parse_print {|let x = "str" in x|} "str";
      [%expect {| <0-3>"str"</0-3> |}]
    ;;

    let%expect_test _ =
      parse_print {|fail {"reason"}|} "str";
      (* TODO: nicer formatting *)
      [%expect {| failed to parse: : reason |}]
    ;;

    let%expect_test _ =
      parse_print {|fix (x -> "a" | "b")|} "a";
      [%expect{| <0-1>"a"</0-1> |}]

    let%expect_test _ =
      parse_print {|sequence(a. {"a"})["a"]|} "a";
      [%expect]

    let%expect_test _ =
      parse_print {|sequence(a. a'. b. {triple(a; a'; b)})["a", "a", "b"]|} "aab";
      [%expect]

    let%expect_test _ =
      parse_print {|sequence(a. a'. b. {triple(a; a'; b)})['a', 'a', 'b']|} "aab";
      [%expect]

    let%expect_test _ =
      parse_print {|fix (x -> sequence(a. x. {pair(a; x)}) ["a", x] | "b")|} "a";
      [%expect]
  end)
;;
