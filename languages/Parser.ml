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

  // is sequence a better name?
  | lift_n(n_term()*. c_term(); parser()*)
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
  | LiftN of string list * c_term * t list
  | Identifier of string

let mk_some : OptRange.t -> n_term -> n_term =
 fun pos tm -> Nominal.Operator (pos, "some", [ Scope ([], [ tm ]) ])
;;

let none pos = Nominal.Operator (pos, "none", [])

let mk_list : pos:OptRange.t -> n_term list -> n_term * OptRange.t =
 fun ~pos lst ->
  let tm = Nominal.Operator (pos, "list", [ Nominal.Scope ([], lst) ]) in
  tm, pos
;;

(*
type ctx_entry =
  | BoundParser of t
  | BoundTerm of n_term

(* TODO: this is hacky *)
let thin_ctx : ctx_entry Lvca_util.String.Map.t -> n_term Lvca_util.String.Map.t =
  Map.filter_map ~f:(function
      | BoundParser _ -> None
      | BoundTerm tm -> Some tm)
;;
*)

type term_ctx = n_term Lvca_util.String.Map.t
type parser_ctx = t Lvca_util.String.Map.t

module Direct = struct
  type direct = { run
    :  term_ctx:term_ctx
    -> parser_ctx:parser_ctx
    -> string
    -> string * (n_term, (string * c_term)) Result.t
  }

  let todo_range = None
  let todo_msg msg = Error
    ( msg
    , Core.Term (Nominal.Primitive (todo_range, Primitive.PrimString msg))
    )

  let mk_char range c = Ok (Nominal.Primitive (range, Primitive.PrimChar c))

  let char c = { run = fun ~term_ctx:_ ~parser_ctx:_ str ->
      if String.length str > 0 && Char.(String.get str 0 = c)
      then String.subo ~pos:1 str, mk_char todo_range c
      else str, todo_msg (Printf.sprintf "char '%c'" c)
    }

  let string prefix = { run = fun ~term_ctx:_ ~parser_ctx:_ str ->
    match String.chop_prefix str ~prefix with
      | None -> str, todo_msg (Printf.sprintf {|string "%s"|} prefix)
      | Some str' -> str', Ok (Nominal.Primitive (todo_range, PrimString prefix))
    }

  let satisfy name core_term = { run = fun ~term_ctx ~parser_ctx:_ str ->
    let err_msg = todo_msg
      (Printf.sprintf {|satisfy (%s -> %s)|} name (Core.to_string core_term))
    in
    if String.length str = 0
    then str, err_msg
    else
      let c = String.get str 0 in
      let tm = Core.(Let
        ( NoRec
        , Term (Primitive (todo_range, PrimChar c))
        , Scope (name, core_term))
        )
      in
      match Core.eval_ctx term_ctx tm with
        | Ok (Operator (_, "true", [])) -> String.subo ~pos:1 str, mk_char todo_range c
        | Ok (Operator (_, "false", []))
        | Ok _ -> str, err_msg (* TODO: throw harder error? (type error) *)
        | Error ((msg, _tm) as err) -> msg, Error err
  }

  let fail c_tm =
    { run = fun ~term_ctx ~parser_ctx:_ str -> match Core.eval_ctx term_ctx c_tm with
      | Ok (Primitive (_ (* TODO: use pos *), PrimString msg)) -> str, todo_msg msg
      | _ -> failwith "TODO"
    }

  let let_ name p body =
    { run = fun ~term_ctx ~parser_ctx str ->
      let parser_ctx = Map.set parser_ctx ~key:name ~data:p in
      body.run ~term_ctx ~parser_ctx str
    }

  let option t =
    { run = fun ~term_ctx ~parser_ctx str -> match t.run ~term_ctx ~parser_ctx str with
      | str', Ok tm -> str', Ok (mk_some todo_range tm)
      | str', Error _ -> str', Ok (none todo_range)
    }

  let mk_list_result = Result.map ~f:(fun tms -> fst @@ mk_list ~pos:todo_range tms)

  let map_snd ~f (a, b) = a, f b

  let count n_tm t =
    let rec go ~term_ctx ~parser_ctx n str = match n with
      | 0 -> str, Ok []
      | _ ->
        let str, head_result = t.run ~term_ctx ~parser_ctx str in
        match head_result with
          | Error msg -> str, Error msg
          | Ok tm ->
            let str, tail_result = go ~term_ctx ~parser_ctx (n - 1) str in
            str, tail_result |> Result.map ~f:(List.cons tm)
    in
    { run = fun ~term_ctx ~parser_ctx str ->
      match Core.eval_ctx term_ctx n_tm with
        | Ok (Primitive (_, PrimInteger n)) -> str
          |> go ~term_ctx ~parser_ctx (n |> Bigint.to_int |> Option.value_exn (* XXX *))
          |> map_snd ~f:mk_list_result
        | _ -> failwith "TODO"
    }

  let rec go_many ~term_ctx ~parser_ctx t str =
    let str, head_result = t.run ~term_ctx ~parser_ctx str in
    match head_result with
      | Error _ -> str, Ok []
      | Ok tm -> str
        |> go_many ~term_ctx ~parser_ctx t
        |> map_snd ~f:(Result.map ~f:(List.cons tm))

  let many t =
    { run = fun ~term_ctx ~parser_ctx str -> str
      |> go_many ~term_ctx ~parser_ctx t
      |> map_snd ~f:mk_list_result
    }

  let many1 t =
    { run = fun ~term_ctx ~parser_ctx str -> str
      |> go_many ~term_ctx ~parser_ctx t
      |> map_snd ~f:(function
        | Ok [] -> todo_msg "many1: empty list"
        | result -> mk_list_result result
      )
    }

  let alt t1 t2 =
    { run = fun ~term_ctx ~parser_ctx str ->
      let str, result = t1.run ~term_ctx ~parser_ctx str in
      match result with
        | Error _ -> t2.run ~term_ctx ~parser_ctx str
        | _ -> str, result
    }

  let return tm = { run = fun ~term_ctx ~parser_ctx:_ str -> str, Core.eval_ctx term_ctx tm }

  let liftn names tm ps =
    { run = fun ~term_ctx ~parser_ctx str ->
      let str, result = ps
        (* TODO: scanl? *)
        |> List.fold
          ~init:(str, Ok [])
          ~f:(fun (str, xs) { run } -> match xs, run ~term_ctx ~parser_ctx str with
              | Ok xs, (str, Ok x) -> str, Ok (x :: xs)
              | Error msg, _
              | _, (_, Error msg) -> str, Error msg)
      in
      match result with
        | Error msg -> str, Error msg
        | Ok xs -> match List.zip names xs with
          | Unequal_lengths -> failwith "TODO"
          | Ok name_vals ->
            let term_ctx = name_vals
              |> List.fold
                ~init:term_ctx
                ~f:(fun ctx (key, tm) -> Map.set ctx ~key ~data:tm)
            in
            str, Core.eval_ctx term_ctx tm
    }

  let fix (_key, p) =
    let f = fun _p' -> p
      (* TODO *)
      (* p.run ~term_ctx ~parser_ctx:(Map.set parser_ctx ~key ~data:p') *)
    in
    let rec p = lazy (f r)
    and r = { run = fun ~term_ctx ~parser_ctx str -> (Lazy.force p).run ~term_ctx
    ~parser_ctx str }
    in r

  let rec translate_direct : t -> direct
    = function
      | Char c -> char c
      | String prefix -> string prefix
      | Satisfy (name, core_term) -> satisfy name core_term
      | Fail tm -> fail tm
      | Let (name, p, body) -> let_ name p (translate_direct body)
      | Option t -> option (translate_direct t)
      | Count (t, n) -> count n (translate_direct t)
      | Many t -> many (translate_direct t)
      | Many1 t -> many1 (translate_direct t)
      | Fix _ -> failwith "TODO: fix"
      | Alt (t1, t2) -> alt (translate_direct t1) (translate_direct t2)
      | Return tm -> return tm
      | LiftN (names, tm, ps) -> liftn names tm (List.map ps ~f:translate_direct)
      | Identifier name -> identifier name

  (* TODO: BoundParser should be already translated? *)
  and identifier name = { run = fun ~term_ctx ~parser_ctx str -> match Map.find parser_ctx name with
    | None -> str, todo_msg (Printf.sprintf {|Identifer not found in context: "%s"|} name)
    | Some p -> (translate_direct p).run ~term_ctx ~parser_ctx str
  }

  let parse_direct : direct -> string -> (n_term, string * c_term) Result.t
    = fun { run } str -> match
        run ~term_ctx:Lvca_util.String.Map.empty
            ~parser_ctx:Lvca_util.String.Map.empty
            str
      with
      | "", result -> result
      | str, Ok _ -> todo_msg (Printf.sprintf
        {|Parser didn't consume entire input. Left over: "%s"|}
        (if String.length str > 50
         then String.prefix str 47 ^ "..."
         else str)
      )
      | _, Error err -> Error err

  type t = direct
end

let rec pp (* Format.formatter -> t -> unit *) : t Fmt.t =
 fun ppf ->
  let core = Core.pp in
  let pf = Fmt.pf in
  function
  | Char char -> pf ppf "'%c'" char
  | String str -> pf ppf {|"%s"|} str
  | Satisfy (name, tm) -> pf ppf "satisfy (\\%s. %a)" name core tm
  | Let (name, named, body) -> pf ppf "let %s = %a in %a" name pp named pp body
  | Fail tm -> pf ppf "fail %a" core tm
  | Option t -> pf ppf "%a?" pp t
  | Count (p, t) -> pf ppf "%a{%a}" pp p core t
  | Many t -> pf ppf "%a*" pp t
  | Many1 t -> pf ppf "%a+" pp t
  | Fix (name, t) -> pf ppf "fix (%s -> %a)" name pp t
  | Alt (t1, t2) -> pf ppf "alt %a %a" pp t1 pp t2
  | Return tm -> pf ppf "return %a" core tm
  | LiftN (names, p, ps) ->
    pf
      ppf
      "lift (\\%a. %a) [%a]"
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

  let keywords : string list = [ "satisfy"; "let"; "in"; "fail" ]
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
          [ string "let"
            *> lift3
                 (fun name bound body -> Let (name, bound, body))
                 Parsers.identifier
                 (string "=" *> parser)
                 (string "in" *> parser)
          ; string "satisfy"
            *> parens
                 (lift3
                    (fun name _arr tm -> Satisfy (name, tm))
                    Parsers.identifier
                    (string "->")
                    term)
          ; (string "fail" *> term >>| fun tm -> Fail tm)
          ; string "fix"
            *> parens
                 (lift3
                    (fun name _arr tm -> Fix (name, tm))
                    Parsers.identifier
                    (string "->")
                    parser)
            (* TODO: sequence *)
            (* ; string "sequence" *)
          ; (parse_token
            >>= fun tok ->
            option
              tok
              (choice
                 [ (char '?' >>| fun _ -> Option tok)
                 ; (char '*' >>| fun _ -> Many tok)
                 ; (char '+' >>| fun _ -> Many1 tok)
                 ; char '|' *> (parser >>| fun rhs -> Alt (tok, rhs))
                 ]))
          ])
    <?> "parser"
 ;;
end

(* Translate our parser type into an angstrom parser *)
let translate : t -> n_term ParseUtil.t =
  let module Parsers = ParseUtil.Mk (ParseUtil.CComment) in
  let open Parsers in
  let mk_err () = failwith "TODO: error" in
  let rec translate' term_ctx parser_ctx = function
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
        let c_tm = Nominal.Primitive (None (* TODO: pos *), Primitive.PrimChar c) in
        let term_ctx = Map.set term_ctx ~key:name ~data:c_tm in
        match Core.eval_ctx term_ctx tm with
        | Ok (Operator (_, "true", [])) -> true
        | _ -> false
      in
      satisfy f
      >>|| (fun ~pos c -> Nominal.Primitive (pos, Primitive.PrimChar c), pos)
      <?> Printf.sprintf {|satisfy(\%s. ...)|} name
    | Let (name, named, body) ->
      let parser_ctx = Map.set parser_ctx ~key:name ~data:named in
      translate' term_ctx parser_ctx body <?> name
    | Fail tm ->
      (match Core.eval_ctx term_ctx tm with
      | Ok (Primitive (_, PrimString msg)) -> fail msg
      | _ -> mk_err ())
    | Option p ->
      option None ((fun tm -> Some tm) <$> translate' term_ctx parser_ctx p)
      >>|| (fun ~pos opt_tm ->
             let result =
               match opt_tm with
               | Some tm -> mk_some pos tm
               | None -> none pos
             in
             result, pos)
      <?> "option"
    | Count (p, n_tm) ->
      let n =
        match Core.eval_ctx term_ctx n_tm with
        | Ok (Primitive (_, PrimInteger i)) ->
          (match Bigint.to_int i with Some n -> n | None -> mk_err ())
        | _ -> mk_err ()
      in
      count n (translate' term_ctx parser_ctx p) >>|| mk_list <?> "count"
    | Many t -> many (translate' term_ctx parser_ctx t) >>|| mk_list <?> "many"
    | Many1 t -> many1 (translate' term_ctx parser_ctx t) >>|| mk_list <?> "many1"
    (* TODO: do we even want explicit fix? or should this be done implicitly? *)
    (* | Fix (name, p) -> fix (fun p' -> translate' (Map.set ctx ~key:name ~data:p') p) *)
    | Fix _ -> failwith "TODO"
    | Alt (p1, p2) -> translate' term_ctx parser_ctx p1 <|> translate' term_ctx parser_ctx p2 <?> "alt"
    | Return tm -> (match tm with Term tm -> return tm | _ -> mk_err ())
    | LiftN (_names, _p, _ps) -> failwith "TODO"
    | Identifier name ->
      (match Map.find parser_ctx name with
      | Some p -> translate' term_ctx parser_ctx p <?> name
      | None -> mk_err ())
  in
  translate' Lvca_util.String.Map.empty Lvca_util.String.Map.empty
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
        begin
          match parse parser str with
        | Error msg -> Caml.Printf.printf "failed to parse: %s\n" msg
        | Ok tm -> Fmt.pr "%a\n" (Nominal.pp_term_range Primitive.pp) tm
        end;
          match Direct.parse_direct (Direct.translate_direct parser) str with
        | Error (msg, _) -> Caml.Printf.printf "failed to parse: %s\n" msg
        | Ok tm -> Fmt.pr "%a\n" (Nominal.pp_term_range Primitive.pp) tm
   ;;

    let%expect_test _ =
      parse_print {|"str"|} "str";
      [%expect {|
        <0-3>"str"</0-3>
        "str" |}]
    ;;

    let%expect_test _ =
      parse_print {|"str"|} "foo";
      [%expect {|
        failed to parse: string "str": string
        failed to parse: string "str"
      |}]
    ;;

    let%expect_test _ =
      parse_print {|"str"*|} "strstrstr";
      [%expect
        {|
          <0-9>list(<0-3>"str"</0-3>, <3-6>"str"</3-6>, <6-9>"str"</6-9>)</0-9>
          list("str", "str", "str") |}]
    ;;

    let%expect_test _ =
      parse_print {|"str"+|} "strstrstr";
      [%expect
        {|
          <0-9>list(<0-3>"str"</0-3>, <3-6>"str"</3-6>, <6-9>"str"</6-9>)</0-9>
          list("str", "str", "str") |}]
    ;;

    let%expect_test _ =
      parse_print {|"str" | "foo"|} "str";
      [%expect
        {|
          <0-3>"str"</0-3>
          "str" |}]
    ;;

    let%expect_test _ =
      parse_print {|"str" | "foo"|} "foo";
      [%expect
        {|
          <0-3>"foo"</0-3>
          "foo" |}]
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
      [%expect
        {|
          <0-1>'c'</0-1>
          'c' |}]
    ;;

    let%expect_test _ =
      parse_print sat_parser "d";
      [%expect
        {|
          failed to parse: satisfy(\x. ...): satisfy: 'd'
          failed to parse: satisfy (x -> match x with { 'c' -> {true()} | _ -> {false()} }) |}]
    ;;

    let%expect_test _ =
      parse_print {|let x = "str" in x|} "str";
      [%expect
        {|
          <0-3>"str"</0-3>
          "str" |}]
    ;;

    let%expect_test _ =
      parse_print {|fail {"reason"}|} "str";
      (* TODO: nicer formatting *)
      [%expect
        {|
          failed to parse: : reason
          failed to parse: reason |}]
    ;;
  end)
;;
