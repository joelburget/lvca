open Base
open Lvca_syntax
open Lvca_core
module Format = Caml.Format

let abstract_syntax_str =
  {|
// global: { char, string, n_term, c_term }

parser :=
  // primitive parsers
  | any_char()
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

type 'loc c_term = 'loc Core.term
type 'loc n_term = ('loc, Primitive.t) Nominal.term

let pp_c_term = Core.pp

type 'loc t =
  (* primitive parsers *)
  | AnyChar of 'loc
  | Char of 'loc * char
  | String of 'loc * string
  | Satisfy of 'loc * string * 'loc c_term
  | Fail of 'loc * 'loc c_term
  | Let of 'loc * string * 'loc t * 'loc t
  (* combinators *)
  | Option of 'loc * 'loc t
  | Count of 'loc * 'loc t * 'loc c_term
  | Many of 'loc * 'loc t
  | Many1 of 'loc * 'loc t
  | Fix of 'loc * string * 'loc t
  (* alternative *)
  | Alt of 'loc * 'loc t * 'loc t
  | Return of 'loc * 'loc c_term
  | Sequence of 'loc * string list * 'loc c_term * 'loc t list
  | Identifier of 'loc * string
  (* [@@deriving show] *)

let location = function
  | AnyChar loc
  | Char (loc, _)
  | String (loc, _)
  | Satisfy (loc, _, _)
  | Fail (loc, _)
  | Let (loc, _, _, _)
  | Option (loc, _)
  | Count (loc, _, _)
  | Many (loc, _)
  | Many1 (loc, _)
  | Fix (loc, _, _)
  | Alt (loc, _, _)
  | Return (loc, _)
  | Sequence (loc, _, _, _)
  | Identifier (loc, _)
  -> loc

let rec map_loc ~f =
  let cf = Core.map_loc ~f in
  function
  | AnyChar loc -> AnyChar (f loc)
  | Char (loc, c) -> Char (f loc, c)
  | String (loc, s) -> String (f loc, s)
  | Satisfy (loc, s, tm) -> Satisfy (f loc, s, cf tm)
  | Fail (loc, tm) -> Fail (f loc, cf tm)
  | Let (loc, s, p1, p2) -> Let (f loc, s, map_loc ~f p1, map_loc ~f p2)
  | Option (loc, p) -> Option (f loc, map_loc ~f p)
  | Count (loc, p, tm) -> Count (f loc, map_loc ~f p, cf tm)
  | Many (loc, p) -> Many (f loc, map_loc ~f p)
  | Many1 (loc, p) -> Many1 (f loc, map_loc ~f p)
  | Fix (loc, s, p) -> Fix (f loc, s, map_loc ~f p)
  | Alt (loc, p1, p2) -> Alt (f loc, map_loc ~f p1, map_loc ~f p2)
  | Return (loc, tm) -> Return (f loc, cf tm)
  | Sequence (loc, ss, tm, ps)
  -> Sequence (f loc, ss, cf tm, List.map ps ~f:(map_loc ~f))
  | Identifier (loc, s) -> Identifier (f loc, s)

let erase = map_loc ~f:(fun _ -> ())

let pp_generic ~open_loc ~close_loc ppf p =
  let core = Core.pp in
  let fmt, pf = Fmt.(fmt, pf) in

  let atom_prec = 2 in
  let quantifier_prec = 1 in
  let app_prec = 1 in
  let alt_prec = 0 in

  let with_parens ~ambient_prec ~prec pp =
    if ambient_prec > prec then Fmt.parens pp else pp
  in

  let rec go ambient_prec ppf p =
    let loc = location p in
    open_loc ppf loc;
    let formatter, prec = match p with
    | AnyChar _ -> fmt ".", atom_prec
    | Char (_, char) -> (fun ppf -> Fmt.(quote ~mark:"'" char) ppf char), atom_prec
    | String (_, str) -> (fun ppf -> Fmt.(quote string) ppf str), atom_prec
    | Satisfy (_, name, tm) ->
      (fun ppf -> pf ppf "@[<2>satisfy (@[%s -> %a@])@]" name core tm), atom_prec
    | Let (_, name, named, body)
    -> (fun ppf ->
        pf ppf "@[<v>@[<2>let %s =@ @[%a@] in@]@ %a@]" name (go 0) named (go 0) body),
        atom_prec
    | Fail (_, tm) ->
      (fun ppf -> pf ppf "@[<2>fail %a@]" core tm), app_prec
    | Count (_, p, tm) ->
      (fun ppf -> pf ppf "@[<hv>%a{%a}@]" (go (Int.succ quantifier_prec)) p core tm),
      quantifier_prec
    | Option (_, p) ->
      (fun ppf -> pf ppf "%a?" (go (Int.succ quantifier_prec)) p), quantifier_prec
    | Many (_, p) ->
      (fun ppf -> pf ppf "%a*" (go (Int.succ quantifier_prec)) p), quantifier_prec
    | Many1 (_, p) ->
      (fun ppf -> pf ppf "%a+" (go (Int.succ quantifier_prec)) p), quantifier_prec
    | Fix (_, name, p) ->
      (fun ppf -> pf ppf "@[<2>fix@ (@[%s -> %a@])@]" name (go 0) p), app_prec
    | Alt (_, t1, t2) ->
      (fun ppf -> pf ppf "@[<2>%a@ |@ %a@]" (go (Int.succ alt_prec)) t1 (go alt_prec) t2),
      alt_prec
    | Return (_, tm) ->
      (fun ppf -> pf ppf "@[<2>return %a@]" core tm), app_prec
    | Sequence (_, names, p, ps) ->
      let formatter ppf =
      pf
        ppf
        (* TODO: consistent binding style between sequence, fix, and satisfy *)
        "@[<2>sequence (@[%a. %a@]) [%a]@]"
        Fmt.(list ~sep:(any ".@ ") string)
        names
        core
        p
        Fmt.(list ~sep:comma (go 0))
        ps
      in
      formatter, app_prec
    | Identifier (_, name) -> (fun ppf -> pf ppf "%s" name), atom_prec
    in
    with_parens ~ambient_prec ~prec (fun ppf () -> formatter ppf) ppf ();
    close_loc ppf loc

  in
  go 0 ppf p
;;

let pp_range ppf p =
  pp_generic ~open_loc:OptRange.open_stag ~close_loc:OptRange.close_stag ppf p

let pp_plain ppf p =
  pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf p

let mk_some : 'loc n_term -> 'loc n_term =
 fun tm -> Nominal.Operator (Nominal.location tm, "some", [ Scope ([], [ tm ]) ])
;;

let mk_none pos = Nominal.Operator (pos, "none", [])
let map_snd ~f (a, b) = a, f b

type 'loc parse_error =
  { parser: 'loc t
  ; sub_errors: 'loc parse_error list
  }

module Direct = struct
  type term_ctx = SourceRanges.t n_term Lvca_util.String.Map.t
  type parser_ctx = direct Lvca_util.String.Map.t

  and direct =
    { run :
        term_ctx:term_ctx
        -> parser_ctx:parser_ctx
        -> pos:int
        -> string
        -> int * (SourceRanges.t n_term, string * SourceRanges.t c_term option) Result.t
    }

  let todo_msg msg = Error (msg, None)

  let mk_char pos c =
    Ok (Nominal.Primitive (SourceRanges.mk "input" pos (pos + 1), Primitive.PrimChar c))
  ;;

  let anychar =
    { run =
        (fun ~term_ctx:_ ~parser_ctx:_ ~pos str ->
          if String.length str > pos
          then pos + 1, mk_char pos str.[pos]
          else pos, todo_msg ".")
    }
  ;;

  let char c =
    { run =
        (fun ~term_ctx:_ ~parser_ctx:_ ~pos str ->
          if String.length str > pos && Char.(str.[pos] = c)
          then pos + 1, mk_char pos c
          else pos, todo_msg (Printf.sprintf "char '%c'" c))
    }
  ;;

  let string prefix =
    { run =
        (fun ~term_ctx:_ ~parser_ctx:_ ~pos str ->
          match str |> String.subo ~pos |> String.chop_prefix ~prefix with
          | None -> pos, todo_msg (Printf.sprintf {|string "%s"|} prefix)
          | Some _str' ->
            let pos' = pos + String.length prefix in
            let rng = SourceRanges.mk "input" pos pos' in
            pos', Ok (Nominal.Primitive (rng, PrimString prefix)))
    }
  ;;

  let satisfy name core_term =
    { run =
        (fun ~term_ctx ~parser_ctx:_ ~pos str ->
          let err_msg = todo_msg
            (Printf.sprintf {|satisfy (%s -> %s)|} name (Core.to_string core_term))
          in
          if pos >= String.length str
          then pos, err_msg
          else (
            let c = str.[pos] in
            let rng = SourceRanges.mk "input" pos (pos + 1) in
            let tm =
              Core.(
                Let (NoRec, Term (Primitive (rng, PrimChar c)), Scope (name, core_term)))
            in
            match Core.eval_ctx term_ctx tm with
            | Ok (Operator (_, "true", [])) -> pos + 1, mk_char pos c
            | Ok (Operator (_, "false", [])) | Ok _ ->
              pos, err_msg (* TODO: throw harder error? (type error) *)
            | Error err -> pos, Error (map_snd ~f:(fun tm -> Some tm) err)))
    }
  ;;

  let fail c_tm =
    { run =
        (fun ~term_ctx ~parser_ctx:_ ~pos _str ->
          match Core.eval_ctx term_ctx c_tm with
          | Ok (Primitive (_, PrimString msg)) -> pos, (* TODO: use pos *) todo_msg msg
          | _ -> failwith "TODO: fail")
    }
  ;;

  let let_ name p body =
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          let parser_ctx = Map.set parser_ctx ~key:name ~data:p in
          body.run ~term_ctx ~parser_ctx ~pos str)
    }
  ;;

  let option t =
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          match t.run ~term_ctx ~parser_ctx ~pos str with
          | str', Ok tm -> str', Ok (mk_some tm)
          | str', Error _ -> str', Ok (mk_none SourceRanges.empty))
    }
  ;;

  let mk_list_result =
    Result.map ~f:(fun lst ->
        let rng = lst |> List.map ~f:Nominal.location |> SourceRanges.unions in
        Nominal.Operator (rng, "list", [ Nominal.Scope ([], lst) ]))
  ;;

  let count n_tm t =
    let rec go ~term_ctx ~parser_ctx ~pos n str =
      match n with
      | 0 -> pos, Ok []
      | _ ->
        let pos, head_result = t.run ~term_ctx ~parser_ctx ~pos str in
        match head_result with
        | Error msg -> pos, Error msg
        | Ok tm ->
          let pos, tail_result = go ~term_ctx ~parser_ctx (n - 1) ~pos str in
          pos, tail_result |> Result.map ~f:(List.cons tm)
    in
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          match Core.eval_ctx term_ctx n_tm with
          | Ok (Primitive (_, PrimInteger n)) ->
            str
            |> go ~term_ctx ~parser_ctx ~pos (Z.to_int n (* XXX: may raise Overflow *))
            |> map_snd ~f:mk_list_result
          | _ -> failwith "TODO: count")
    }
  ;;

  let rec go_many ~term_ctx ~parser_ctx ~pos t str =
    let pos, head_result = t.run ~term_ctx ~parser_ctx ~pos str in
    match head_result with
    | Error _ -> pos, Ok []
    | Ok tm ->
      str
      |> go_many ~term_ctx ~parser_ctx ~pos t
      |> map_snd ~f:(Result.map ~f:(List.cons tm))
  ;;

  let many t =
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          str |> go_many ~term_ctx ~parser_ctx ~pos t |> map_snd ~f:mk_list_result)
    }
  ;;

  let many1 t =
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          str
          |> go_many ~term_ctx ~parser_ctx ~pos t
          |> map_snd ~f:(function
                 | Ok [] -> todo_msg "many1: empty list"
                 | result -> mk_list_result result))
    }
  ;;

  let fix name p =
    let f p' =
      { run =
          (fun ~term_ctx ~parser_ctx ~pos str ->
            p.run ~term_ctx ~parser_ctx:(Map.set parser_ctx ~key:name ~data:p') ~pos str)
      }
    in
    let rec lazy_p = lazy (f r)
    and r =
      { run =
          (fun ~term_ctx ~parser_ctx ~pos str ->
            (Lazy.force lazy_p).run ~term_ctx ~parser_ctx ~pos str)
      }
    in
    r
  ;;

  let alt t1 t2 =
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          let pos, result = t1.run ~term_ctx ~parser_ctx ~pos str in
          match result with
          | Error _ -> t2.run ~term_ctx ~parser_ctx ~pos str
          | _ -> pos, result)
    }
  ;;

  let return tm =
    { run =
        (fun ~term_ctx ~parser_ctx:_ ~pos _str ->
          let result = Core.eval_ctx term_ctx tm in
          pos, Result.map_error result ~f:(map_snd ~f:(fun tm -> Some tm)))
    }
  ;;

  let liftn names tm ps =
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          let (pos, _), result =
            List.fold_map ps ~init:(pos, true) ~f:(fun (pos, continue) { run } ->
                if continue
                then (
                  match run ~term_ctx ~parser_ctx ~pos str with
                  | pos, Ok tm -> (pos, true), Ok tm
                  | pos, Error msg -> (pos, false), Error msg)
                else (pos, false), todo_msg "don't continue")
          in
          match Result.all result with
          | Error msg -> pos, Error msg
          | Ok xs ->
            (match List.zip names xs with
            | Unequal_lengths ->
              failwith
                (Caml.Printf.sprintf
                   "TODO: liftn Unequal_lengths (%n vs %n)"
                   (List.length names)
                   (List.length xs))
            | Ok name_vals ->
              let term_ctx =
                name_vals
                |> List.fold ~init:term_ctx ~f:(fun ctx (key, tm) ->
                       Map.set ctx ~key ~data:tm)
              in
              let result =
                Core.eval_ctx term_ctx tm
                |> Result.map_error ~f:(map_snd ~f:(fun tm -> Some tm))
              in
              pos, result))
    }
  ;;

  let identifier name =
    { run =
        (fun ~term_ctx ~parser_ctx ~pos str ->
          match Map.find parser_ctx name with
          | None ->
            pos, todo_msg (Printf.sprintf {|Identifer not found in context: "%s"|} name)
          | Some p -> p.run ~term_ctx ~parser_ctx ~pos str)
    }
  ;;

  let rec translate_direct : SourceRanges.t t -> direct = function
    | AnyChar _ -> anychar
    | Char (_, c) -> char c
    | String (_, prefix) -> string prefix
    | Satisfy (_, name, core_term) -> satisfy name core_term
    | Fail (_, tm) -> fail tm
    | Let (_, name, p, body) -> let_ name (translate_direct p) (translate_direct body)
    | Option (_, t) -> option (translate_direct t)
    | Count (_, t, n) -> count n (translate_direct t)
    | Many (_, t) -> many (translate_direct t)
    | Many1 (_, t) -> many1 (translate_direct t)
    | Fix (_, name, p) -> fix name (translate_direct p)
    | Alt (_, t1, t2) -> alt (translate_direct t1) (translate_direct t2)
    | Return (_, tm) -> return tm
    | Sequence (_, names, tm, ps) -> liftn names tm (List.map ps ~f:translate_direct)
    | Identifier (_, name) -> identifier name
  ;;

  let parse_direct
    : direct
    -> string
    -> (SourceRanges.t n_term, string * SourceRanges.t c_term option) Result.t =
   fun { run } str ->
    let strlen = String.length str in
    match
      run
        ~term_ctx:Lvca_util.String.Map.empty
        ~parser_ctx:Lvca_util.String.Map.empty
        ~pos:0
        str
    with
    | n, result when n = strlen -> result
    | _, Ok _ ->
      todo_msg
        (Printf.sprintf
           {|Parser didn't consume entire input. Left over: "%s"|}
           (if strlen > 50 then String.prefix str 47 ^ "..." else str))
    | _, Error err -> Error err
 ;;

  type t = direct
end

module Parse (Comment : ParseUtil.Comment_int) = struct
  type term = OptRange.t t

  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let keywords : string list =
    [ "satisfy"; "let"; "in"; "fail"; "sequence"; "return"; "fix" ]
  ;;

  let keyword : string Parsers.t = keywords |> List.map ~f:string |> choice
  let operators : string list = [ "?"; "*"; "+"; "|"; "=" ]
  let operator : string Parsers.t = operators |> List.map ~f:string |> choice

  let t : OptRange.t Core.term Parsers.t -> term Parsers.t =
   fun c_term ->
    fix (fun parser ->
        let parse_token =
          choice
            [ char '.' >>|| (fun ~pos _ -> AnyChar pos, pos)
            ; char_lit >>|| (fun ~pos c -> Char (pos, c), pos)
            ; string_lit >>|| (fun ~pos s -> String (pos, s), pos)
            ; parens parser (* XXX: update? *)
            ; Parsers.identifier >>|| (fun ~pos name -> Identifier (pos, name), pos)
            ]
        in

        choice
          [ (string "let" >>== fun ~pos:kw_pos _ -> lift3
             (fun name bound body ->
               let pos = OptRange.union kw_pos (location body) in
               Let (pos, name, bound, body))
             Parsers.identifier
             (string "=" *> parser)
             (string "in" *> parser))
            <?> "let"
          ; (string "satisfy" >>== fun ~pos:kw_pos _ -> parens
             (lift3
                (fun name _arr (tm, pos) ->
                  let pos = OptRange.union kw_pos pos in
                  Satisfy (pos, name, tm))
                Parsers.identifier
                (string "->")
                (attach_pos c_term)))
            <?> "satisfy"
          ; (string "fail" >>== fun ~pos:kw_pos _ ->
            c_term
            >>|| (fun ~pos tm ->
              let pos = OptRange.union kw_pos pos in
              Fail (pos, tm), pos))
            <?> "fail"
          ; (string "fix" >>== fun ~pos:kw_pos _ ->
            parens
                 (lift3
                    (fun name _arr tm ->
                      let pos = OptRange.union kw_pos (location tm) in
                      Fix (pos, name, tm))
                    Parsers.identifier
                    (string "->")
                    parser))
            <?> "fix"
          ; (string "sequence" >>== fun ~pos:kw_pos _ -> lift2
             (fun (names, body) (components, pos) ->
               let pos = OptRange.union kw_pos pos in
               Sequence (pos, names, body, components))
             (parens
                (lift2
                   (fun names body -> names, body)
                   (many (Parsers.identifier <* char '.'))
                   c_term))
             (attach_pos (brackets (sep_by1 (char ',') parser))))
            <?> "sequence"
          ; (string "return" >>== fun ~pos:kw_pos _ ->
            c_term >>|| fun ~pos tm ->
            let pos = OptRange.union kw_pos pos in
            Return (pos, tm), pos)
            <?> "return"

          (* quantifiers *)
          ; (parse_token >>= fun tok ->
              let mk_pos = OptRange.union (location tok) in
              option tok
              (choice
                 [ char '?' >>|| (fun ~pos _ ->
                   let pos = mk_pos pos in
                   Option (pos, tok), pos) <?> "?"
                 ; char '*' >>|| (fun ~pos _ ->
                   let pos = mk_pos pos in
                   Many (pos, tok), pos) <?> "*"
                 ; char '+' >>|| (fun ~pos _ ->
                   let pos = mk_pos pos in
                   Many1 (pos, tok), pos) <?> "+"
                 ; (braces c_term
                   >>|| fun ~pos tm ->
                   let pos = mk_pos pos in
                   Count (pos, tok, tm), pos)
                 ; char '|' >>= (fun _ ->
                    parser >>|| fun ~pos rhs ->
                    let pos = mk_pos pos in
                    Alt (pos, tok, rhs), pos)
                   <?> "|"
                 ]))
          ]
        )
    <?> "parser"
 ;;

  let whitespace_t c_term = ParseUtil.whitespace *> t c_term
end

module TestParsers = struct
  let char_count = {|'c'{{2}}|}
  let dot = {|.|}
  let str = {|"str"|}
  let str_star = {|"str"*|}
  let str_plus = {|"str"+|}
  let alt = {|"str" | "foo"|}

  let sat_parser =
    {|satisfy (x -> match x with {
    | 'c' -> {true()}
    | _ -> {false()}
  })
  |}

  let let_var = {|let x = "str" in x|}
  let fail = {|fail {"reason"}|}
  let char_opt = "'c'?"
  let ret = "return {foo()}"
  let fix = {|fix (x -> "a" | "b")|}
  let seq = {|sequence(a. {"a"}) ["a"]|}

  let list_parser =
    {|fix (lst ->
        (sequence (x. xs. {cons(x; xs)}) ['c', lst]) |
        return {nil()}
      )
    |}

  let seq2 = {|sequence(a. a'. b. {triple(a; a'; b)})["a", "a", "b"]|}
  let seq3 = {|sequence(a. a'. b. {triple(a; a'; b)})['a', 'a', 'b']|}
  let fix2 = {|fix (x -> "b" | sequence(a. x. {pair(a; x)}) ["a", x])|}
  let pair = "sequence (a. b. {pair(a; b)}) ['a', 'b']"
end

let%test_module "Parsing" =
  (module struct
    module ParseCore = Core.Parse (ParseUtil.CComment)
    module ParseParser = Parse (ParseUtil.CComment)

    let parse_print : string -> string -> unit =
     fun parser_str str ->
      match ParseUtil.parse_string (ParseParser.t ParseCore.term) parser_str with
      | Error msg -> Caml.print_string ("failed to parse parser desc: " ^ msg)
      | Ok parser ->
        let parser' = map_loc ~f:(SourceRanges.of_opt_range ~buf:"parser") parser in
        match Direct.parse_direct (Direct.translate_direct parser') str with
        | Error (msg, _) -> Caml.Printf.printf "failed to parse: %s\n" msg
        | Ok tm -> Fmt.pr "%a\n" (Nominal.pp_term_ranges Primitive.pp) tm
   ;;

   open TestParsers

    let () =
      Format.set_formatter_stag_functions SourceRanges.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let%expect_test _ =
      parse_print char_count "cc";
      [%expect
        {| <input:0-2>list(<input:0-1>'c'</input:0-1>, <input:1-2>'c'</input:1-2>)</input:0-2> |}]
    ;;

    let%expect_test _ =
      parse_print dot "c";
      [%expect{| <input:0-1>'c'</input:0-1> |}]
    ;;

    let%expect_test _ =
      parse_print str "str";
      [%expect {| <input:0-3>"str"</input:0-3> |}]
    ;;

    let%expect_test _ =
      parse_print str "foo";
      [%expect {| failed to parse: string "str" |}]
    ;;

    let%expect_test _ =
      parse_print str_star "strstrstr";
      [%expect
        {| <input:0-9>list(<input:0-3>"str"</input:0-3>, <input:3-6>"str"</input:3-6>, <input:6-9>"str"</input:6-9>)</input:0-9> |}]
    ;;

    let%expect_test _ =
      parse_print str_plus "strstrstr";
      [%expect
        {| <input:0-9>list(<input:0-3>"str"</input:0-3>, <input:3-6>"str"</input:3-6>, <input:6-9>"str"</input:6-9>)</input:0-9> |}]
    ;;

    let%expect_test _ =
      parse_print alt "str";
      [%expect {| <input:0-3>"str"</input:0-3> |}]
    ;;

    let%expect_test _ =
      parse_print alt "foo";
      [%expect {| <input:0-3>"foo"</input:0-3> |}]
    ;;

    let%expect_test _ =
      parse_print sat_parser "c";
      [%expect {| <input:0-1>'c'</input:0-1> |}]
    ;;

    let%expect_test _ =
      parse_print sat_parser "d";
      [%expect
        {|
          failed to parse: satisfy (x -> match x with { 'c' -> {true()} | _ -> {false()} }) |}]
    ;;

    let%expect_test _ =
      parse_print let_var "str";
      [%expect {| <input:0-3>"str"</input:0-3> |}]
    ;;

    let%expect_test _ =
      parse_print fail "str";
      (* TODO: nicer formatting *)
      [%expect {| failed to parse: reason |}]
    ;;

    let%expect_test _ =
      parse_print char_opt "c";
      [%expect {| <input:0-1>some(<input:0-1>'c'</input:0-1>)</input:0-1> |}]
    ;;

    (* TODO: determine proper provenance *)
    let%expect_test _ =
      parse_print char_opt "";
      [%expect {| <>none()</> |}]
    ;;

    let%expect_test _ =
      parse_print ret "";
      [%expect {| <parser:8-13>foo()</parser:8-13> |}]
    ;;

    (* let%expect_test _ = parse_print "sequence (. {foo()}) []" ""; [%expect{| foo() |}]
       ;; *)

    let%expect_test _ =
      parse_print fix "a";
      [%expect {| <input:0-1>"a"</input:0-1> |}]
    ;;

    let%expect_test _ =
      parse_print seq "a";
      [%expect {| <parser:13-16>"a"</parser:13-16> |}]
    ;;

    let%expect_test _ =
      parse_print list_parser "";
      [%expect {| <parser:83-88>nil()</parser:83-88> |}]
    ;;

    let%expect_test _ =
      parse_print list_parser "c";
      [%expect
        {| <parser:39-50>cons(<input:0-1>'c'</input:0-1>; <parser:83-88>nil()</parser:83-88>)</parser:39-50> |}]
    ;;

    let%expect_test _ =
      parse_print list_parser "cc";
      [%expect
        {| <parser:39-50>cons(<input:0-1>'c'</input:0-1>; <parser:39-50>cons(<input:1-2>'c'</input:1-2>; <parser:83-88>nil()</parser:83-88>)</parser:39-50>)</parser:39-50> |}]
    ;;

    let%expect_test _ =
      parse_print seq2 "aab";
      [%expect
        {| <parser:20-36>triple(<input:0-1>"a"</input:0-1>; <input:1-2>"a"</input:1-2>; <input:2-3>"b"</input:2-3>)</parser:20-36> |}]
    ;;

    let%expect_test _ =
      parse_print seq3 "aab";
      [%expect
        {| <parser:20-36>triple(<input:0-1>'a'</input:0-1>; <input:1-2>'a'</input:1-2>; <input:2-3>'b'</input:2-3>)</parser:20-36> |}]
    ;;

    let%expect_test _ =
      parse_print fix2 "a";
      [%expect {| failed to parse: string "a" |}]
    ;;

    let%expect_test _ =
      parse_print fix2 "ab";
      [%expect
        {| <parser:32-42>pair(<input:0-1>"a"</input:0-1>; <input:1-2>"b"</input:1-2>)</parser:32-42> |}]
    ;;

    let%expect_test _ =
      parse_print pair "ab";
      [%expect
        {| <parser:17-27>pair(<input:0-1>'a'</input:0-1>; <input:1-2>'b'</input:1-2>)</parser:17-27> |}]
    ;;

   let parse_print_parser : string -> unit =
     fun parser_str ->
      match ParseUtil.parse_string (ParseParser.t ParseCore.term) parser_str with
      | Error msg -> Caml.print_string ("failed to parse parser desc: " ^ msg)
      | Ok parser -> Fmt.pr "%a\n" pp_range parser
   ;;

   let%expect_test _ =
     parse_print_parser char_count;
     [%expect{| 'c'{{2}} |}]

   let%expect_test _ =
     parse_print_parser "F++";
     [%expect{| failed to parse parser desc: : end_of_input |}]

   let%expect_test _ =
     parse_print_parser "(F+)+";
     [%expect{| (F+)+ |}]

   let%expect_test _ =
     parse_print_parser dot;
     [%expect{| . |}]

   let%expect_test _ =
     parse_print_parser "let a = b | c in d?";
     [%expect{|
       let a = b | c in
       d? |}]

   let%expect_test _ =
     parse_print_parser str_star;
     [%expect{| "str"* |}]

   let%expect_test _ =
     parse_print_parser "'a' | 'b' | 'c'";
     [%expect{| 'a' | 'b' | 'c' |}]

   let%expect_test _ =
     parse_print_parser list_parser;
     [%expect{|

       fix (lst -> sequence (x. xs. {cons(x; xs)}) ['c', lst] | return {nil()}) |}]
  end)
;;

module Properties = struct
  open PropertyResult
  module ParseCore = Core.Parse (ParseUtil.CComment)
  module ParseParser = Parse (ParseUtil.CComment)

  let parse parser_str = ParseUtil.parse_string (ParseParser.t ParseCore.term) parser_str

  let pp_str p = Fmt.str "%a" pp_plain p

  let string_round_trip1 t =
    match t |> pp_str |> parse with
    | Ok t' ->
      let t'' = erase t' in
      PropertyResult.check Caml.(t'' = t) (Fmt.str "%a <> %a" pp_plain t'' pp_plain t)
    | Error msg ->
      Failed (Fmt.str {|parse_string "%s": %s|} (pp_str t) msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = pp_str t in
      if Base.String.(str' = str)
      then Ok
      else
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = pp_str t' in
          PropertyResult.check String.(str'' = str') (Fmt.str {|"%s" <> "%s"|} str'' str')
  ;;
end
