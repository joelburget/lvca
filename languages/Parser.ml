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

type parser_stack = SourceRanges.t t Stack.t

module Direct = struct
  type 'loc parser = 'loc t
  type term_ctx = SourceRanges.t n_term Lvca_util.String.Map.t
  type parser_ctx = SourceRanges.t parser Lvca_util.String.Map.t

  and parse_result =
    { snapshot: trace_snapshot
    ; result: (SourceRanges.t n_term, string * SourceRanges.t c_term option) Result.t
    }

  and direct =
    { run :
           translate_direct:(SourceRanges.t parser -> direct)
        -> term_ctx:term_ctx
        -> parser_ctx:parser_ctx
        -> pos:int
        -> string
        -> trace_snapshot * (SourceRanges.t n_term, string * SourceRanges.t c_term option) Result.t
    }

  and trace_snapshot =
    { pos: int
    ; name: string (* TODO: full description *)
    ; term_ctx: term_ctx
    ; parser_ctx: parser_ctx
    ; subparses: trace_snapshot list
    }

  type t = direct

  let mk_snapshot ~name ~term_ctx ~parser_ctx ?subparses:(subparses=[]) pos
    = { pos; name; term_ctx; parser_ctx; subparses }

  let mk_error msg = Error (msg, None)

  let mk_char pos c =
    Nominal.Primitive (SourceRanges.mk "input" pos (pos + 1), Primitive.PrimChar c)
  ;;

  let context_free ~name go =
    { run = fun ~translate_direct:_ ~term_ctx ~parser_ctx ~pos str ->
        let mk_snapshot = mk_snapshot ~name ~term_ctx ~parser_ctx in
        match go pos str with
          | Ok (pos', result) -> mk_snapshot pos', Ok result
          | Error msg -> mk_snapshot pos, mk_error msg
    }
  ;;

  let anychar = context_free ~name:"." (fun pos str ->
    if String.length str > pos
    then Ok (pos + 1, mk_char pos str.[pos])
    else Error ".")
  ;;

  let char c = context_free ~name:"char" (fun pos str ->
    if String.length str > pos && Char.(str.[pos] = c)
    then Ok (pos + 1, mk_char pos c)
    else Error (Printf.sprintf "char '%c'" c))
  ;;

  let string prefix = context_free ~name:"string" (fun pos str ->
    match str |> String.subo ~pos |> String.chop_prefix ~prefix with
    | None -> Error (Printf.sprintf {|string "%s"|} prefix)
    | Some _str' ->
      let pos' = pos + String.length prefix in
      let rng = SourceRanges.mk "input" pos pos' in
      Ok (pos', Nominal.Primitive (rng, PrimString prefix)))
  ;;

  let satisfy name core_term =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx ~pos str ->
          let mk_snapshot = mk_snapshot ~name:"satisfy" ~term_ctx ~parser_ctx in
          let err_msg = mk_error
            (Printf.sprintf {|satisfy (%s -> %s)|} name (Core.to_string core_term))
          in
          if pos >= String.length str
          then mk_snapshot pos, err_msg
          else (
            let c = str.[pos] in
            let rng = SourceRanges.mk "input" pos (pos + 1) in
            let tm =
              Core.(
                Let (NoRec, Term (Primitive (rng, PrimChar c)), Scope (name, core_term)))
            in
            match Core.eval_ctx term_ctx tm with
            | Ok (Operator (_, "true", [])) ->
              mk_snapshot (pos + 1), Ok (mk_char pos c)
            | Ok (Operator (_, "false", [])) | Ok _ ->
              mk_snapshot pos, err_msg (* TODO: throw harder error? (type error) *)
            | Error err ->
              mk_snapshot pos, Error (map_snd ~f:(fun tm -> Some tm) err)))
    }
  ;;

  let fail c_tm =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx ~pos _str ->
          match Core.eval_ctx term_ctx c_tm with
          | Ok (Primitive (_, PrimString msg)) ->
            mk_snapshot ~name:"fail" ~term_ctx ~parser_ctx pos, mk_error msg
          | _ -> failwith "TODO: fail")
    }
  ;;

  let let_ name p body =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let parser_ctx = Map.set parser_ctx ~key:name ~data:p in
          let subparse, result =
            (translate_direct body).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~name:"let" ~term_ctx ~parser_ctx ~subparses:[subparse]
              subparse.pos
          in
          snapshot, result
        )
    }
  ;;

  let option t =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let subparse, result =
            (translate_direct t).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~name:"option" ~term_ctx ~parser_ctx ~subparses:[subparse]
              subparse.pos
          in
          let result = match result with
            | Ok tm -> mk_some tm
            | Error _ -> mk_none SourceRanges.empty
          in
          snapshot, Ok result)
    }
  ;;

  let mk_list = fun lst ->
    let rng = lst |> List.map ~f:Nominal.location |> SourceRanges.unions in
    Nominal.Operator (rng, "list", [ Nominal.Scope ([], lst) ])
  ;;

  let count n_tm p =
    let rec go ~translate_direct ~term_ctx ~parser_ctx ~pos n str =
      let p = translate_direct p in
      match n with
      | 0 -> Ok []
      | _ ->
        let snapshot, head_result = p.run ~translate_direct ~term_ctx ~parser_ctx ~pos str in
        match head_result with
        | Error msg -> Error (snapshot, msg)
        | Ok tm ->
          go ~translate_direct ~term_ctx ~parser_ctx (n - 1) ~pos:snapshot.pos str
            |> Result.map ~f:(List.cons (snapshot, tm))
    in
    { run = fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
        match Core.eval_ctx term_ctx n_tm with
        | Ok (Primitive (_, PrimInteger n)) ->
          let n = Z.to_int n (* XXX: may raise Overflow *) in
          let results =
            go ~translate_direct ~term_ctx ~parser_ctx ~pos n str
          in
          let snapshot, result = match results with
            | Ok results ->
              let subparses, tms = List.unzip results in
              let pos = match List.last subparses with
                | None -> pos
                | Some { pos; _ } -> pos
              in
              let snapshot = mk_snapshot ~name:"count" ~term_ctx ~parser_ctx ~subparses
                pos
              in
              snapshot, Ok tms
            | Error (snapshot, msg) -> snapshot, Error msg
          in
          snapshot, Result.map ~f:mk_list result
        | Ok _
        | Error _ -> failwith "TODO"
    }
  ;;

  let rec go_many ~translate_direct ~term_ctx ~parser_ctx ~pos t str =
    let snapshot, head_result =
      (translate_direct t).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
    in
    match head_result with
    | Error _ -> snapshot, Ok []
    | Ok tm -> str
      |> go_many ~translate_direct ~term_ctx ~parser_ctx ~pos:snapshot.pos t
      |> map_snd ~f:(Result.map ~f:(List.cons tm))
  ;;

  let many t =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let snapshot, result =
            go_many ~translate_direct ~term_ctx ~parser_ctx ~pos t str
          in
          let snapshot =
            mk_snapshot ~name:"*" ~term_ctx ~parser_ctx ~subparses:[snapshot] snapshot.pos
          in
          let result = Result.map result ~f:mk_list in
          snapshot, result)
    }
  ;;

  let many1 t =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let snapshot, result = go_many ~translate_direct ~term_ctx ~parser_ctx ~pos t
          str
          in
          let snapshot =
            mk_snapshot ~name:"+" ~term_ctx ~parser_ctx ~subparses:[snapshot] snapshot.pos
          in
          let result = match result with
            | Ok [] -> mk_error "many1: empty list"
            | Ok tms -> Ok (mk_list tms)
            | Error msg -> Error msg
          in
          snapshot, result)
    }
  ;;

  let fix name p =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let parser_ctx = Map.set parser_ctx ~key:name ~data:p in
          let snapshot, result =
            (translate_direct p).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~name:"fix" ~term_ctx ~parser_ctx ~subparses:[snapshot] snapshot.pos
          in
          snapshot, result)
    }
  ;;

  let alt t1 t2 =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let t1 = translate_direct t1 in
          let t2 = translate_direct t2 in
          let snapshot1, result = t1.run ~translate_direct ~term_ctx ~parser_ctx ~pos str in
          let snapshots, pos, result = match result with
          | Error _ ->
            let snapshot2, result =
              t2.run ~translate_direct ~term_ctx ~parser_ctx ~pos:snapshot1.pos str
            in
            [snapshot1; snapshot2], snapshot2.pos, result
          | _ -> [snapshot1], snapshot1.pos, result
          in
          let snapshot =
            mk_snapshot ~name:"|" ~term_ctx ~parser_ctx ~subparses:snapshots pos
          in
          snapshot, result
        )
    }
  ;;

  let return tm =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx ~pos _str ->
          let result = Core.eval_ctx term_ctx tm in
          let snapshot = mk_snapshot ~name:"return" ~term_ctx ~parser_ctx pos in
          snapshot, Result.map_error result ~f:(map_snd ~f:(fun tm -> Some tm)))
    }
  ;;

  let liftn names tm ps =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          (* Run through each subparser. We end up with
           * - pos: The position the last parser ended up
           * - results: (snapshop, result) option list
           *)
          let (pos, _), results =
            List.fold_map ps ~init:(pos, true) ~f:(fun (pos, continue) p ->
              let { run } = translate_direct p in
              if continue
              then (
                let snapshot, result =
                  run ~translate_direct ~term_ctx ~parser_ctx ~pos str
                in
                let continue = match result with Ok _ -> true | Error _ -> false in
                (snapshot.pos, continue), Some (snapshot, result))
              else (pos, false), None)
          in

          (* Get the list of subparser results. If there was an error, the last one will
           * be Error, otherwise all will be Ok. If there wasn't an error there will
           * always be n Ok results.
           *)
          let subparses, results = results
            |> List.filter_map ~f:Fn.id
            |> List.unzip
          in

          let snapshot = mk_snapshot ~name:"sequence" ~term_ctx ~parser_ctx ~subparses pos
          in
          match Result.all results with
          | Error msg -> snapshot, Error msg
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
              snapshot, result))
    }
  ;;

  let identifier name =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          match Map.find parser_ctx name with
          | None ->
            mk_snapshot ~name ~term_ctx ~parser_ctx pos,
            mk_error (Printf.sprintf {|Identifer not found in context: "%s"|} name)
          | Some p ->
            (translate_direct p).run ~translate_direct ~term_ctx ~parser_ctx ~pos str)
    }
  ;;

  let translate_direct : SourceRanges.t parser -> direct = function
    | AnyChar _ -> anychar
    | Char (_, c) -> char c
    | String (_, prefix) -> string prefix
    | Satisfy (_, name, core_term) -> satisfy name core_term
    | Fail (_, tm) -> fail tm
    | Let (_, name, p, body) -> let_ name p body
    | Option (_, t) -> option t
    | Count (_, t, n) -> count n t
    | Many (_, t) -> many t
    | Many1 (_, t) -> many1 t
    | Fix (_, name, p) -> fix name p
    | Alt (_, t1, t2) -> alt t1 t2
    | Return (_, tm) -> return tm
    | Sequence (_, names, tm, ps) -> liftn names tm ps
    | Identifier (_, name) -> identifier name
  ;;

  let parse_direct : direct -> string -> parse_result
   = fun { run } str ->
    let strlen = String.length str in
    let snapshot, result = run
      ~translate_direct
      ~term_ctx:Lvca_util.String.Map.empty
      ~parser_ctx:Lvca_util.String.Map.empty
      ~pos:0
      str
    in
    let result = match result with
      | result when snapshot.pos = strlen -> result
      | Ok _ -> mk_error
        (Printf.sprintf
           {|Parser didn't consume entire input. Left over: "%s"|}
           (if strlen > 50 then String.prefix str 47 ^ "..." else str))
      | Error _ as result -> result
    in
    { snapshot; result }
 ;;
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
        let Direct.{ result; _ } =
          Direct.parse_direct (Direct.translate_direct parser') str
        in
        match result with
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
      | Ok parser -> Fmt.pr "%a\n" pp_plain parser
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
     parse_print_parser "sequence (Ok. Q) [.] | .";
     [%expect{| sequence (Ok. Q) [.] | . |}]

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
