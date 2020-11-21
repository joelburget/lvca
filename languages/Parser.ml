open Base
open Lvca_syntax
open Lvca_core
module Format = Caml.Format

module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

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
  | Sequence of 'loc * (string option * 'loc t) list * 'loc t
  | Return of 'loc * 'loc c_term
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
  | Sequence (loc, _, _)
  | Return (loc, _)
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
  | Sequence (loc, ps, p)
  ->
    let ps' = ps
      |> List.map ~f:(fun (name, p) -> name, map_loc ~f p)
    in
    Sequence (f loc, ps', map_loc ~f p)
  | Return (loc, tm) -> Return (f loc, cf tm)
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
    | Sequence (_, ps, p) ->
      let named_parser ppf (opt_name, p) = match opt_name with
        | None -> pf ppf "%a" (go 0) p
        | Some name -> pf ppf "%s:%a" name (go 0) p
      in
      let formatter ppf = pf
        ppf
        "%a -> %a"
        Fmt.(list ~sep:(any ".@ ") named_parser)
        ps
        (go 0)
        p
      in
      formatter, app_prec
    | Return (_, tm) -> (fun ppf -> pf ppf "@[<2>%a@]" core tm), app_prec
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
        -> int * trace_snapshot list * (SourceRanges.t n_term, string * SourceRanges.t c_term option) Result.t
    }

  and trace_snapshot =
    { success: bool
    ; pre_pos: int
    ; post_pos: int
    ; parser: SourceRanges.t parser
    ; term_ctx: term_ctx
    ; parser_ctx: parser_ctx
    ; snapshots: trace_snapshot list
    }

  type t = direct

  let mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ?snapshots:(snapshots=[])
    pre_pos post_pos
    = { success = Result.is_ok result; pre_pos; post_pos; parser; term_ctx; parser_ctx;
    snapshots }

  let mk_error msg = Error (msg, None)

  let mk_char pos c =
    Nominal.Primitive (SourceRanges.mk "input" pos (pos + 1), Primitive.PrimChar c)
  ;;

  let context_free go =
    { run = fun ~translate_direct:_ ~term_ctx:_ ~parser_ctx:_ ~pos str ->
        match go pos str with
          | Ok (pos, result) -> pos, [], Ok result
          | Error msg -> pos, [], mk_error msg
    }
  ;;

  let anychar = context_free (fun pos str ->
    if String.length str > pos
    then Ok (pos + 1, mk_char pos str.[pos])
    else Error ".")
  ;;

  let char c = context_free (fun pos str ->
    if String.length str > pos && Char.(str.[pos] = c)
    then Ok (pos + 1, mk_char pos c)
    else Error (Printf.sprintf "char '%c'" c))
  ;;

  let string prefix = context_free (fun pos str ->
    match str |> String.subo ~pos |> String.chop_prefix ~prefix with
    | None -> Error (Printf.sprintf {|string "%s"|} prefix)
    | Some _str' ->
      let pos' = pos + String.length prefix in
      let rng = SourceRanges.mk "input" pos pos' in
      Ok (pos', Nominal.Primitive (rng, PrimString prefix)))
  ;;

  let satisfy name core_term =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx:_ ~pos str ->
          let err_msg = mk_error
            (Printf.sprintf {|satisfy (%s -> %s)|} name (Core.to_string core_term))
          in
          if pos >= String.length str
          then pos, [], err_msg
          else (
            let c = str.[pos] in
            let rng = SourceRanges.mk "input" pos (pos + 1) in
            let tm =
              Core.(
                Let (NoRec, Term (Primitive (rng, PrimChar c)), Scope (name, core_term)))
            in
            match Core.eval_ctx term_ctx tm with
            | Ok (Operator (_, "true", [])) ->
              pos + 1, [], Ok (mk_char pos c)
            | Ok (Operator (_, "false", [])) | Ok _ ->
                pos, [], err_msg (* TODO: throw harder error? (type error) *)
            | Error err ->
              pos, [], Error (map_snd ~f:(fun tm -> Some tm) err)))
    }
  ;;

  let fail c_tm =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx:_ ~pos _str ->
          match Core.eval_ctx term_ctx c_tm with
          | Ok (Primitive (_, PrimString msg)) -> pos, [], mk_error msg
          | _ -> failwith "TODO: fail")
    }
  ;;

  let let_ name parser body =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let parser_ctx = Map.set parser_ctx ~key:name ~data:parser in
          let pos0 = pos in
          let pos1, snapshots, result =
            (translate_direct body).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~result ~parser:body ~term_ctx ~parser_ctx ~snapshots pos0 pos1
          in
          pos1, [snapshot], result
        )
    }
  ;;

  let option parser =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let pos0 = pos in
          let pos1, snapshots, result =
            (translate_direct parser).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot = mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots
            pos0 pos1
          in
          let result = match result with
            | Ok tm -> mk_some tm
            | Error _ -> mk_none SourceRanges.empty
          in
          pos1, [snapshot], Ok result)
    }
  ;;

  let mk_list = fun lst ->
    let rng = lst |> List.map ~f:Nominal.location |> SourceRanges.unions in
    Nominal.Operator (rng, "list", [ Nominal.Scope ([], lst) ])
  ;;

  let count n_tm parser =
    let rec go ~translate_direct ~term_ctx ~parser_ctx ~pos n str =
      match n with
      | 0 -> Ok []
      | _ ->
        let pos0 = pos in
        let pos, snapshots, head_result =
          (translate_direct parser).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
        in
        let snapshot =
          mk_snapshot ~result:head_result ~parser ~term_ctx ~parser_ctx ~snapshots
            pos0 pos
        in
        match head_result with
        | Error msg -> Error (pos, snapshot :: snapshots, msg)
        | Ok tm ->
          go ~translate_direct ~term_ctx ~parser_ctx (n - 1) ~pos str
            |> Result.map ~f:(List.cons (pos, snapshot, tm))
    in

    { run = fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
        match Core.eval_ctx term_ctx n_tm with
        | Ok (Primitive (_, PrimInteger n)) ->
          let n = Z.to_int n (* TODO: may raise Overflow *) in
          let results = go ~translate_direct ~term_ctx ~parser_ctx ~pos n str in
          let pos, rev_snapshots, result = match results with
            | Ok results ->
              let poss, snapshots, tms = List.unzip3 results in
              let pos = match List.last poss with
                | None -> pos
                | Some pos -> pos
              in
              pos, snapshots, Ok tms
            | Error (pos, rev_snapshots, msg) -> pos, rev_snapshots, Error msg
          in
          pos, List.rev rev_snapshots, Result.map ~f:mk_list result
        | Ok _
        | Error _ -> failwith "TODO"
    }
  ;;

  let rec go_many ~translate_direct ~term_ctx ~parser_ctx ~pos parser str =
    let pos0 = pos in
    let pos, snapshots, head_result =
      (translate_direct parser).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
    in
    let snapshot =
      mk_snapshot ~result:head_result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos
    in
    match head_result with
    | Error _ -> pos, [snapshot], Ok []
    | Ok tm ->
      let pos, snapshots, result =
        go_many ~translate_direct ~term_ctx ~parser_ctx ~pos parser str
      in
      pos, snapshot :: snapshots, Result.map result ~f:(List.cons tm)
  ;;

  let many parser =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let pos, snapshots, result =
            go_many ~translate_direct ~term_ctx ~parser_ctx ~pos parser str
          in
          let result = Result.map result ~f:mk_list in
          pos, snapshots, result)
    }
  ;;

  let many1 parser =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let pos, snapshots, result =
            go_many ~translate_direct ~term_ctx ~parser_ctx ~pos parser str
          in
          let result = match result with
            | Ok [] -> mk_error "many1: empty list"
            | Ok tms -> Ok (mk_list tms)
            | Error msg -> Error msg
          in
          pos, snapshots, result)
    }
  ;;

  let fix name parser =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let pos0 = pos in
          let parser_ctx = Map.set parser_ctx ~key:name ~data:parser in
          let pos1, snapshots, result =
            (translate_direct parser).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot = mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots
            pos0 pos1
          in
          pos1, [snapshot], result)
    }
  ;;

  let alt p1 p2 =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let pos0 = pos in
          let pos, snapshots1, result =
            (translate_direct p1).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let pos1 = pos in
          let snapshot1 =
            mk_snapshot ~result ~parser:p1 ~term_ctx ~parser_ctx ~snapshots:snapshots1
              pos0 pos1
          in
          let pos, snapshots, result = match result with
            | Error _ ->
              let pos, snapshots2, result =
                (translate_direct p2).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
              in
              let pos2 = pos in
              let snapshot2 =
                mk_snapshot ~result ~parser:p2 ~term_ctx ~parser_ctx ~snapshots:snapshots2
                  pos1 pos2
              in
              pos, [snapshot1; snapshot2], result
            | _ -> pos, [snapshot1], result
          in
          pos, snapshots, result
        )
    }
  ;;

  let liftn named_ps parser =
    let names, ps = List.unzip named_ps in
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          (* Run through each subparser. We end up with
           * - pos: The position the last parser ended up
           * - results: (snapshop, result) option list
           *)
          let (pos, _), results =
            List.fold_map ps ~init:(pos, true) ~f:(fun (pos, continue) parser ->
              let { run } = translate_direct parser in
              if continue
              then (
                let pre_pos = pos in
                let pos, snapshots, result =
                  run ~translate_direct ~term_ctx ~parser_ctx ~pos str
                in
                let snapshot =
                  mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pre_pos pos
                in
                let continue = match result with Ok _ -> true | Error _ -> false in
                (pos, continue), Some (snapshot, result))
              else (pos, false), None)
          in

          (* Get the list of subparser results. If there was an error, the last one will
           * be Error, otherwise all will be Ok. If there wasn't an error there will
           * always be n Ok results.
           *)
          let snapshots, results = results
            |> List.filter_map ~f:Fn.id
            |> List.unzip
          in

          match Result.all results with
          | Error msg -> pos, snapshots, Error msg
          | Ok xs ->
            (match List.zip names xs with
            | Unequal_lengths ->
              failwith
                (Caml.Printf.sprintf
                   "TODO: liftn Unequal_lengths (%n vs %n)"
                   (List.length names)
                   (List.length xs))
            | Ok name_vals ->
              let term_ctx = name_vals
                |> List.fold ~init:term_ctx ~f:(fun ctx (key_opt, tm) ->
                  match key_opt with
                    | None -> ctx
                    | Some key -> Map.set ctx ~key ~data:tm)
              in
              let pos0 = pos in
              let pos1, snapshots', result = (translate_direct parser).run
                ~translate_direct ~term_ctx ~parser_ctx ~pos str
              in
              let snapshot =
                mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots:snapshots'
                  pos0 pos1
              in
              pos1, snapshot::snapshots, result))
    }
  ;;

  let return tm =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx:_ ~pos _str ->
          let result =
                Core.eval_ctx term_ctx tm
                |> Result.map_error ~f:(map_snd ~f:(fun tm -> Some tm))
          in
          pos, [], result)
    }

  let identifier name =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          match Map.find parser_ctx name with
          | None ->
            pos, [],
            mk_error (Printf.sprintf {|Identifer not found in context: "%s"|} name)
          | Some parser ->
            let pos0 = pos in
            let pos1, snapshots, result = (translate_direct parser).run
              ~translate_direct ~term_ctx ~parser_ctx ~pos str
            in
            let snapshot =
              mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos1
            in
            pos1, [snapshot], result)
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
    | Sequence (_, ps, p) -> liftn ps p
    | Identifier (_, name) -> identifier name
    | Return (_, tm) -> return tm
  ;;

  let parse_direct : SourceRanges.t parser -> string -> parse_result
   = fun parser str ->
    let { run } = translate_direct parser in
    let strlen = String.length str in
    let term_ctx = Lvca_util.String.Map.empty in
    let parser_ctx = Lvca_util.String.Map.empty in
    let pos, snapshots, result = run ~translate_direct ~term_ctx ~parser_ctx ~pos:0 str in
    let result = match result with
      | result when pos = strlen -> result
      | Ok _ -> mk_error
        (Printf.sprintf
           {|Parser didn't consume entire input. Left over: "%s"|}
           (if strlen > 50 then String.prefix str 47 ^ "..." else str))
      | Error _ as result -> result
    in
    let snapshot = mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots 0 pos in
    { snapshot; result }
 ;;
end

module Parse (Comment : ParseUtil.Comment_int) = struct
  type term = OptRange.t t

  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let keywords : string list = [ (* "satisfy"; *) "let"; "in"; (* "fail"; "fix" *) ]
  let keyword : string Parsers.t = keywords |> List.map ~f:string |> choice
  let operators : string list = [ "?"; "*"; "+"; "|"; "="; ":" ]
  let operator : string Parsers.t = operators |> List.map ~f:string |> choice

  type atom =
    | CharAtom of char
    | StrAtom of string
    | Dot

  let string_of_atom = function
    | CharAtom c -> Printf.sprintf "%C" c
    | StrAtom str -> Printf.sprintf "%S" str
    | Dot -> "."

  type token =
    | Atom of atom
    | Operator of string
    | Keyword of string
    | Ident of string
    | Core of OptRange.t Core.term
    | Parenthesized of term
    | SatisfyTok of string * OptRange.t Core.term
    | FailTok of OptRange.t Core.term
    | FixTok of string * token list

  let rec string_of_token = function
    | Atom atom -> string_of_atom atom
    | Operator str | Keyword str | Ident str -> str
    | Core tm -> Core.to_string tm
    | Parenthesized tm -> Fmt.str "%a" pp_plain tm
    | SatisfyTok (name, tm) -> Fmt.str "satisfy (%s -> %a)" name Core.pp tm
    | FailTok tm -> Fmt.str "fail (%a)" Core.pp tm
    | FixTok (name, toks) -> Fmt.str "fix (%s -> %s)"
      name
      (toks |> List.map ~f:string_of_token |> String.concat ~sep:"; ")

  let todo_pos = None

  let lbp = function
    | "?" | "*" | "+" -> 3 (* ? *)
    | ":" -> 2
    | "|" -> 1
    | "=" -> failwith "TODO"
    | _ -> failwith "invalid operator name"

  let rec prefix ~tokens = function
    | Atom atom -> return ~pos:todo_pos (match atom with
      | CharAtom c -> Char (todo_pos, c)
      | StrAtom c -> String (todo_pos, c)
      | Dot -> AnyChar todo_pos
    )
    | Operator _ -> fail "unexpected operator"
    | Keyword kw_name -> (match kw_name with
      | "let" ->
        (match Queue.dequeue tokens with
          | Some (Ident name) ->
            (match Queue.dequeue tokens with
              | Some (Operator "=") ->
                expression ~tokens ~ambient_prec:0 >>= fun e1 ->
                (match Queue.dequeue tokens with
                  | Some (Keyword "in")
                  -> expression ~tokens ~ambient_prec:0 >>= fun e2 ->
                    let pos = todo_pos in
                    return ~pos (Let (pos, name, e1, e2))
                  | Some _ | None -> fail "TODO: error")
              | Some _ | None -> fail "TODO: error")
          | Some _ | None -> fail "TODO: error")
      | _ -> failwith "invalid keyword"
    )
    | Ident name ->
      let pos = todo_pos in
      return ~pos (Identifier (pos, name))
    | Core _tm -> failwith "TODO"
      (* TODO: bring back return, remove implicit return from sequence?
      let pos = todo_pos in
      return ~pos (Core (pos, tm))
      *)
    | Parenthesized p ->
      let pos = location p in
      return ~pos p
    | SatisfyTok (name, tm) ->
      let pos = todo_pos in
      return ~pos (Satisfy (pos, name, tm))
    | FailTok tm ->
      let pos = todo_pos in
      return ~pos (Fail (pos, tm))
    | FixTok (name, toks) ->
      expression ~tokens:(Queue.of_list toks) ~ambient_prec:0 >>= fun expr ->
      let pos = todo_pos in
      return ~pos (Fix (pos, name, expr))

  (* a:'a' | b:'b' bad: TODO: detect *)
  (* a:'a'? *)

  and infix (* or postfix *) ~tokens ~left ~op_name =
    let pos = todo_pos in
    match op_name with
    | "?" -> return ~pos (Option (pos, left))
    | "*" -> return ~pos (Many (pos, left))
    | "+" -> return ~pos (Many1 (pos, left))
    | "|" ->
      expression ~tokens ~ambient_prec:1 >>= fun right ->
      return ~pos (Alt (pos, left, right))
    | ":" -> failwith "TODO"
        (*
        (match left with
      | Identifier (_loc, name) ->
        expression ~tokens ~ambient_prec:2 >>= fun right ->
        return ~
    )
    *)
    | _ -> failwith ("infix TODO: " ^ op_name)

  and expression ~tokens ~ambient_prec =
    let rec go left tokens =
      let pos = location left in
      match Queue.peek tokens with
      | None -> return ~pos left
      | Some token ->
        match token with
        | Operator op_name ->
          if ambient_prec < lbp op_name
          then (
            let _ : token = Queue.dequeue_exn tokens in
            infix ~tokens ~left ~op_name >>= fun expr ->
            go expr tokens
          )
          else return ~pos left
        | Core tm ->
          let _ : token = Queue.dequeue_exn tokens in
          let pos = todo_pos in
          (* TODO: decide precedence, go *)
          return ~pos (Count (pos, left, tm))
        | _ -> return ~pos left
    in

    let token = Queue.dequeue_exn tokens in
    prefix ~tokens token >>= fun left -> go left tokens

  let tokens_to_parser = function
    | [] -> fail "empty input"
    | tokens -> expression ~tokens:(Queue.of_list tokens) ~ambient_prec:0

  let arrow = string "->"

  let t : OptRange.t Core.term Parsers.t -> term Parsers.t =
   fun c_term ->
    fix (fun parser ->
      let token = fix (fun token -> choice
        [ char_lit >>| (fun c -> Atom (CharAtom c))
        ; string_lit >>| (fun str -> Atom (StrAtom str))
        ; char '.' >>| (fun _ -> Atom Dot)
        ; operator >>| (fun op -> Operator op)
        ; keyword >>| (fun kw -> Keyword kw)
        ; string "satisfy" >>= (fun _ ->
          parens
            (lift3
              (fun name _arr tm -> SatisfyTok (name, tm))
              identifier
              arrow
              c_term))
        ; string "fail" >>= (fun _ ->
          c_term >>| fun tm -> FailTok tm)
        ; string "fix" >>= (fun _ ->
          parens
            (lift3
              (fun name _arr toks -> FixTok (name, toks))
              identifier
              arrow
              (many1 token)))
        ; identifier >>| (fun ident -> Ident ident)
        ; c_term >>| (fun tm -> Core tm)
        ; parens parser >>| (fun p -> Parenthesized p)
        ]) <?> "token"
      in

      many token >>= tokens_to_parser
    ) <?> "parser"
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
  let ret = "{foo()}"
  let fix = {|fix (x -> "a" | "b")|}
  let seq = {|a:"a" -> a|}

  let list_parser =
    {|fix (lst ->
        (c:'c' cs:lst -> {cons(x; xs)}) |
        {nil()}
      )
    |}

  let seq2 = {|a:"a" a':"a" b:"b" -> {triple(a; a'; b)}|}
  let seq3 = {|a:'a' a':'a' b:'b' -> {triple(a; a'; b)}|}
  let fix2 = {|fix (x -> "b" | a:"a" x:x -> {pair(a; x)}))|}
  let pair = "a:'a' b:'b' -> {pair(a; b)}"
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
          Direct.parse_direct parser' str
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

    (*
    let%expect_test _ =
      parse_print ret "";
      [%expect {| <parser:8-13>foo()</parser:8-13> |}]
    ;;

    let%expect_test _ =
      parse_print "{foo()}" "";
      [%expect{| foo() |}]
    ;;
    *)

    let%expect_test _ =
      parse_print fix "a";
      [%expect {| <input:0-1>"a"</input:0-1> |}]
    ;;

    (*
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
      parse_print list_parser "...";
      [%expect]
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
    *)

   let parse_print_parser : string -> unit =
     fun parser_str ->
      match ParseUtil.parse_string (ParseParser.t ParseCore.term) parser_str with
      | Error msg -> Caml.print_string ("failed to parse parser desc: " ^ msg)
      | Ok parser -> Fmt.pr "%a\n" pp_plain parser
   ;;

   (*
   let%expect_test _ =
     parse_print_parser char_count;
     [%expect{| 'c'{{2}} |}]
*)

   let%expect_test _ =
     parse_print_parser "F++";
     [%expect{| failed to parse parser desc: : end_of_input |}]

     (*
   let%expect_test _ =
     parse_print_parser "(F+)+";
     [%expect{| (F+)+ |}]
*)

   let%expect_test _ =
     parse_print_parser dot;
     [%expect{| . |}]

     (*
   let%expect_test _ =
     parse_print_parser "let a = b | c in d?";
     [%expect{|
       let a = b | c in
       d? |}]
*)

   let%expect_test _ =
     parse_print_parser str_star;
     [%expect{| "str"* |}]

   let%expect_test _ =
     parse_print_parser "'a' | 'b' | 'c'";
     [%expect{| 'a' | 'b' | 'c' |}]

     (*
   let%expect_test _ =
     parse_print_parser "(. -> Q) | .";
     [%expect{| (. -> Q) | . |}]

   let%expect_test _ =
     parse_print_parser list_parser;
     [%expect{|

       fix (lst -> (c:'c' lst:lst -> cons(x; xs)) | {nil()}) |}]
    *)
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
      if String.(str' = str)
      then Ok
      else
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = pp_str t' in
          PropertyResult.check String.(str'' = str') (Fmt.str {|"%s" <> "%s"|} str'' str')
  ;;
end
