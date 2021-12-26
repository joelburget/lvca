open Base
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Lvca_del
module Format = Stdlib.Format

module Lang =
[%lvca.abstract_syntax_module
{|
char : *
string : *
list : * -> *
core_term : *
option : * -> *

term :=
  | Any_char()
  | Char(char)
  | String(string)
  | Satisfy(string; core_term)
  | Fail(core_term)
  | Option(term)
  | Count(term; core_term)
  | Many(term)
  | Many1(term)
  | Choice(list term)
  | Sequence(sequence)
  | Let(term; term. term)
  | Fix(term. term)
  ;

sequence :=
  | Empty_sequence(core_term)
  | Binding(term; term. sequence)
  | Non_binding(term; sequence)
  ;
|}
, { char = "Primitive.Char"
  ; string = "Primitive.String"
  ; list = "List_model"
  ; core_term = "Core.Term"
  ; option = "Option_model.Option"
  }]

module Sequence = Lang.Sequence

module Term = struct
  include Lang.Term
  include Nominal.Convertible.Extend (Lang.Term)

  module Prec = struct
    let atom = 6
    let quantifier = 5
    let app = 4
    let eq = 3
    let arr = 2
    let alt = 1
  end

  let rec sequence_to_list = function
    | Lang.Sequence.Empty_sequence (_, body) -> [], body
    | Binding (_, tm, ({ name; _ }, seq)) ->
      let rest, body = sequence_to_list seq in
      (Some name, tm) :: rest, body
    | Non_binding (_, tm, seq) ->
      let rest, body = sequence_to_list seq in
      (None, tm) :: rest, body
  ;;

  let pp ppf p =
    let core = Core.Term.pp_concrete in
    let fmt, pf = Fmt.(fmt, pf) in
    let with_parens ~ambient_prec ~prec pp =
      if ambient_prec > prec then Fmt.parens pp else pp
    in
    let rec go ambient_prec ppf p =
      let loc = info p in
      Provenance.open_stag ppf loc;
      let formatter, prec =
        match p with
        | Any_char _ -> fmt ".", Prec.atom
        | Char (_, (_, char)) ->
          (fun ppf -> Fmt.(quote ~mark:"'" char) ppf char), Prec.atom
        | String (_, (_, str)) -> (fun ppf -> Fmt.(quote string) ppf str), Prec.atom
        | Satisfy (_, (_, name), tm) ->
          (fun ppf -> pf ppf "@[<2>satisfy (@[%s -> {%a}@])@]" name core tm), Prec.app
        | Let (_, named, ({ name; _ }, body)) ->
          ( (fun ppf ->
              pf ppf "@[<v>@[<2>let %s =@ @[%a@] in@]@ %a@]" name (go 0) named (go 0) body)
          , Prec.atom )
        | Fail (_, tm) ->
          let f ppf =
            match tm with
            | Primitive (_, (_, String msg)) -> pf ppf {|@[<2>fail "%s"@]|} msg
            | _ -> pf ppf "@[<2>fail {%a}@]" core tm
          in
          f, Prec.app
        | Count (_, p, tm) ->
          let f ppf =
            match tm with
            | Primitive (_, (_, Integer n)) ->
              pf ppf "@[<hv>%a%s@]" (go Prec.quantifier) p (Z.to_string n)
            | _ -> pf ppf "@[<hv>%a{%a}@]" (go Prec.quantifier) p core tm
          in
          f, Prec.quantifier
        | Option (_, p) ->
          (fun ppf -> pf ppf "%a?" (go Prec.quantifier) p), Prec.quantifier
        | Many (_, p) -> (fun ppf -> pf ppf "%a*" (go Prec.quantifier) p), Prec.quantifier
        | Many1 (_, p) ->
          (fun ppf -> pf ppf "%a+" (go Prec.quantifier) p), Prec.quantifier
        | Choice (_, branches) ->
          let initial_bar =
            Format.pp_print_custom_break ~fits:("", 0, "") ~breaks:("", 2, "| ")
          in
          let branches = List_model.to_list branches in
          let formatter ppf =
            pf
              ppf
              "@[<hv 0>%t%a@,@]"
              initial_bar
              Fmt.(list ~sep:(any "@;<1 2>| ") (go Prec.alt))
              branches
          in
          formatter, Prec.alt
        | Fix (_, ({ name; _ }, p)) ->
          (fun ppf -> pf ppf "@[<2>fix@ (@[%s -> %a@])@]" name (go 0) p), Prec.app
        | Sequence (_, seq) ->
          let binders, body = sequence_to_list seq in
          let formatter ppf =
            pf
              ppf
              "@[<hv 2>@[<hv>%a@] ->@ {%a}@]"
              Fmt.(list ~sep:(any "@ ") binder)
              binders
              core
              body
          in
          formatter, Prec.arr
        | Term_var (_, name) -> (fun ppf -> Fmt.string ppf name), Prec.atom
      in
      with_parens ~ambient_prec ~prec (fun ppf () -> formatter ppf) ppf ();
      Provenance.close_stag ppf loc
    and binder ppf (opt_name, p) =
      match opt_name with
      | None -> (go 0) ppf p
      | Some name -> pf ppf "%s=%a" name (go Prec.eq) p
    in
    go 0 ppf p
  ;;
end

module Parse = struct
  open Lvca_parsing
  open C_comment_parser
  open Lang.Term

  module Parse_helpers =
  [%lvca.abstract_syntax_module
  {|
    count : *
    term : *
    single_var : *

    quantifier :=
      | Q_count(count)
      | Q_option()
      | Q_many()
      | Q_many1()
      ;

    sequence_elem :=
      | Bound(single_var; term)
      | Unbound(term)
      ;
    |}
  , { count = "Core.Term"; term = "Lang.Term"; single_var = "Single_var" }]

  open Parse_helpers.Quantifier
  open Parse_helpers.Sequence_elem

  let rec apply_quantifiers tm = function
    | [] -> tm
    | q :: qs ->
      let tm =
        match q with
        | Q_count (range, c_tm) -> Count (range, tm, c_tm)
        | Q_option range -> Option (range, tm)
        | Q_many range -> Many (range, tm)
        | Q_many1 range -> Many1 (range, tm)
      in
      apply_quantifiers tm qs
  ;;

  let rec mk_sequence binders rhs =
    match binders with
    | [] -> Lang.Sequence.Empty_sequence (Core.Term.info rhs, rhs)
    | Bound (info, name, tm) :: binders ->
      Binding (info, tm, (name, mk_sequence binders rhs))
    | Unbound (info, tm) :: binders -> Non_binding (info, tm, mk_sequence binders rhs)
  ;;

  let arrow = string "->"
  let attach_pos' p = p >>~ fun range a -> Provenance.of_range range, a
  let make0, make1, make2 = Provenance.(make0, make1, make2)
  let input = Provenance.Parse_input.Buffer_name "input"
  let identifier = lower_identifier String.Set.empty

  let t c_term =
    fix (fun parser ->
        (* prec 6 *)
        let parse_atom =
          choice
            [ make1 mk_Char (attach_pos' char_lit : (Provenance.t * char) Lvca_parsing.t)
            ; make1 mk_String (attach_pos' string_lit)
            ; make0 mk_Any_char (char '.')
            ; make1 mk_Term_var identifier
            ; parens parser
            ]
        in
        (* prec 5 *)
        let parse_quantified =
          let%bind _, tm = parse_atom in
          let quantifier =
            choice
              [ make1 mk_Q_count (braces c_term)
              ; make1
                  mk_Q_count
                  (integer_lit
                  >>~ fun range i ->
                  let i = Z.of_string i in
                  let info = Provenance.of_range range in
                  Core.Term_syntax.Term.Primitive (info, (info, Integer i)))
              ; make0 mk_Q_option (char '?')
              ; make0 mk_Q_many (char '*')
              ; make0 mk_Q_many1 (char '+')
              ]
          in
          many quantifier >>| apply_quantifiers tm
        in
        (* prec 4 *)
        let parse_app =
          choice
            [ make2
                (fun ~info _ body -> mk_Fix ~info body)
                (string "fix")
                (parens
                   (lift3
                      (fun (_, v) _arr (_, body) -> v, body)
                      (make1 Single_var.mk identifier)
                      arrow
                      parser))
            ; make2
                (fun ~info _ (name, tm) -> mk_Satisfy ~info name tm)
                (string "satisfy")
                (parens
                   (lift3
                      (fun (_, name) _arr (_, tm) -> name, tm)
                      (identifier
                      >>~ fun range ident -> Provenance.of_range ~input range, ident)
                      arrow
                      (braces c_term)))
            ; make2
                (fun ~info _ tm -> mk_Fail ~info tm)
                (string "fail")
                (choice
                   [ braces c_term
                   ; (string_lit
                     >>~ fun range str ->
                     let info = Provenance.of_range range in
                     Core.Term_syntax.Term.Primitive (info, (info, String str)))
                   ])
            ]
          <|> parse_quantified
        in
        (* prec 3 *)
        let parse_eq =
          choice
            [ make3
                (fun ~info ident _eq tm ->
                  let info = Provenance.of_range info in
                  mk_Bound ~info ident tm)
                (make1 Single_var.mk identifier)
                (char '=')
                parse_app
            ; make1 mk_Unbound parse_app
            ]
        in
        (* prec 2 *)
        let parse_arr =
          choice
            [ make3
                (fun ~info bindings _arr rhs ->
                  let info = Provenance.of_range info in
                  mk_Sequence ~info (mk_sequence bindings rhs))
                (many parse_eq)
                arrow
                (braces c_term)
            ; parse_app
            ]
        in
        (* prec 1 *)
        let parse_alt =
          let f ~info tm tms =
            match tms with
            | [] -> tm
            | _ -> mk_Choice ~info (List_model.of_list (tm :: tms))
          in
          make2 f parse_arr (many (char '|' *> parse_arr))
        in
        (* prec 0 *)
        let parse_let =
          choice
            [ make6
                (fun ~info _let name _eq tm _in rhs ->
                  let info = Provenance.of_range info in
                  mk_Let ~info tm (name, rhs))
                (string "let")
                (make1 Single_var.mk identifier)
                (char '=')
                parser
                (string "in")
                parser
            ; parse_alt
            ]
        in
        parse_let)
    <?> "parser"
  ;;
end

module Evaluate = struct
  type term_ctx = Core.Value.t String.Map.t
  type parser_ctx = Lang.Term.t String.Map.t
  type parse_result = (Core.Value.t, string * Core.Term.t option) Result.t

  type trace_snapshot =
    { success : bool
    ; pre_pos : int
    ; post_pos : int
    ; parser : Lang.Term.t
    ; term_ctx : term_ctx
    ; parser_ctx : parser_ctx
    ; snapshots : trace_snapshot list
    }

  type t =
    { run :
        of_term:(Lang.Term.t -> t)
        -> term_ctx:term_ctx
        -> parser_ctx:parser_ctx
        -> pos:int
        -> string
        -> int * trace_snapshot list * parse_result
    }

  type toplevel_result =
    { didnt_consume_msg : string option
    ; snapshot : trace_snapshot
    ; result : parse_result
    }

  let mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ?(snapshots = []) pre_pos post_pos
    =
    { success = Result.is_ok result
    ; pre_pos
    ; post_pos
    ; parser
    ; term_ctx
    ; parser_ctx
    ; snapshots
    }
  ;;

  let input = Provenance.Parse_input.Buffer_name "input"

  let mk_Some v =
    let info = Provenance.calculated_here [%here] [ Core.Value.info v ] in
    Core.Value_syntax.(
      Value.Operator
        ( info
        , (info, "Some")
        , List_model.of_list
            [ Operator_scope.Operator_scope (info, List_model.Nil info, v) ] ))
  ;;

  let mk_None =
    let info = Provenance.of_here [%here] in
    Core.Value_syntax.Value.Operator (info, (info, "None"), List_model.Nil info)
  ;;

  let mk_Char pos c =
    let info = Provenance.of_range (Opt_range.mk pos (pos + 1)) in
    Core.Value_syntax.Value.Primitive (info, (info, Char c))
  ;;

  let mk_Error msg = Error (msg, None)

  let context_free go =
    { run =
        (fun ~of_term:_ ~term_ctx:_ ~parser_ctx:_ ~pos str ->
          match go pos str with
          | Ok (pos, result) -> pos, [], Ok result
          | Error msg -> pos, [], mk_Error msg)
    }
  ;;

  let anychar =
    context_free (fun pos str ->
        if String.length str > pos
        then Ok (pos + 1, mk_Char pos str.[pos])
        else Error "expected: .")
  ;;

  let char c =
    context_free (fun pos str ->
        if String.length str > pos && Char.(str.[pos] = c)
        then Ok (pos + 1, mk_Char pos c)
        else Error (Printf.sprintf "expected: char '%c'" c))
  ;;

  let string prefix =
    context_free (fun pos str ->
        match str |> String.subo ~pos |> String.chop_prefix ~prefix with
        | None -> Error (Printf.sprintf {|expected: string "%s"|} prefix)
        | Some _str' ->
          let pos' = pos + String.length prefix in
          let rng = Opt_range.mk pos pos' in
          let info = Provenance.of_range ~input rng in
          Ok (pos', Core.Value_syntax.Value.Primitive (info, (info, String prefix))))
  ;;

  let satisfy name core_term =
    { run =
        (fun ~of_term:_ ~term_ctx ~parser_ctx:_ ~pos str ->
          let err_msg =
            mk_Error
              (Fmt.str
                 {|expected: satisfy (%s -> {%a})|}
                 name
                 Core.Term.pp_concrete
                 core_term)
          in
          if pos >= String.length str
          then pos, [], err_msg
          else (
            let c = str.[pos] in
            let info = Provenance.of_range ~input (Opt_range.mk pos (pos + 1)) in
            let tm =
              Core.Term_syntax.Term.Let
                ( info
                , Primitive (info, (info, Char c))
                , Option_model.Option.None info
                , (Single_var.{ name; info }, core_term) )
            in
            match Core.eval_in_ctx term_ctx tm with
            | Ok (Operator (_, (_, "true"), List_model.Nil _)) ->
              pos + 1, [], Ok (mk_Char pos c)
            | Ok (Operator (_, (_, "false"), List_model.Nil _)) | Ok _ ->
              pos, [], err_msg (* TODO: throw harder error? (type error) *)
            | Error err -> pos, [], Error (Tuple2.map2 ~f:(fun tm -> Some tm) err)))
    }
  ;;

  let fail c_tm =
    { run =
        (fun ~of_term:_ ~term_ctx ~parser_ctx:_ ~pos _str ->
          match Core.eval_in_ctx term_ctx c_tm with
          | Ok (Primitive (_, (_, String msg))) -> pos, [], mk_Error msg
          | _ -> failwith "TODO: fail")
    }
  ;;

  let let_ name parser body =
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          let parser_ctx = Map.set parser_ctx ~key:name ~data:parser in
          let pos0 = pos in
          let pos1, snapshots, result =
            (of_term body).run ~of_term ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~result ~parser:body ~term_ctx ~parser_ctx ~snapshots pos0 pos1
          in
          pos1, [ snapshot ], result)
    }
  ;;

  let option parser =
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          let pos0 = pos in
          let pos1, snapshots, result =
            (of_term parser).run ~of_term ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos1
          in
          let result = match result with Ok tm -> mk_Some tm | Error _ -> mk_None in
          pos1, [ snapshot ], Ok result)
    }
  ;;

  let mk_list lst =
    let open Core.Value_syntax in
    let provs = List.map lst ~f:Core.Value.info in
    let info = Provenance.calculated_here [%here] provs in
    let lst =
      List.map lst ~f:(fun v ->
          Operator_scope.Operator_scope (info, List_model.Nil info, v))
    in
    Value.Operator (info, (info, "list"), List_model.of_list lst)
  ;;

  let count n_tm parser =
    let rec go ~of_term ~term_ctx ~parser_ctx ~pos n str =
      match n with
      | 0 -> Ok []
      | _ ->
        let pos0 = pos in
        let pos, snapshots, head_result =
          (of_term parser).run ~of_term ~term_ctx ~parser_ctx ~pos str
        in
        let snapshot =
          mk_snapshot
            ~result:head_result
            ~parser
            ~term_ctx
            ~parser_ctx
            ~snapshots
            pos0
            pos
        in
        (match head_result with
        | Error msg -> Error (pos, snapshot :: snapshots, msg)
        | Ok tm ->
          go ~of_term ~term_ctx ~parser_ctx (n - 1) ~pos str
          |> Result.map ~f:(List.cons (pos, snapshot, tm)))
    in
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          match Core.eval_in_ctx term_ctx n_tm with
          | Ok (Primitive (_, (_, Integer n))) ->
            let n = Z.to_int n (* TODO: may raise Overflow *) in
            let results = go ~of_term ~term_ctx ~parser_ctx ~pos n str in
            let pos, rev_snapshots, result =
              match results with
              | Ok results ->
                let poss, snapshots, tms = List.unzip3 results in
                let pos = match List.last poss with None -> pos | Some pos -> pos in
                pos, snapshots, Ok tms
              | Error (pos, rev_snapshots, msg) -> pos, rev_snapshots, Error msg
            in
            pos, List.rev rev_snapshots, Result.map ~f:mk_list result
          | Ok _ | Error _ -> failwith "TODO: count error")
    }
  ;;

  let rec go_many ~of_term ~term_ctx ~parser_ctx ~pos parser str =
    let pos0 = pos in
    let pos, snapshots, head_result =
      (of_term parser).run ~of_term ~term_ctx ~parser_ctx ~pos str
    in
    let snapshot =
      mk_snapshot ~result:head_result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos
    in
    match head_result with
    | Error _ -> pos, [ snapshot ], Ok []
    | Ok tm ->
      let pos, snapshots, result =
        go_many ~of_term ~term_ctx ~parser_ctx ~pos parser str
      in
      pos, snapshot :: snapshots, Result.map result ~f:(List.cons tm)
  ;;

  let many parser =
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          let pos, snapshots, result =
            go_many ~of_term ~term_ctx ~parser_ctx ~pos parser str
          in
          let result = Result.map result ~f:mk_list in
          pos, snapshots, result)
    }
  ;;

  let many1 parser =
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          let pos, snapshots, result =
            go_many ~of_term ~term_ctx ~parser_ctx ~pos parser str
          in
          let result =
            match result with
            | Ok [] -> mk_Error "many1: empty list"
            | Ok tms -> Ok (mk_list tms)
            | Error msg -> Error msg
          in
          pos, snapshots, result)
    }
  ;;

  let choice ps =
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          let snapshot_queue = Queue.create () in
          let match_opt =
            List.find_map ps ~f:(fun parser ->
                let pos', snapshots, result =
                  (of_term parser).run ~of_term ~term_ctx ~parser_ctx ~pos str
                in
                let snapshot =
                  mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos pos'
                in
                Queue.enqueue snapshot_queue snapshot;
                match result with Error _ -> None | Ok _ -> Some (pos', result))
          in
          match match_opt with
          | None ->
            pos, Queue.to_list snapshot_queue, Error ("choice: no match found", None)
          | Some (pos, result) -> pos, Queue.to_list snapshot_queue, result)
    }
  ;;

  let fix name parser =
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          let pos0 = pos in
          let parser_ctx = Map.set parser_ctx ~key:name ~data:parser in
          let pos1, snapshots, result =
            (of_term parser).run ~of_term ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos1
          in
          pos1, [ snapshot ], result)
    }
  ;;

  let sequence named_ps tm =
    let names, ps =
      named_ps |> List.map ~f:(fun (name_opt, p) -> name_opt, p) |> List.unzip
    in
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          (* Run through each subparser. We end up with
           * - pos: The position the last parser ended up
           * - results: (snapshop, result) option list
           *)
          let (pos, _), results =
            List.fold_map ps ~init:(pos, true) ~f:(fun (pos, continue) parser ->
                let { run } = of_term parser in
                if continue
                then (
                  let pre_pos = pos in
                  let pos, snapshots, result =
                    run ~of_term ~term_ctx ~parser_ctx ~pos str
                  in
                  let snapshot =
                    mk_snapshot
                      ~result
                      ~parser
                      ~term_ctx
                      ~parser_ctx
                      ~snapshots
                      pre_pos
                      pos
                  in
                  let continue = match result with Ok _ -> true | Error _ -> false in
                  (pos, continue), Some (snapshot, result))
                else (pos, false), None)
          in
          (* Get the list of subparser results. If there was an error, the last one will
           * be Error, otherwise all will be Ok. If there wasn't an error there will
           * always be n Ok results.
           *)
          let snapshots, results = results |> List.filter_map ~f:Fn.id |> List.unzip in
          match Result.all results with
          | Error msg -> pos, snapshots, Error msg
          | Ok xs ->
            (match List.zip names xs with
            | Unequal_lengths ->
              failwith
                (Printf.sprintf
                   "TODO: sequence Unequal_lengths (%n vs %n)"
                   (List.length names)
                   (List.length xs))
            | Ok name_vals ->
              let term_ctx =
                List.fold_result name_vals ~init:term_ctx ~f:(fun ctx (key_opt, v) ->
                    match key_opt with
                    | None -> Ok ctx
                    | Some key ->
                      v
                      |> Core.Term.of_value
                      |> Core.eval
                      |> Result.map ~f:(fun data -> Map.set ctx ~key ~data))
              in
              (match term_ctx with
              | Error (msg, tm) -> pos, snapshots, Error (msg, Some tm)
              | Ok term_ctx ->
                let result =
                  Core.eval_in_ctx term_ctx tm
                  |> Result.map_error ~f:(Tuple2.map2 ~f:(fun tm -> Some tm))
                in
                pos, snapshots, result)))
    }
  ;;

  let identifier name =
    { run =
        (fun ~of_term ~term_ctx ~parser_ctx ~pos str ->
          match Map.find parser_ctx name with
          | None ->
            ( pos
            , []
            , mk_Error (Printf.sprintf {|Identifer not found in context: "%s"|} name) )
          | Some parser ->
            let pos0 = pos in
            let pos1, snapshots, result =
              (of_term parser).run ~of_term ~term_ctx ~parser_ctx ~pos str
            in
            let snapshot =
              mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos1
            in
            pos1, [ snapshot ], result)
    }
  ;;

  let rec translate_sequence tms = function
    | Lang.Sequence.Empty_sequence (_, c_term) -> sequence tms c_term
    | Binding (_, tm, (Single_var.{ name; _ }, seq)) ->
      translate_sequence (tms @ [ Some name, tm ]) seq
    | Non_binding (_, tm, seq) -> translate_sequence (tms @ [ None, tm ]) seq
  ;;

  let of_term : Lang.Term.t -> t = function
    | Any_char _ -> anychar
    | Char (_, (_, c)) -> char c
    | String (_, (_, prefix)) -> string prefix
    | Satisfy (_, (_, name), core_term) -> satisfy name core_term
    | Fail (_, tm) -> fail tm
    | Let (_, p, (Single_var.{ name; _ }, body)) -> let_ name p body
    | Option (_, t) -> option t
    | Count (_, t, n) -> count n t
    | Many (_, t) -> many t
    | Many1 (_, t) -> many1 t
    | Choice (_, ps) -> choice (List_model.to_list ps)
    | Fix (_, (Single_var.{ name; _ }, p)) -> fix name p
    | Sequence (_, seq) -> translate_sequence [] seq
    | Term_var (_, name) -> identifier name
  ;;

  let parse_direct
      :  ?term_ctx:term_ctx -> ?parser_ctx:parser_ctx -> Lang.Term.t -> string
      -> toplevel_result
    =
   fun ?(term_ctx = String.Map.empty) ?(parser_ctx = String.Map.empty) parser str ->
    let { run } = of_term parser in
    let pos, snapshots, result = run ~of_term ~term_ctx ~parser_ctx ~pos:0 str in
    (* Some str if the parser succeeded but didn't consume the entire input,
       otherwise None *)
    let didnt_consume_msg =
      match result with
      | _ when pos = String.length str -> None
      | Ok _ ->
        let str' = String.subo ~pos str in
        Some
          (Printf.sprintf
             {|The parser succeeded but didn't consume the entire input. Left over: "%s"|}
             (if String.length str' > 50 then String.prefix str' 47 ^ "..." else str'))
      | Error _ -> None
    in
    let snapshot = mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots 0 pos in
    { didnt_consume_msg; snapshot; result }
 ;;
end

module Test_parsers = struct
  let dot = {|.|}
  let str = {|"str"|}
  let str_star = {|"str"*|}
  let str_plus = {|"str"+|}
  let choice = {|choice ("str" | "foo")|}

  let sat_parser =
    {|satisfy (x -> {match x with {
    | 'c' -> {true()}
    | _ -> {false()}
  }})
  |}
  ;;

  let let_var = {|let x = "str" in x|}
  let fail = {|fail {{"reason"}}|}
  let char_opt = "'c'?"
  let ret = "{{foo()}}"
  let fix = {|fix (x -> choice ("a" | "b"))|}
  let seq = {|a="a" -> a|}

  let list_parser =
    {|fix (lst -> choice (
    | c='c' cs=lst -> {{cons(c; cs)}}
      | {{nil()}}
      ))
    |}
  ;;

  let seq2 = {|a="a" a'="a" b="b" -> {{triple(a; a'; b)}}|}
  let seq3 = {|a='a' a'='a' b='b' -> {{triple(a; a'; b)}}|}

  let fix2 =
    {|fix (x -> choice (
    | a="a" x=x -> {{pair(a; x)}}
    | "b" -> {{"b"}}
  ))|}
  ;;

  let pair = "a='a' b='b' -> {{pair(a; b)}}"
  let pair2 = "a=. b=. -> {{pair(a; b)}}"

  (* XXX: "fix need for parens" *)
  let fix3 =
    {|let char = satisfy (c -> {is_alpha c}) in
let digit = satisfy (c -> {is_digit c}) in
let name = (chars=char+ -> {let str = string_of_chars chars in {var(str)} }) in
let literal = (chars=digit+ -> {{literal(chars)}}) in
let atom = choice(name | literal) in
fix (expr -> choice (
  | atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
  | atom=atom -> {{atom}}
))|}
  ;;
end

let parse_parser = Lvca_parsing.parse_string (Parse.t Core.Parse.term)

let%test_module "Parsing" =
  (module struct
    open Test_parsers

    let margin = Stdlib.Format.(pp_get_margin std_formatter ())
    let () = Stdlib.Format.(pp_set_margin std_formatter 80)

    let parse_print_parser : ?width:int -> string -> unit =
     fun ?width parser_str ->
      match
        Lvca_parsing.(parse_string (whitespace *> Parse.t Core.Parse.term) parser_str)
      with
      | Error msg -> Stdio.print_string ("failed to parse parser desc: " ^ msg)
      | Ok parser ->
        let pre_geom =
          Option.map width ~f:(fun n ->
              let geom = Format.get_geometry () in
              Format.set_margin n;
              geom)
        in
        Fmt.pr "%a\n" Term.pp parser;
        (match pre_geom with
        | None -> ()
        | Some { max_indent; margin } -> Format.set_geometry ~max_indent ~margin)
   ;;

    (*
    let%expect_test _ = parse_print
      {|chars=alpha+ -> { let str = string_of_chars chars in {var(str)} }|}
      "ab";
      (* TODO: should give comprehensible error *)
      [%expect]
    *)

    let%expect_test _ =
      parse_print_parser
        {|let atom = name | literal in
fix
(expr ->
(atom=atom ' '* '+'
' '*
expr=expr -> {{add(atom;
expr)}} | atom=atom -> {atom}))|};
      [%expect
        {|
       let atom = name | literal in
       fix
         (expr ->
                    | atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
                    | atom=atom -> {atom}
                  ) |}]
    ;;

    let%expect_test _ =
      (* TODO: decide about leading bars *)
      parse_print_parser
        {|let char = satisfy (c -> {is_alpha c}) in
let digit = satisfy (c -> {is_digit c}) in
let name = (chars=char+ -> {let str = string_of_chars chars in {var(str)} }) in
let literal = (chars=digit+ -> {{literal(chars)}}) in
let atom = (name | literal) in
fix (expr ->
  ( atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
  | atom=atom -> {{atom}}
))|};
      [%expect
        {|
       let char = satisfy (c -> {is_alpha c}) in
       let digit = satisfy (c -> {is_digit c}) in
       let name = chars=char+ -> {let str = string_of_chars chars in {var(str)}} in
       let literal = chars=digit+ -> {{literal(chars)}} in
       let atom = name | literal in
       fix
         (expr ->
                    | atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
                    | atom=atom -> {{atom}}
                  ) |}]
    ;;

    let%expect_test _ =
      parse_print_parser "'c'2";
      [%expect {| 'c'2 |}]
    ;;

    let%expect_test _ =
      parse_print_parser "'c'{{2}}";
      [%expect {| 'c'2 |}]
    ;;

    let%expect_test _ =
      parse_print_parser "F++";
      [%expect {| F++ |}]
    ;;

    let%expect_test _ =
      parse_print_parser "F+?";
      [%expect {| F+? |}]
    ;;

    let%expect_test _ =
      parse_print_parser "(F+)+";
      [%expect {| F++ |}]
    ;;

    let%expect_test _ =
      parse_print_parser dot;
      [%expect {| . |}]
    ;;

    let%expect_test _ =
      parse_print_parser str_star;
      [%expect {| "str"* |}]
    ;;

    let%expect_test _ =
      parse_print_parser "('a' | 'b' | 'c')";
      [%expect {| 'a' | 'b' | 'c' |}]
    ;;

    let%expect_test _ =
      parse_print_parser "(. -> {Q} | .)";
      [%expect {| . -> {Q} | . |}]
    ;;

    let list_parser = {|fix (lst -> c='c' cs=lst -> {{cons(c; cs)}} | -> {{nil()}})|}

    let%expect_test _ =
      parse_print_parser list_parser;
      [%expect {| fix (lst -> c='c' cs=lst -> {{cons(c; cs)}} |  -> {{nil()}}) |}]
    ;;

    let%expect_test _ =
      parse_print_parser {|. ' '* '+' ' '* . -> {{"parsed an addition"}}|};
      [%expect {| . ' '* '+' ' '* . -> {{"parsed an addition"}} |}]
    ;;

    let%expect_test _ =
      parse_print_parser "a=. ' '* '+' ' '* b=. -> {{plus(a; b)}}";
      [%expect {| a=. ' '* '+' ' '* b=. -> {{plus(a; b)}} |}]
    ;;

    let%expect_test _ =
      parse_print_parser {|fail {{"some reason for failing"}}|};
      [%expect {| fail "some reason for failing" |}]
    ;;

    let%expect_test _ =
      parse_print_parser {|fail "some reason for failing"|};
      [%expect {| fail "some reason for failing" |}]
    ;;

    let%expect_test _ =
      parse_print_parser
        {|
     satisfy (c -> {match c with {
  | 'c' -> {true()}
  | _ -> {false()}
     }})|};
      [%expect {| satisfy (c -> {match c with { 'c' -> {true()} | _ -> {false()} }}) |}]
    ;;

    let%expect_test _ =
      parse_print_parser
        {|
let atom = name | literal in
fix (expr -> (
        a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
  | a=atom -> {a}
))|};
      [%expect
        {|
       let atom = name | literal in
       fix (expr -> a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}} | a=atom -> {a}) |}]
    ;;

    (* TODO
    let%expect_test _ =
      parse_print_parser
        {|
let atom = (name | literal) in
fix (expr -> (
        | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
  | a=atom -> a
))|};
      [%expect
        {|
       let atom = (name | literal) in
       fix
         (expr -> (
                    | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
                    | a=atom -> a
                  )) |}]
    ;;
    *)

    let%expect_test _ =
      parse_print_parser "name | literal";
      [%expect {| name | literal |}]
    ;;

    let%expect_test _ =
      parse_print_parser ~width:12 "name | literal";
      [%expect {|
        | name
        | literal |}]
    ;;

    let%expect_test _ =
      parse_print_parser "fix (x -> (e=expr -> {e} | a=atom -> {a}))";
      [%expect {| fix (x -> e=expr -> {e} | a=atom -> {a}) |}]
    ;;

    let () = Stdlib.Format.(pp_set_margin std_formatter margin)
  end)
;;

module Properties = struct
  open Property_result

  let parse parser_str = Lvca_parsing.parse_string (Parse.t Core.Parse.term) parser_str
  let pp_str p = Fmt.to_to_string Term.pp p

  let string_round_trip1 t =
    match t |> pp_str |> parse with
    | Ok t' ->
      Property_result.check
        (Term.equivalent t t')
        (Fmt.str "%a <> %a" Term.pp t' Term.pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (pp_str t) msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = pp_str t in
      if String.(str' = str)
      then Ok
      else (
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = pp_str t' in
          Property_result.check
            String.(str'' = str')
            (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end
