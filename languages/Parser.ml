open Base
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Stdio
module Format = Stdlib.Format

type 'info c_term = 'info Lvca_core.Term.t
type 'info n_term = 'info Nominal.Term.t

let eval_primitive _eval_ctx _eval_ctx' _ctx _tm _name _args =
  Error
    ( "no primitive evaluation"
    , Lvca_core.Term.Term
        (Nominal.Term.Primitive (Source_ranges.empty, String "TODO: make this unnecessary"))
    )
;;

type 'info t =
  (* primitive parsers *)
  | AnyChar of 'info
  | Char of 'info * char
  | String of 'info * string
  | Satisfy of 'info * string * 'info c_term
  | Fail of 'info * 'info c_term
  (* combinators *)
  | Option of 'info * 'info t
  | Count of 'info * 'info t * 'info c_term
  | Many of 'info * 'info t
  | Many1 of 'info * 'info t
  | Choice of 'info * 'info t list
  | Sequence of 'info * 'info binder list * 'info c_term
  (* language stuff *)
  | Let of 'info * string * 'info t * 'info t
  | Fix of 'info * string * 'info t
  | Identifier of 'info * string

and 'info binder = Binder of string option * 'info t

let rec equal loc_eq t1 t2 =
  match t1, t2 with
  | AnyChar l1, AnyChar l2 -> loc_eq l1 l2
  | Char (l1, c1), Char (l2, c2) -> loc_eq l1 l2 && Char.(c1 = c2)
  | String (l1, s1), String (l2, s2) -> loc_eq l1 l2 && String.(s1 = s2)
  | Option (l1, t1), Option (l2, t2) -> loc_eq l1 l2 && equal loc_eq t1 t2
  | Many (l1, t1), Many (l2, t2) -> loc_eq l1 l2 && equal loc_eq t1 t2
  | Many1 (l1, t1), Many1 (l2, t2) -> loc_eq l1 l2 && equal loc_eq t1 t2
  | Choice (l1, ts1), Choice (l2, ts2) ->
    loc_eq l1 l2 && List.equal (equal loc_eq) ts1 ts2
  | Let (l1, nm1, x1, y1), Let (l2, nm2, x2, y2) ->
    loc_eq l1 l2 && String.(nm1 = nm2) && equal loc_eq x1 x2 && equal loc_eq y1 y2
  | Fix (l1, nm1, x1), Fix (l2, nm2, x2) ->
    loc_eq l1 l2 && String.(nm1 = nm2) && equal loc_eq x1 x2
  | Identifier (l1, nm1), Identifier (l2, nm2) -> loc_eq l1 l2 && String.(nm1 = nm2)
  (* XXX: implement these *)
  (* | Satisfy of 'info * string * 'info c_term *)
  (* | Fail of 'info * 'info c_term *)
  (* | Count of 'info * 'info t * 'info c_term *)
  (* | Sequence of 'info * 'info binder list * 'info c_term *)
  | _, _ -> false
;;

let info = function
  | AnyChar i
  | Char (i, _)
  | String (i, _)
  | Satisfy (i, _, _)
  | Fail (i, _)
  | Let (i, _, _, _)
  | Option (i, _)
  | Count (i, _, _)
  | Many (i, _)
  | Many1 (i, _)
  | Choice (i, _)
  | Fix (i, _, _)
  | Sequence (i, _, _)
  | Identifier (i, _) ->
    i
;;

let rec map_info ~f =
  let cf = Lvca_core.Term.map_info ~f in
  let map_binder (Binder (name, p)) = Binder (name, map_info ~f p) in
  function
  | AnyChar loc -> AnyChar (f loc)
  | Char (loc, c) -> Char (f loc, c)
  | String (loc, s) -> String (f loc, s)
  | Satisfy (loc, s, tm) -> Satisfy (f loc, s, cf tm)
  | Fail (loc, tm) -> Fail (f loc, cf tm)
  | Let (loc, s, p1, p2) -> Let (f loc, s, map_info ~f p1, map_info ~f p2)
  | Option (loc, p) -> Option (f loc, map_info ~f p)
  | Count (loc, p, tm) -> Count (f loc, map_info ~f p, cf tm)
  | Many (loc, p) -> Many (f loc, map_info ~f p)
  | Many1 (loc, p) -> Many1 (f loc, map_info ~f p)
  | Choice (loc, ps) -> Choice (f loc, List.map ps ~f:(map_info ~f))
  | Fix (loc, s, p) -> Fix (f loc, s, map_info ~f p)
  | Sequence (loc, ps, p) -> Sequence (f loc, List.map ps ~f:map_binder, cf p)
  | Identifier (loc, s) -> Identifier (f loc, s)
;;

let erase = map_info ~f:(fun _ -> ())

module Prec = struct
  let atom = 6
  let quantifier = 5
  let eq = 4
  let app = 3
  let alt = 1
  let arr = 1
end

let pp_generic ~open_loc ~close_loc ppf p =
  let core = Lvca_core.Term.pp in
  let fmt, pf = Fmt.(fmt, pf) in
  let with_parens ~ambient_prec ~prec pp =
    if ambient_prec > prec then Fmt.parens pp else pp
  in
  let rec go ambient_prec ppf p =
    let loc = info p in
    open_loc ppf loc;
    let formatter, prec =
      match p with
      | AnyChar _ -> fmt ".", Prec.atom
      | Char (_, char) -> (fun ppf -> Fmt.(quote ~mark:"'" char) ppf char), Prec.atom
      | String (_, str) -> (fun ppf -> Fmt.(quote string) ppf str), Prec.atom
      | Satisfy (_, name, tm) ->
        (fun ppf -> pf ppf "@[<2>satisfy (@[%s -> {%a}@])@]" name core tm), Prec.atom
      | Let (_, name, named, body) ->
        ( (fun ppf ->
            pf ppf "@[<v>@[<2>let %s =@ @[%a@] in@]@ %a@]" name (go 0) named (go 0) body)
        , Prec.atom )
      | Fail (_, tm) ->
        let f ppf =
          match tm with
          | Term (Primitive (_, String msg)) -> pf ppf {|@[<2>fail "%s"@]|} msg
          | _ -> pf ppf "@[<2>fail {%a}@]" core tm
        in
        f, Prec.app
      | Count (_, p, tm) ->
        let f ppf =
          match tm with
          | Term (Primitive (_, Integer n)) ->
            pf ppf "@[<hv>%a%s@]" (go (Int.succ Prec.quantifier)) p (Z.to_string n)
          | _ -> pf ppf "@[<hv>%a{%a}@]" (go (Int.succ Prec.quantifier)) p core tm
        in
        f, Prec.quantifier
      | Option (_, p) ->
        (fun ppf -> pf ppf "%a?" (go (Int.succ Prec.quantifier)) p), Prec.quantifier
      | Many (_, p) ->
        (fun ppf -> pf ppf "%a*" (go (Int.succ Prec.quantifier)) p), Prec.quantifier
      | Many1 (_, p) ->
        (fun ppf -> pf ppf "%a+" (go (Int.succ Prec.quantifier)) p), Prec.quantifier
      | Choice (_, branches) ->
        let initial_bar =
          Format.pp_print_custom_break ~fits:("", 0, "") ~breaks:("", 2, "| ")
        in
        let formatter ppf =
          pf
            ppf
            "@[<hv 0>choice (%t%a@,)@]"
            initial_bar
            Fmt.(list ~sep:(any "@;<1 2>| ") (go Prec.alt))
            branches
        in
        formatter, Prec.alt
      | Fix (_, name, p) ->
        (fun ppf -> pf ppf "@[<2>fix@ (@[%s -> %a@])@]" name (go 0) p), Prec.app
      | Sequence (_, ps, p) ->
        let formatter ppf =
          pf
            ppf
            "@[<hv 2>@[<hv>%a@] ->@ {%a}@]"
            Fmt.(list ~sep:(any "@ ") binder)
            ps
            core
            p
        in
        formatter, Prec.app
      | Identifier (_, name) -> (fun ppf -> Fmt.string ppf name), Prec.atom
    in
    with_parens ~ambient_prec ~prec (fun ppf () -> formatter ppf) ppf ();
    close_loc ppf loc
  and binder ppf (Binder (opt_name, p)) =
    match opt_name with
    | None -> (go 0) ppf p
    | Some name -> pf ppf "%s=%a" name (go Prec.eq) p
  in
  go 0 ppf p
;;

let pp_range ppf p =
  pp_generic ~open_loc:Opt_range.open_stag ~close_loc:Opt_range.close_stag ppf p
;;

let pp_ranges ppf p =
  pp_generic
    ~open_loc:(fun ppf loc -> Stdlib.Format.pp_open_stag ppf (Source_ranges.Stag loc))
    ~close_loc:(fun ppf _loc -> Stdlib.Format.pp_close_stag ppf ())
    ppf
    p
;;

let pp_plain ppf p = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf p
let pp_str p = Fmt.to_to_string pp_plain p

let mk_some : 'info n_term -> 'info n_term =
 fun tm -> Nominal.Term.Operator (Nominal.Term.info tm, "some", [ Scope ([], tm) ])
;;

let mk_none range = Nominal.Term.Operator (range, "none", [])
let map_snd ~f (a, b) = a, f b

type 'info parse_error =
  { parser : 'info t
  ; sub_errors : 'info parse_error list
  }

type parser_stack = Source_ranges.t t Stack.t

module Direct = struct
  type 'info parser = 'info t
  type term_ctx = Source_ranges.t n_term String.Map.t

  type parse_result =
    (Source_ranges.t n_term, string * Source_ranges.t c_term option) Result.t

  type parser_ctx = Source_ranges.t parser String.Map.t

  type trace_snapshot =
    { success : bool
    ; pre_pos : int
    ; post_pos : int
    ; parser : Source_ranges.t parser
    ; term_ctx : term_ctx
    ; parser_ctx : parser_ctx
    ; snapshots : trace_snapshot list
    }

  type direct =
    { run :
        translate_direct:(Source_ranges.t parser -> direct)
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

  type t = direct

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

  let mk_error msg = Error (msg, None)

  let mk_char pos c =
    Nominal.Term.Primitive (Source_ranges.mk "input" pos (pos + 1), Char c)
  ;;

  let context_free go =
    { run =
        (fun ~translate_direct:_ ~term_ctx:_ ~parser_ctx:_ ~pos str ->
          match go pos str with
          | Ok (pos, result) -> pos, [], Ok result
          | Error msg -> pos, [], mk_error msg)
    }
  ;;

  let anychar =
    context_free (fun pos str ->
        if String.length str > pos
        then Ok (pos + 1, mk_char pos str.[pos])
        else Error "expected: .")
  ;;

  let char c =
    context_free (fun pos str ->
        if String.length str > pos && Char.(str.[pos] = c)
        then Ok (pos + 1, mk_char pos c)
        else Error (Printf.sprintf "expected: char '%c'" c))
  ;;

  let string prefix =
    context_free (fun pos str ->
        match str |> String.subo ~pos |> String.chop_prefix ~prefix with
        | None -> Error (Printf.sprintf {|expected: string "%s"|} prefix)
        | Some _str' ->
          let pos' = pos + String.length prefix in
          let rng = Source_ranges.mk "input" pos pos' in
          Ok (pos', Nominal.Term.Primitive (rng, String prefix)))
  ;;

  let satisfy name core_term =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx:_ ~pos str ->
          let err_msg =
            mk_error
              (Fmt.str
                 {|expected: satisfy (%s -> {%a})|}
                 name
                 Lvca_core.Term.pp
                 core_term)
          in
          if pos >= String.length str
          then pos, [], err_msg
          else (
            let c = str.[pos] in
            let rng = Source_ranges.mk "input" pos (pos + 1) in
            let tm =
              Lvca_core.Term.(
                Let
                  { info = Source_ranges.empty
                  ; is_rec = Lvca_core.Is_rec.No_rec
                  ; tm = Term (Primitive (rng, Char c))
                  ; ty = None
                  ; scope = Scope (name, core_term)
                  })
            in
            match Lvca_core.eval_in_ctx term_ctx tm with
            | Ok (Operator (_, "true", [])) -> pos + 1, [], Ok (mk_char pos c)
            | Ok (Operator (_, "false", [])) | Ok _ ->
              pos, [], err_msg (* TODO: throw harder error? (type error) *)
            | Error err -> pos, [], Error (map_snd ~f:(fun tm -> Some tm) err)))
    }
  ;;

  let fail c_tm =
    { run =
        (fun ~translate_direct:_ ~term_ctx ~parser_ctx:_ ~pos _str ->
          match Lvca_core.eval_in_ctx term_ctx c_tm with
          | Ok (Primitive (_, String msg)) -> pos, [], mk_error msg
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
          pos1, [ snapshot ], result)
    }
  ;;

  let option parser =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let pos0 = pos in
          let pos1, snapshots, result =
            (translate_direct parser).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos1
          in
          let result =
            match result with
            | Ok tm -> mk_some tm
            | Error _ -> mk_none Source_ranges.empty
          in
          pos1, [ snapshot ], Ok result)
    }
  ;;

  let mk_list lst =
    let rng = lst |> List.map ~f:Nominal.Term.info |> Source_ranges.unions in
    let lst = lst |> List.map ~f:(fun tm -> Nominal.Scope.Scope ([], tm)) in
    Nominal.Term.Operator (rng, "list", lst)
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
          go ~translate_direct ~term_ctx ~parser_ctx (n - 1) ~pos str
          |> Result.map ~f:(List.cons (pos, snapshot, tm)))
    in
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          match Lvca_core.eval_in_ctx term_ctx n_tm with
          | Ok (Primitive (_, Integer n)) ->
            let n = Z.to_int n (* TODO: may raise Overflow *) in
            let results = go ~translate_direct ~term_ctx ~parser_ctx ~pos n str in
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

  let rec go_many ~translate_direct ~term_ctx ~parser_ctx ~pos parser str =
    let pos0 = pos in
    let pos, snapshots, head_result =
      (translate_direct parser).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
    in
    let snapshot =
      mk_snapshot ~result:head_result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos
    in
    match head_result with
    | Error _ -> pos, [ snapshot ], Ok []
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
          let result =
            match result with
            | Ok [] -> mk_error "many1: empty list"
            | Ok tms -> Ok (mk_list tms)
            | Error msg -> Error msg
          in
          pos, snapshots, result)
    }
  ;;

  let choice ps =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let snapshot_queue = Queue.create () in
          let match_opt =
            List.find_map ps ~f:(fun parser ->
                let pos', snapshots, result =
                  (translate_direct parser).run
                    ~translate_direct
                    ~term_ctx
                    ~parser_ctx
                    ~pos
                    str
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
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          let pos0 = pos in
          let parser_ctx = Map.set parser_ctx ~key:name ~data:parser in
          let pos1, snapshots, result =
            (translate_direct parser).run ~translate_direct ~term_ctx ~parser_ctx ~pos str
          in
          let snapshot =
            mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos1
          in
          pos1, [ snapshot ], result)
    }
  ;;

  let sequence named_ps tm =
    let names, ps =
      named_ps |> List.map ~f:(fun (Binder (name_opt, p)) -> name_opt, p) |> List.unzip
    in
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
                name_vals
                |> List.fold ~init:term_ctx ~f:(fun ctx (key_opt, tm) ->
                       match key_opt with
                       | None -> ctx
                       | Some key -> Map.set ctx ~key ~data:tm)
              in
              let result =
                Lvca_core.eval_in_ctx term_ctx tm
                |> Result.map_error ~f:(map_snd ~f:(fun tm -> Some tm))
              in
              pos, snapshots, result))
    }
  ;;

  let identifier name =
    { run =
        (fun ~translate_direct ~term_ctx ~parser_ctx ~pos str ->
          match Map.find parser_ctx name with
          | None ->
            ( pos
            , []
            , mk_error (Printf.sprintf {|Identifer not found in context: "%s"|} name) )
          | Some parser ->
            let pos0 = pos in
            let pos1, snapshots, result =
              (translate_direct parser).run
                ~translate_direct
                ~term_ctx
                ~parser_ctx
                ~pos
                str
            in
            let snapshot =
              mk_snapshot ~result ~parser ~term_ctx ~parser_ctx ~snapshots pos0 pos1
            in
            pos1, [ snapshot ], result)
    }
  ;;

  let translate_direct : Source_ranges.t parser -> direct = function
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
    | Choice (_, ps) -> choice ps
    | Fix (_, name, p) -> fix name p
    | Sequence (_, ps, p) -> sequence ps p
    | Identifier (_, name) -> identifier name
  ;;

  let parse_direct
      :  ?term_ctx:term_ctx -> ?parser_ctx:parser_ctx -> Source_ranges.t parser -> string
      -> toplevel_result
    =
   fun ?(term_ctx = String.Map.empty) ?(parser_ctx = String.Map.empty) parser str ->
    let { run } = translate_direct parser in
    let pos, snapshots, result = run ~translate_direct ~term_ctx ~parser_ctx ~pos:0 str in
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

module Parse = struct
  type term = Opt_range.t t

  open Lvca_parsing

  let keywords : string list = [ (* "satisfy"; *) "let"; "in" (* "fail"; "fix" *) ]
  let keyword : string Lvca_parsing.t = keywords |> List.map ~f:string |> choice
  let operators : string list = [ "?"; "*"; "+"; "|"; "="; "->" ]
  let operator : string Lvca_parsing.t = operators |> List.map ~f:string |> choice

  type atom =
    | CharAtom of char
    | IntAtom of int
    | StrAtom of string
    | Dot

  let string_of_atom = function
    | CharAtom c -> Printf.sprintf "%C" c
    | IntAtom i -> Printf.sprintf "%d" i
    | StrAtom str -> Printf.sprintf "%S" str
    | Dot -> "."
  ;;

  type token =
    | Atom of atom * Opt_range.t
    | Operator of string * Opt_range.t
    | Keyword of string * Opt_range.t
    | Ident of string * Opt_range.t
    | Core of Opt_range.t Lvca_core.Term.t * Opt_range.t
    | Parenthesized of term * Opt_range.t
    | FailTok of Opt_range.t Lvca_core.Term.t * Opt_range.t
    | SatisfyTok of string * Opt_range.t Lvca_core.Term.t * Opt_range.t
    | ChoiceTok of token list * Opt_range.t
    | FixTok of string * token list * Opt_range.t

  let token_location = function
    | Atom (_, loc)
    | Operator (_, loc)
    | Keyword (_, loc)
    | Ident (_, loc)
    | Core (_, loc)
    | Parenthesized (_, loc)
    | FailTok (_, loc)
    | SatisfyTok (_, _, loc)
    | ChoiceTok (_, loc)
    | FixTok (_, _, loc) ->
      loc
  ;;

  let rec string_of_token = function
    | Atom (atom, _) -> string_of_atom atom
    | Operator (str, _) | Keyword (str, _) | Ident (str, _) -> str
    | Core (tm, _) -> Fmt.str "{%a}" Lvca_core.Term.pp tm
    | Parenthesized (tm, _) -> Fmt.to_to_string pp_plain tm
    | FailTok (tm, _) -> Fmt.str "fail {%a}" Lvca_core.Term.pp tm
    | SatisfyTok (name, tm, _) -> Fmt.str "satisfy (%s -> {%a})" name Lvca_core.Term.pp tm
    | ChoiceTok (toks, _) ->
      Fmt.str
        "choice (%s)"
        (toks |> List.map ~f:string_of_token |> String.concat ~sep:"; ")
    | FixTok (name, toks, _) ->
      Fmt.str
        "fix (%s -> %s)"
        name
        (toks |> List.map ~f:string_of_token |> String.concat ~sep:"; ")
  ;;

  let string_of_tokens tokens =
    tokens |> Queue.to_list |> List.map ~f:string_of_token |> String.concat ~sep:"; "
  ;;

  let string_of_binder (Binder (opt_name, p)) =
    match opt_name with
    | None -> pp_str p
    | Some name -> Printf.sprintf "%s=%s" name (pp_str p)
  ;;

  let string_of_binders binders =
    binders |> List.map ~f:string_of_binder |> String.concat ~sep:" "
  ;;

  let lbp = function
    | "?" | "*" | "+" -> Prec.quantifier
    | "=" -> Prec.eq
    | "|" -> Prec.alt
    | "->" -> Prec.arr
    | _ -> failwith "invalid operator name"
  ;;

  (* Parse an expression starting with a token (nothing in front of it) *)
  let rec prefix ~tokens = function
    | Atom (atom, range) ->
      (match atom with
      | CharAtom c -> return ~range (Char (range, c))
      | IntAtom i -> fail (Printf.sprintf "unexpected int atom (%d) in prefix position" i)
      | StrAtom c -> return ~range (String (range, c))
      | Dot -> return ~range (AnyChar range))
    | Operator (op_name, _pos) ->
      (* There must always be something preceding an operator *)
      fail ("prefix: unexpected operator " ^ op_name)
    | Keyword (kw_name, let_pos) ->
      (match kw_name with
      | "let" ->
        let mk_err tok_opt expected =
          match tok_opt with
          | Some tok ->
            fail (Printf.sprintf {|expected %s (got %s)|} expected (string_of_token tok))
          | None -> fail (Printf.sprintf {|expected %s (hit end of input)|} expected)
        in
        (match Queue.dequeue tokens with
        | Some (Ident (name, _)) ->
          (match Queue.dequeue tokens with
          | Some (Operator ("=", _)) ->
            (* XXX why treated differently from below? *)
            expression ~tokens ~ambient_prec:0
            >>= fun e1 ->
            (match Queue.dequeue tokens with
            | Some (Keyword ("in", _)) ->
              sequence ~tokens
              >>= fun e2 ->
              let range = Opt_range.union let_pos (info e2) in
              return ~range (Let (range, name, e1, e2))
            | tok_opt -> mk_err tok_opt {|keyword "in"|})
          | tok_opt -> mk_err tok_opt {|operator "="|})
        | tok_opt -> mk_err tok_opt "an identifier")
      | _ -> fail (Printf.sprintf {|invalid keyword "%s", expected "let"|} kw_name))
    | Ident (name, range) -> return ~range (Identifier (range, name))
    | Core (tm, range) -> return ~range (Sequence (range, [], tm))
    | Parenthesized (p, range) -> return ~range p
    | FailTok (tm, range) -> return ~range (Fail (range, tm))
    | SatisfyTok (name, tm, range) -> return ~range (Satisfy (range, name, tm))
    | ChoiceTok (toks, range) ->
      let tokens = Queue.of_list toks in
      choice_branches ~first:true ~tokens
      >>= fun branches ->
      (match Queue.length tokens with
      | 0 -> return ~range (Choice (range, branches))
      | _ -> fail ("Leftover tokens in a choice body: " ^ string_of_tokens tokens))
    | FixTok (name, toks, range) ->
      let tokens = Queue.of_list toks in
      sequence ~tokens
      >>= fun expr ->
      (match Queue.length tokens with
      | 0 -> return ~range (Fix (range, name, expr))
      | _ -> fail "Leftover tokens in a fix body")

  and choice_branches ~first ~tokens =
    let go () =
      sequence ~tokens
      >>== fun { value = branch; range = seq_pos; _ } ->
      choice_branches ~first:false ~tokens
      >>== fun { value = branches; range = branch_pos; _ } ->
      let range = Opt_range.union seq_pos branch_pos in
      return ~range (branch :: branches)
    in
    match Queue.peek tokens with
    | Some (Operator ("|", _)) ->
      let (_ : token) = Queue.dequeue_exn tokens in
      go ()
    | Some tok ->
      if first
      then go ()
      else fail (Printf.sprintf "TODO: choice branches %s" (string_of_token tok))
    | None -> return ~range:None []

  (* Parse the operator following an expression *)
  and infix ~(* or postfix *) tokens ~left ~op_name ~op_pos =
    let range =
      tokens
      |> Queue.to_list
      |> List.map ~f:token_location
      |> Opt_range.list_range
      |> Opt_range.union op_pos
    in
    match op_name with
    | "?" -> return ~range (Option (range, left))
    | "*" -> return ~range (Many (range, left))
    | "+" -> return ~range (Many1 (range, left))
    | _ ->
      failwith
        (Printf.sprintf "infix TODO: %s before [%s]" op_name (string_of_tokens tokens))

  and expression ~tokens ~ambient_prec =
    let token = Queue.dequeue_exn tokens in
    prefix ~tokens token
    >>= fun left ->
    let range = info left in
    (* Consume all operators with left binding power (precedence) higher than
       the ambient precedence *)
    let rec go ~ambient_prec left =
      match Queue.peek tokens with
      | None -> return ~range left
      | Some token ->
        (match token with
        | Operator ("->", _) -> return ~range left
        (* TODO: what about =? *)
        | Operator (op_name, op_pos) ->
          if lbp op_name > ambient_prec
          then (
            let (_ : token) = Queue.dequeue_exn tokens in
            infix ~tokens ~left ~op_name ~op_pos
            >>= go ~ambient_prec:(lbp op_name - 1 (* HACK *)))
          else return ~range left
        | Core (tm, range) ->
          let (_ : token) = Queue.dequeue_exn tokens in
          return ~range (Count (range, left, tm))
        | Atom (IntAtom i, range) ->
          let (_ : token) = Queue.dequeue_exn tokens in
          let i = Z.of_int i in
          return ~range (Count (range, left, Term (Primitive (range, Integer i))))
        | _ -> return ~range left)
    in
    go ~ambient_prec left

  and sequence_elem ~tokens =
    match Queue.peek_exn tokens with
    | Ident (name, name_pos) ->
      let next_toks =
        match Queue.to_list tokens with
        | _ :: next_toks -> next_toks
        | [] -> failwith "invariant violation: sequence_elem: no tokens"
      in
      let ident = Identifier (name_pos, name) in
      (match next_toks with
      | Operator ("=", _) :: _ ->
        (* dequeue both the identifier and the "=" *)
        let (_ : token) = Queue.dequeue_exn tokens in
        let (_ : token) = Queue.dequeue_exn tokens in
        expression ~tokens ~ambient_prec:Prec.eq
        >>= fun expr ->
        let range = Opt_range.union name_pos (info expr) in
        return ~range (Binder (Some name, expr))
      | Operator _ :: _ ->
        (* don't dequeue any tokens -- they'll all be parsed by expression *)
        expression ~tokens ~ambient_prec:Prec.arr
        >>= fun p -> return ~range:(info p) (Binder (None, p))
      | _ ->
        (* dequeue the identifier *)
        let (_ : token) = Queue.dequeue_exn tokens in
        return ~range:name_pos (Binder (None, ident)))
    | _ ->
      expression ~tokens ~ambient_prec:Prec.arr
      >>= fun expr -> return ~range:(info expr) (Binder (None, expr))

  (* Parse the forms:
    1. `x=foo bar y=baz -> {...}`
    2. `foo`

    Algorithm:
      Consume tokens one at a time:
        * expressions and bound expressions go in a `binders` queue
        * if we hit `|` (lower precedence than a sequence) or the end, then we
          expect a single expression to the left. fail otherwise.
        * if we hit `->`, return a sequence with all binders to the left,
          returning the expression to the right.
  *)
  and sequence ~tokens =
    let binders = Queue.create () in
    let rec go () =
      match Queue.peek tokens with
      | Some (Operator ("|", _)) | None ->
        (match Queue.to_list binders with
        (* Parse form 2: not a binder, but an expression. *)
        | [ Binder (None, expr) ] -> return ~range:(info expr) expr
        | _binders -> fail "Expected a single expression")
      (* Parse form 1 *)
      | Some (Operator ("->", _)) ->
        let _arr : token = Queue.dequeue_exn tokens in
        (match Queue.dequeue tokens with
        | Some (Core (tm, range)) ->
          return ~range (Sequence (range, Queue.to_list binders, tm))
        | Some (Ident (name, range)) ->
          return
            ~range
            (Sequence
               ( range
               , Queue.to_list binders
               , Lvca_core.Term.Term (Nominal.Term.Var (range, name)) ))
        | Some tok ->
          fail (Printf.sprintf "TODO (sequence token %s)" (string_of_token tok))
        | None -> fail "No token following `->` (expected a return value)")
      (* Consume groups of binders until we hit "->" or the end. *)
      | Some _token ->
        sequence_elem ~tokens
        >>= fun binder ->
        Queue.enqueue binders binder;
        go ()
    in
    go ()
  ;;

  let tokens_to_parser tokens =
    match tokens with
    | [] -> fail "empty input"
    | tokens -> sequence ~tokens:(Queue.of_list tokens)
  ;;

  let go f Parse_result.{ value; range } = Parse_result.{ value = f value range; range }

  let t : Opt_range.t Lvca_core.Term.t Lvca_parsing.t -> term Lvca_parsing.t =
   fun c_term ->
    let arrow = string "->" in
    fix (fun parser ->
        let token =
          fix (fun token ->
              choice
                [ char_lit >>|| go (fun c range -> Atom (CharAtom c, range))
                ; integer_lit
                  >>|| go (fun i range -> Atom (IntAtom (Int.of_string i), range))
                ; string_lit >>|| go (fun str range -> Atom (StrAtom str, range))
                ; char '.' >>|| go (fun _ range -> Atom (Dot, range))
                ; operator >>|| go (fun op range -> Operator (op, range))
                ; keyword >>|| go (fun kw range -> Keyword (kw, range))
                ; (string "fail"
                  >>== fun { range = p1; _ } ->
                  choice
                    [ braces c_term
                      >>|| go (fun tm p2 ->
                               let range = Opt_range.union p1 p2 in
                               FailTok (tm, range))
                    ; string_lit
                      >>|| go (fun str p2 ->
                               let range = Opt_range.union p1 p2 in
                               FailTok
                                 (Lvca_core.Term.Term (Primitive (p2, String str)), range))
                    ])
                ; (string "satisfy"
                  >>== fun { range = sat_pos; _ } ->
                  parens
                    (lift3
                       (fun name _arr (tm, tm_pos) ->
                         let range = Opt_range.union sat_pos tm_pos in
                         SatisfyTok (name, tm, range))
                       identifier
                       arrow
                       (attach_pos (braces c_term))))
                ; (string "choice"
                  >>== fun { range = choice_pos; _ } ->
                  parens (many token)
                  >>|| go (fun toks toks_pos ->
                           let range = Opt_range.union choice_pos toks_pos in
                           ChoiceTok (toks, range)))
                ; (string "fix"
                  >>== fun { range = fix_pos; _ } ->
                  parens
                    (lift3
                       (fun name _arr toks ->
                         let toks_pos =
                           toks |> List.map ~f:token_location |> Opt_range.list_range
                         in
                         let range = Opt_range.union fix_pos toks_pos in
                         FixTok (name, toks, range))
                       identifier
                       arrow
                       (many1 token)))
                ; identifier >>|| go (fun ident range -> Ident (ident, range))
                ; braces c_term >>|| go (fun tm range -> Core (tm, range))
                ; parens parser >>|| go (fun p range -> Parenthesized (p, range))
                ])
          <?> "token"
        in
        many token >>= tokens_to_parser)
    <?> "parser"
 ;;
end

let parse = Parse.t

module TestParsers = struct
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

let%test_module "Parsing" =
  (module struct
    let parse_print : string -> string -> unit =
     fun parser_str str ->
      match Lvca_parsing.parse_string (Parse.t Lvca_core.Term.parse) parser_str with
      | Error msg -> print_endline ("failed to parse parser desc: " ^ msg)
      | Ok parser ->
        let parser' = map_info ~f:(Source_ranges.of_opt_range ~buf:"parser") parser in
        let Direct.{ result; _ } = Direct.parse_direct parser' str in
        (match result with
        | Error (msg, _) -> printf "failed to parse: %s\n" msg
        | Ok tm -> Fmt.pr "%a\n" Nominal.Term.pp_source_ranges tm)
   ;;

    open TestParsers

    let () =
      Format.set_formatter_stag_functions Source_ranges.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let%expect_test _ =
      parse_print "'c'2" "cc";
      [%expect
        {| <input:{0,2}>list(<input:{0,1}>'c'</input:{0,1}>; <input:{1,2}>'c'</input:{1,2}>)</input:{0,2}> |}]
    ;;

    let%expect_test _ =
      parse_print dot "c";
      [%expect {| <input:{0,1}>'c'</input:{0,1}> |}]
    ;;

    let%expect_test _ =
      parse_print str "str";
      [%expect {| <input:{0,3}>"str"</input:{0,3}> |}]
    ;;

    let%expect_test _ =
      parse_print str "foo";
      [%expect {| failed to parse: expected: string "str" |}]
    ;;

    let%expect_test _ =
      parse_print str_star "strstrstr";
      [%expect
        {| <input:{0,9}>list(<input:{0,3}>"str"</input:{0,3}>; <input:{3,6}>"str"</input:{3,6}>; <input:{6,9}>"str"</input:{6,9}>)</input:{0,9}> |}]
    ;;

    let%expect_test _ =
      parse_print str_plus "strstrstr";
      [%expect
        {| <input:{0,9}>list(<input:{0,3}>"str"</input:{0,3}>; <input:{3,6}>"str"</input:{3,6}>; <input:{6,9}>"str"</input:{6,9}>)</input:{0,9}> |}]
    ;;

    let%expect_test _ =
      parse_print choice "str";
      [%expect {| <input:{0,3}>"str"</input:{0,3}> |}]
    ;;

    let%expect_test _ =
      parse_print choice "foo";
      [%expect {| <input:{0,3}>"foo"</input:{0,3}> |}]
    ;;

    let%expect_test _ =
      parse_print sat_parser "c";
      [%expect {| <input:{0,1}>'c'</input:{0,1}> |}]
    ;;

    let%expect_test _ =
      parse_print sat_parser "d";
      [%expect
        {|
          failed to parse: expected: satisfy (x -> {match x with { 'c' -> {true()} | _ -> {false()} }}) |}]
    ;;

    let%expect_test _ =
      parse_print let_var "str";
      [%expect {| <input:{0,3}>"str"</input:{0,3}> |}]
    ;;

    let%expect_test _ =
      parse_print fail "str";
      (* TODO: nicer formatting *)
      [%expect {| failed to parse: reason |}]
    ;;

    let%expect_test _ =
      parse_print char_opt "c";
      [%expect {| <input:{0,1}>some(<input:{0,1}>'c'</input:{0,1}>)</input:{0,1}> |}]
    ;;

    (* TODO: determine proper provenance *)
    let%expect_test _ =
      parse_print char_opt "";
      [%expect {| <>none()</> |}]
    ;;

    let%expect_test _ =
      parse_print ret "";
      [%expect {| <parser:{2,7}>foo()</parser:{2,7}> |}]
    ;;

    let%expect_test _ =
      parse_print fix "a";
      [%expect {| <input:{0,1}>"a"</input:{0,1}> |}]
    ;;

    let%expect_test _ =
      parse_print seq "a";
      (* TODO: which attribution is correct? [%expect {| <parser:13-16>"a"</parser:13-16> |}] *)
      [%expect {| <input:{0,1}>"a"</input:{0,1}> |}]
    ;;

    let%expect_test _ =
      parse_print list_parser "";
      [%expect {| <parser:{69,74}>nil()</parser:{69,74}> |}]
    ;;

    let%expect_test _ =
      parse_print list_parser "c";
      [%expect
        {| <parser:{45,56}>cons(<input:{0,1}>'c'</input:{0,1}>; <parser:{69,74}>nil()</parser:{69,74}>)</parser:{45,56}> |}]
    ;;

    let%expect_test _ =
      parse_print list_parser "cc";
      [%expect
        {| <parser:{45,56}>cons(<input:{0,1}>'c'</input:{0,1}>; <parser:{45,56}>cons(<input:{1,2}>'c'</input:{1,2}>; <parser:{69,74}>nil()</parser:{69,74}>)</parser:{45,56}>)</parser:{45,56}> |}]
    ;;

    let%expect_test _ =
      parse_print seq2 "aab";
      [%expect
        {| <parser:{24,40}>triple(<input:{0,1}>"a"</input:{0,1}>; <input:{1,2}>"a"</input:{1,2}>; <input:{2,3}>"b"</input:{2,3}>)</parser:{24,40}> |}]
    ;;

    let%expect_test _ =
      parse_print seq3 "aab";
      [%expect
        {| <parser:{24,40}>triple(<input:{0,1}>'a'</input:{0,1}>; <input:{1,2}>'a'</input:{1,2}>; <input:{2,3}>'b'</input:{2,3}>)</parser:{24,40}> |}]
    ;;

    let%expect_test _ =
      parse_print fix2 "a";
      [%expect {| failed to parse: choice: no match found |}]
    ;;

    let%expect_test _ =
      parse_print fix2 "ab";
      [%expect
        {| <parser:{40,50}>pair(<input:{0,1}>"a"</input:{0,1}>; <parser:{68,71}>"b"</parser:{68,71}>)</parser:{40,50}> |}]
    ;;

    let%expect_test _ =
      parse_print fix3 "a + 1";
      [%expect
        {| <parser:{321,336}>add(<parser:{149,157}>var(<input:{0,1}>"a"</input:{0,1}>)</parser:{149,157}>; <parser:{198,212}>literal(<input:{4,5}>list(<input:{4,5}>'1'</input:{4,5}>)</input:{4,5}>)</parser:{198,212}>)</parser:{321,336}> |}]
    ;;

    let%expect_test _ =
      parse_print fix3 "a + b + c";
      [%expect
        {| <parser:{321,336}>add(<parser:{149,157}>var(<input:{0,1}>"a"</input:{0,1}>)</parser:{149,157}>; <parser:{321,336}>add(<parser:{149,157}>var(<input:{4,5}>"b"</input:{4,5}>)</parser:{149,157}>; <parser:{149,157}>var(<input:{8,9}>"c"</input:{8,9}>)</parser:{149,157}>)</parser:{321,336}>)</parser:{321,336}> |}]
    ;;

    let%expect_test _ =
      parse_print pair "ab";
      [%expect
        {| <parser:{17,27}>pair(<input:{0,1}>'a'</input:{0,1}>; <input:{1,2}>'b'</input:{1,2}>)</parser:{17,27}> |}]
    ;;

    let%expect_test _ =
      parse_print pair2 "ab";
      [%expect
        {| <parser:{13,23}>pair(<input:{0,1}>'a'</input:{0,1}>; <input:{1,2}>'b'</input:{1,2}>)</parser:{13,23}> |}]
    ;;

    let parse_print_parser : ?width:int -> string -> unit =
     fun ?width parser_str ->
      match
        Lvca_parsing.(
          parse_string (whitespace *> Parse.t Lvca_core.Term.parse) parser_str)
      with
      | Error msg -> print_string ("failed to parse parser desc: " ^ msg)
      | Ok parser ->
        let pre_geom =
          match width with
          | None -> None
          | Some n ->
            let geom = Format.get_geometry () in
            Format.set_margin n;
            Some geom
        in
        Fmt.pr "%a\n" pp_plain parser;
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
      parse_print
        {|let alpha = satisfy (c -> {is_alpha c}) in
        chars=alpha+ -> { let str = string_of_chars chars in {var(str)} }|}
        "ab";
      [%expect
        {| <parser:{105,113}>var(<input:{0,2}>"ab"</input:{0,2}>)</parser:{105,113}> |}]
    ;;

    let%expect_test _ =
      parse_print_parser
        {|let atom = choice (name | literal) in
fix
(expr -> choice
(atom=atom ' '* '+'
' '*
expr=expr -> {{add(atom;
expr)}} | atom=atom -> {atom}))|};
      [%expect
        {|
       let atom = choice (name | literal) in
       fix
         (expr -> choice (
                    | atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
                    | atom=atom -> {atom}
                  )) |}]
    ;;

    let%expect_test _ =
      parse_print_parser fix3;
      [%expect
        {|
       let char = satisfy (c -> {is_alpha c}) in
       let digit = satisfy (c -> {is_digit c}) in
       let name = chars=char+ -> {let str = string_of_chars chars in {var(str)}} in
       let literal = chars=digit+ -> {{literal(chars)}} in
       let atom = choice (name | literal) in
       fix
         (expr -> choice (
                    | atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
                    | atom=atom -> {{atom}}
                  )) |}]
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
      [%expect {| (F+)+ |}]
    ;;

    let%expect_test _ =
      parse_print_parser "(F+)+";
      [%expect {| (F+)+ |}]
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
      parse_print_parser "choice ('a' | 'b' | 'c')";
      [%expect {| choice ('a' | 'b' | 'c') |}]
    ;;

    let%expect_test _ =
      parse_print_parser "choice (. -> {Q} | .)";
      [%expect {| choice (. -> {Q} | .) |}]
    ;;

    let%expect_test _ =
      parse_print_parser list_parser;
      [%expect
        {|

       fix (lst -> choice (c='c' cs=lst -> {{cons(c; cs)}} |  -> {{nil()}})) |}]
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
let atom = choice (name | literal) in
fix (expr -> choice (
        | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
  | a=atom -> {a}
))|};
      [%expect
        {|
       let atom = choice (name | literal) in
       fix
         (expr -> choice (
                    | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
                    | a=atom -> {a}
                  )) |}]
    ;;

    (* TODO
    let%expect_test _ =
      parse_print_parser
        {|
let atom = choice (name | literal) in
fix (expr -> choice (
        | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
  | a=atom -> a
))|};
      [%expect
        {|
       let atom = choice (name | literal) in
       fix
         (expr -> choice (
                    | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
                    | a=atom -> a
                  )) |}]
    ;;
    *)

    let%expect_test _ =
      parse_print_parser ~width:12 "choice (name | literal)";
      [%expect {|
     choice (
       | name
       | literal
     ) |}]
    ;;
  end)
;;

module Properties = struct
  open Property_result

  let parse parser_str =
    Lvca_parsing.parse_string (Parse.t Lvca_core.Term.parse) parser_str
  ;;

  let pp_str p = Fmt.to_to_string pp_plain p

  let string_round_trip1 t =
    match t |> pp_str |> parse with
    | Ok t' ->
      let t' = erase t' in
      Property_result.check
        (equal Unit.( = ) t' t)
        (Fmt.str "%a <> %a" pp_plain t' pp_plain t)
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
