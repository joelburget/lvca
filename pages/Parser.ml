open Base
open Lvca_core
open Lvca_syntax
open ReactiveData

module P = Lvca_languages.Parser
module ParseCore = Core.Parse (ParseUtil.CComment)
module ParseParser = P.Parse (ParseUtil.CComment)
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module RHtml = Js_of_ocaml_tyxml.Tyxml_js.R.Html

open Components

let parse_parser str = ParseUtil.parse_string (ParseParser.t ParseCore.term) str

module Model = struct
  type parser_defn =
    | NoInputYet
    | Parsed of OptRange.t P.t
    | FailedParse of string

  type parser_defn_signal_pool = (string * parser_defn) Pool.Signal.t
  let parser_defn_signal_pool = Pool.Signal.create ()
  let test_signal_pool = Pool.Signal.create ()

  let evaluations_pool : Pool.Signal.key Pool.RList.t = Pool.RList.create ()

  type parser_context =
    { parser_str_key: Pool.Signal.key
    ; evaluations_key: Pool.RList.key
    }

  let mk_parser_defn str = match parse_parser str with
    | Ok parser -> Parsed parser
    | Error msg -> FailedParse msg

  let new_context parser_str lst =
    let create_parser_defn str =
      let parser = match str with
      | "" -> NoInputYet
      | _ -> mk_parser_defn str
      in str, parser
    in

    let evaluations_key = Pool.RList.add evaluations_pool (RList.create lst) in
    { parser_str_key = Pool.Signal.add parser_defn_signal_pool
        (React.S.create (create_parser_defn parser_str))
    ; evaluations_key
    }

  let new_test str = Pool.Signal.add test_signal_pool (React.S.create str)

  type t = parser_context list

  let initial_model = P.TestParsers.(
    [ new_context char_count [new_test "cc"]
    ; new_context str [new_test "str"; new_test "foo"]
    ; new_context str_star [new_test ""; new_test "strstrstr"]
    ; new_context str_plus [new_test ""; new_test "strstrstr"]
    ; new_context alt [new_test "str"; new_test "foo"]
    ; new_context sat_parser [new_test "c"; new_test "d"]
    ; new_context ".*" [new_test "foo"]
    ; new_context fix2
      [ new_test "a"
      ; new_test "ab"
      ]
    ])
end

module Action = struct
  type parser_no = int
  type test_no = int

  type t =
    | AddParser
    | AddTest of parser_no
    | RemoveTest of parser_no * test_no
    | UpdateParser of Pool.Signal.key * string
end

module Controller = struct
  let update (action : Action.t) (model_s : Model.t React.S.t) signal_update =
    let contexts = React.S.value model_s in

    match action with
      | AddParser
      -> signal_update (Model.new_context "" [] :: contexts)
      | AddTest parser_no ->
        let context = List.nth_exn contexts parser_no in
        let key = Pool.Signal.add Model.test_signal_pool (React.S.create "") in
        let _, evaluations_h = Pool.RList.find_exn
          Model.evaluations_pool
          context.evaluations_key
        in
        RList.snoc key evaluations_h
      | RemoveTest (parser_no, test_no) ->
        let context = List.nth_exn contexts parser_no in
        let _, evaluations_h = Pool.RList.find_exn
          Model.evaluations_pool
          context.evaluations_key
        in
        RList.remove test_no evaluations_h
      | UpdateParser (key, str) ->
        let _, update = Pool.Signal.find_exn Model.parser_defn_signal_pool key in
        update (str, Model.mk_parser_defn str)
end

let string_location ~str ~loc =
  let before = String.subo str ~len:loc in
  let after = String.subo str ~pos:loc in
  [%html{|
  <div class="string-location">
    <span class="string-location-before">|}[Html.txt before]{|</span>
    <span class="string-location-after">|}[Html.txt after]{|</span>
  </div>
  |}]

(* TODO: reactive version *)
let view_parser parser =
  let selection_s = React.S.const None in
  let elt, formatter = RangeFormatter.mk selection_s in
  Fmt.pf formatter "%a" P.pp_plain parser;
  Fmt.flush formatter ();
  rows ~classes:["parser-display"] [elt]

let view_term tm =
  let str = Fmt.str "parsed: %a" (Nominal.pp_term_ranges Primitive.pp) tm in
  Html.div [txt str]

let view_term_ctx ctx = ctx
  |> Map.to_alist
  |> List.map ~f:(fun (name, tm) -> cols ~border:true ~label:"term"
    [ txt name
    ; view_term tm
    ])
  |> rows ~label:"term-ctx"

let view_parser_ctx ctx = ctx
  |> Map.to_alist
  |> List.map ~f:(fun (name, p) -> cols ~border:true ~label:"parser"
    [ txt name
    ; view_parser p
    ])
  |> rows ~label:"parser-ctx"

let rec view_snapshots str snapshots =
  let opened_s, opened_update = React.S.create false in
  let handle_click v _evt = opened_update v; false in

  let view_snapshot' str p = Html.li [view_snapshot str p] in

  opened_s
    |> React.S.map (fun opened ->
      let onclick = handle_click (not opened) in
      match List.length snapshots, opened with
      | 0, _ -> []
      | _, true ->
        [ rows
          [ (* TODO: div is to prevent button from taking up too much space *)
            Html.div [button ~onclick "close subparser snapshots"]
          ]
        ; snapshots |> List.map ~f:(view_snapshot' str) |> olist
        ]
      | n, false ->
        [ rows
          [ Html.(p [ txt (Caml.Printf.sprintf "This parser called %d other parsers" n)])
          ; (* TODO: div is to prevent button from taking up too much space *)
            Html.div [button ~onclick "open subparser snapshots"]
          ]
        ])
    |> RList.from_signal
    |> r_rows ~border:true ~label:"view-snapshots"

and view_snapshot str P.Direct.{ pos; parser; term_ctx; parser_ctx; snapshots } = dlist
  (* ~label:"view-snapshot" *)
  (* ~border:true *)
  [ "parser", view_parser parser
  ; "location", string_location ~str ~loc:pos
  ; "term context", view_term_ctx term_ctx
  ; "parser context", view_parser_ctx parser_ctx
  ; "snapshots", view_snapshots str snapshots
  ]

module View = struct
  module Direct = P.Direct

  let view_parser_test parser test =
    match parser with
    | Model.NoInputYet | FailedParse _ -> Components.empty_elem, Components.empty_elem
    | Parsed parser ->
      let parser' = P.map_loc
        ~f:(SourceRanges.of_opt_range ~buf:"TODO")
        parser
      in
      let P.Direct.{ snapshot; result } = Direct.parse_direct parser' test in
      let result = match result with
        | Error (msg, tm_opt) ->
          let tm_str = match tm_opt with
            | None -> "(no tm) " ^ msg
            | Some tm -> Printf.sprintf "%s: %s" msg (Fmt.str "%a" Core.pp tm )

          in
          error_msg tm_str
        | Ok tm -> view_term tm
      in
      let trace =
        rows ~border:false
          [ subheader "trace this parser's execution"
          ; view_snapshot test snapshot
          ]
      in
      result, trace

  let mk_context update parser_no Model.{ parser_str_key; evaluations_key } =
    let evaluations_l, _ = Pool.RList.find_exn Model.evaluations_pool evaluations_key in
    let parser_s, _ =
      Pool.Signal.find_exn Model.parser_defn_signal_pool parser_str_key
    in
    let parser_defn_input, parser_defn_input_event = Common.mk_multiline_input
      (React.S.Pair.fst parser_s)
    in

    let new_test_handler _evt = update (Action.AddTest parser_no); false in
    let new_test_button = button ~onclick:new_test_handler "new test" in

    let (_ : unit React.event) = parser_defn_input_event
      |> React.E.map (function
        | Common.InputUpdate str -> update (UpdateParser (parser_str_key, str))
        | InputSelect _ | InputUnselect -> ()
        )
    in

    let parser_s = React.S.Pair.snd parser_s in

    let parser_error_elem = parser_s
      |> React.S.map (function
        | Model.NoInputYet | Parsed _ -> []
        | FailedParse msg -> [error_msg msg]
      )
    in

    (* TODO: there has to be a way to do this without the ref *)
    let i = ref 0 in
    let test_elems = evaluations_l
      |> RList.map (fun test_key ->
        let test_s, test_update =
          Pool.Signal.find_exn Model.test_signal_pool test_key
        in

        let test_input, test_input_event = Common.mk_single_line_input test_s in

        (* Q: is it okay to just update test directly without dispatching? *)
        let (_ : unit React.event) = React.E.map test_update test_input_event in

        let test_no = !i in
        Int.incr i;
        let remove_test_handler _evt =
          update (Action.RemoveTest (parser_no, test_no));
          false
        in
        let remove_test_button = button ~onclick:remove_test_handler "remove test" in

        let parser_result_s = React.S.l2 view_parser_test parser_s test_s in
        let output_s = React.S.Pair.fst parser_result_s in
        let snapshot_s = React.S.Pair.snd parser_result_s in
        rows
          [ cols
            [ test_input
            ; RHtml.div (RList.singleton_s output_s)
            ; remove_test_button
            ]
          ; RHtml.div (RList.singleton_s snapshot_s)
          ]
      )
    in

    Html.(div
      ~a:[a_class ["parser-context"]]
      [ parser_defn_input
      ; RHtml.div (RList.from_signal parser_error_elem)
      ; RHtml.div test_elems
      ; new_test_button
      ])

  let view model_s signal_update =
    let update evt = Controller.update evt model_s signal_update in
    let context_elems_s = model_s |> React.S.map (List.mapi ~f:(mk_context update)) in
    let new_parser_handler _evt = update AddParser; false in

    rows
      [ button ~onclick:new_parser_handler "create new parser"
      ; RHtml.div (RList.from_signal context_elems_s)
      ]
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
