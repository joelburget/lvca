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

type trace_snapshot = P.Direct.trace_snapshot

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
    ; tests_expanded: bool React.signal
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
    ; tests_expanded = true
    ; evaluations_key
    }

  let new_test str = Pool.Signal.add test_signal_pool (React.S.create str)

  type t = parser_context list

  let initial_model = P.TestParsers.(
    [ new_context fix2
      [ new_test "a"
      ; new_test "ab"
      ]
    ; new_context str_star [new_test ""; new_test "strstrstr"]
    ; new_context ".*" [new_test "foo"]
    ; new_context char_count [new_test "cc"]
    ; new_context str [new_test "str"; new_test "foo"]
    ; new_context str_plus [new_test ""; new_test "strstrstr"]
    ; new_context alt [new_test "str"; new_test "foo"]
    ; new_context sat_parser [new_test "c"; new_test "d"]
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
  if String.(str = "")
  then
    [%html{|<div class="font-mono mx-2 .bg-gray-100 underline">(empty string)</div>|}]
  else
    let before = String.subo str ~len:loc in
    let after = String.subo str ~pos:loc in
    [%html{|
    <div class="flex flex-row font-mono mx-2 .bg-gray-100 underline">
      <div class="inline-block">
        <span class="text-gray-500">|}[Html.txt before]{|</span>
      </div>
      <div style="width: 0" class="inline-block">
        <div
           style="width: 2px; margin-top: 0.125rem"
           class="h-5 relative bg-black"
        ></div>
      </div>
      <div class="inline-block">
        <span>|}[Html.txt after]{|</span>
      </div>
    </div>
    |}]

(* TODO: reactive version *)
let view_parser parser success =
  let selection_s = React.S.const None in
  let elt, formatter = RangeFormatter.mk selection_s in
  Fmt.pf formatter "%a" P.pp_plain parser;
  Fmt.flush formatter ();
  Html.(div ~a:[a_class [if success then "success" else "error"]] [elt])

let view_term tm =
  let str = Fmt.str "parsed: %a" (Nominal.pp_term_ranges Primitive.pp) tm in
  Html.div [txt str]

  (*
let view_term_ctx ctx = ctx
  |> Map.to_alist
  |> List.map ~f:(fun (name, tm) -> dlist ~label:"term" [ name, view_term tm ])
  |> rows ~label:"term-ctx"

let view_parser_ctx ctx = ctx
  |> Map.to_alist
  |> List.map ~f:(fun (name, p) -> dlist ~label:"parser" [ name, view_parser p ])
  |> rows ~label:"parser-ctx"
  *)

let path_controls _path_s _path_h = rows
  []

let snapshot_advanced_view str P.Direct.{ pre_pos; post_pos; _ } =
  let n = post_pos - pre_pos in
  let chars = match n with
    | 1 -> "1 character"
    | _ -> Printf.sprintf "%d characters" n
  in

  Html.(div
    [ inline_block (txt (" advances the input " ^ chars ^ " to "))
    ; inline_block (string_location ~str ~loc:post_pos)
    ])

let snapshot_controls str snapshots path_h =

  let header = [%html{|
    <tr>
      <td class="border-2">parser</td>
      <td class="border-2">action</td>
      <td class="border-2"></td>
    </tr>
  |}]
  in

  let body = snapshots
    |> List.mapi ~f:(fun i snapshot ->
      let P.Direct.{ success; parser; _ } = snapshot in
      let onclick _ = RList.snoc i path_h; false in
      let btn = button ~onclick "view" in

      Html.(tr
        [ td ~a:[a_class ["border-2"]] [view_parser parser success]
        ; td ~a:[a_class ["border-2"]] [snapshot_advanced_view str snapshot]
        ; td ~a:[a_class ["border-2"]] [btn]
        ]))
    |> RList.const
  in

  table header body

let view_controls str path_s path_h snapshots =
  let n_snaps = List.length snapshots in
  let msg = match n_snaps with
    | 0 -> "this parser calls no subparsers"
    | 1 -> "this parser calls 1 subparser"
    | _ -> Caml.Printf.sprintf "this parser calls %n subparsers" n_snaps
  in

  let always_visible_rows =
    [ path_controls path_s path_h
    ; subheader msg
    ]
  in

  rows (if List.length snapshots > 0
    then Lvca_util.List.snoc always_visible_rows (snapshot_controls str snapshots path_h)
    else always_visible_rows)

type path_traversal =
  { bottom_snapshot: trace_snapshot
  ; stack: trace_snapshot list
  }

(* Traverse the path, returning the snapshot at the bottom and all the snapshots along the
 * way. *)
let traverse_path ~root ~path =
  let current_snapshot = ref root in
  let stack = path
    |> List.map ~f:(fun i ->
      let snapshot = !current_snapshot in
      let P.Direct.{ snapshots; _ } = snapshot in
      current_snapshot := List.nth_exn snapshots i;
      snapshot
    )
  in
  { bottom_snapshot = !current_snapshot; stack }

let view_stack = fun root path_h path_s ->

    path_s
      |> RList.signal
      |> React.S.map (fun path -> (traverse_path ~root ~path).stack
        |> List.mapi ~f:(fun n snapshot ->
          let P.Direct.{ parser; success; _ } = snapshot in
          let elem = view_parser parser success in
          let onclick _ = RList.set path_h (List.take path n); false in
          (* let button = button ~onclick "return here" in *)
          let btn = Html.(div
            ~a:[a_onclick onclick; a_class ["button"]]
            [elem])
          in
          Html.(tr [td [btn]]))
      )
      |> RList.from_signal
      |> RHtml.table

let view_root_snapshot str root =
  let path_s, path_h = RList.create [] in

  let current_snapshot_s = path_s
    |> RList.signal
    |> React.S.map (fun path -> (traverse_path ~root ~path).bottom_snapshot)
  in

  let stack_view = view_stack root path_h path_s in

  let stack_section = path_s
    |> RList.signal
    |> React.S.map (fun path ->
      [ subheader "stack"
      ; if List.length path > 0 then stack_view else txt "(empty)"
      ]
    )
    |> RList.from_signal
  in

  let controls_s = current_snapshot_s
    |> React.S.map (fun P.Direct.{ snapshots; _ } ->
        view_controls str path_s path_h snapshots)
  in

  let parser_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ success; parser; _ } -> view_parser parser success)
  in

  let pre_loc_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ pre_pos; _ } -> string_location ~str ~loc:pre_pos)
    |> RList.singleton_s
    |> RHtml.(div ~a:[a_class (React.S.const ["inline-block"])])
  in

  let advanced_n_s = current_snapshot_s |> React.S.map (snapshot_advanced_view str) in

  (* TODO: is there a cleaner way to do this? *)
  let mk_div r_elem = RHtml.div (RList.singleton_s r_elem) in

  let status_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ success; _ } -> Html.(span
        ~a:[a_class [if success then "success" else "error"]]
        [ if success
          then txt "succeeds"
          else txt "fails"
        ]))
    |> React.S.map List.return
    |> RList.from_signal
    |> RHtml.span
  in

  rows
    [ RHtml.div stack_section
    ; subheader "parser"
    ; mk_div parser_view
    ; cols [ Html.(div [ span [ txt "The input to this parser is " ]]); pre_loc_view]
    ; Html.(div
      [ inline_block (span [ txt "This parser " ])
      ; inline_block status_view
      ; inline_block (txt " and ")
      ; RHtml.div
        ~a:[a_class ["inline-block"]]
        (RList.singleton_s advanced_n_s) (* TODO: only if success *)
      ])
    ; mk_div controls_s
    ]

module View = struct
  module Direct = P.Direct

  let view_parser_test parser test =
    match parser with
    | Model.NoInputYet | FailedParse _ -> Components.empty_elem, Components.empty_elem
    | Parsed parser ->
      let parser = P.map_loc ~f:(SourceRanges.of_opt_range ~buf:"TODO") parser in
      let P.Direct.{ snapshot; result } = Direct.parse_direct parser test in
      let result = match result with
        | Error (msg, tm_opt) ->
          let tm_str = match tm_opt with
            | None -> "(no tm) " ^ msg
            | Some tm -> Printf.sprintf "%s: %s" msg (Fmt.str "%a" Core.pp tm )

          in
          error_msg tm_str
        | Ok tm -> view_term tm
      in
      let trace = rows ~border:true
        [ subheader "trace this parser's execution"
        ; view_root_snapshot test snapshot
        ]
      in
      result, trace

  let mk_context update parser_no
    Model.{ parser_str_key; tests_expanded; evaluations_key } =
    let evaluations_l, _ = Pool.RList.find_exn Model.evaluations_pool evaluations_key in
    let parser_s, _ =
      Pool.Signal.find_exn Model.parser_defn_signal_pool parser_str_key
    in
    let parser_defn_input, parser_defn_input_event = Common.mk_multiline_input
      ~rows:(Some 2)
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
        rows ~border:true
          [ cols
            [ test_input
            ; RHtml.div (RList.singleton_s output_s)
            ; remove_test_button
            ]
          ; RHtml.div (RList.singleton_s snapshot_s)
          ]
      )
    in

    Html.tr
      [ Html.td
        [ parser_defn_input
        ; Html.table
          [ Html.(tr ~a:[a_class ["border-2"]]
            [RHtml.td (RList.from_signal parser_error_elem)])
          ; Html.(tr ~a:[a_class ["border-2"]] [RHtml.td test_elems])
          ; Html.(tr ~a:[a_class ["border-2"]] [Html.td [new_test_button]])
          ]
        ]
      ]

  let view model_s signal_update =
    let update evt = Controller.update evt model_s signal_update in
    let context_elems_s = model_s |> React.S.map (List.mapi ~f:(mk_context update)) in
    let new_parser_handler _evt = update AddParser; false in

    rows
      [ subheader "parsers"
      ; Html.div [button ~onclick:new_parser_handler "create new parser"]
      ; RHtml.table (RList.from_signal context_elems_s)
      ]
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
