open Base
open Lvca_core
open Lvca_syntax
open ReactiveData

module ParseCore = Core.Parse (ParseUtil.CComment)
module ParseParser = Lvca_languages.Parser.Parse (ParseUtil.CComment)

let parse_parser str = ParseUtil.parse_string (ParseParser.t ParseCore.term) str

module Model = struct
  type parser_defn =
    | NoInputYet
    | Parsed of Lvca_languages.Parser.t
    | FailedParse of string

  type parser_defn_signal_pool = (string * parser_defn) Pool.Signal.t
  let parser_defn_signal_pool = Pool.Signal.create ()
  let test_signal_pool = Pool.Signal.create ()

  let evaluations_pool : Pool.Signal.key Pool.RList.t = Pool.RList.create ()

  type parser_context =
    { parser_str_key: Pool.Signal.key
    ; evaluations_key: Pool.RList.key
    }

  let new_context parser_str lst =
    let create_parser_defn str =
      let parser = match str with
      | "" -> NoInputYet
      | _ -> match parse_parser str with
        | Ok parser -> Parsed parser
        | Error msg -> FailedParse msg
      in str, parser
    in

    let evaluations_key = Pool.RList.add evaluations_pool (RList.create lst) in
    { parser_str_key = Pool.Signal.add parser_defn_signal_pool
        (React.S.create (create_parser_defn parser_str))
    ; evaluations_key
    }

  let new_test str = Pool.Signal.add test_signal_pool (React.S.create str)

  type t = parser_context list

  let initial_model = Lvca_languages.Parser.TestParsers.(
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
        let _, evaluations_h = Pool.RList.find_exn Model.evaluations_pool context.evaluations_key in
        RList.snoc key evaluations_h
      | RemoveTest (parser_no, test_no) ->
        let context = List.nth_exn contexts parser_no in
        let _, evaluations_h = Pool.RList.find_exn Model.evaluations_pool context.evaluations_key in
        RList.remove test_no evaluations_h
      | UpdateParser (key, str) ->
        let _, update = Pool.Signal.find_exn Model.parser_defn_signal_pool key in
        let parser_defn = match parse_parser str with
          | Ok parser -> Model.Parsed parser
          | Error msg -> FailedParse msg
        in
        update (str, parser_defn)
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Direct = Lvca_languages.Parser.Direct

  let mk_error msg = [%html{|<div class="error">|}[Html.txt msg]{|</div>|}]

  let view_parser_test parser test =
    let translate_direct, parse_direct = Direct.(translate_direct, parse_direct) in
    match parser with
    | Model.NoInputYet | FailedParse _ -> []
    | Parsed parser ->
      let parser_d = translate_direct parser in
      let elem = match parse_direct parser_d test with
        | Error (msg, tm_opt) ->
          let tm_str = match tm_opt with
            | None -> "(no tm) " ^ msg
            | Some tm -> (Printf.sprintf "%s: %s" msg (Fmt.str "%a" Core.pp tm ))

          in
          mk_error tm_str
        | Ok tm ->
          let str = Fmt.str "parsed: %a" (Nominal.pp_term_ranges Primitive.pp) tm in
          [%html{|<div>|}[Html.txt str]{|</div>|}]
      in
      [elem]

  let mk_context update parser_no Model.{ parser_str_key; evaluations_key } =
    let evaluations_l, _ = Pool.RList.find_exn Model.evaluations_pool evaluations_key in
    let parser_s, _ =
      Pool.Signal.find_exn Model.parser_defn_signal_pool parser_str_key
    in
    let parser_defn_input, parser_defn_input_event = Common.mk_multiline_input
      (React.S.Pair.fst parser_s)
    in

    let new_test_handler _evt = update (Action.AddTest parser_no); false in
    let new_test_button =
      Html.(button ~a:[a_onclick new_test_handler] [txt "new test"])
    in

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
        | FailedParse msg -> [mk_error msg]
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
        let remove_test_button =
          Html.(button ~a:[a_onclick remove_test_handler] [txt "remove test"])
        in

        let output_s = React.S.l2 view_parser_test parser_s test_s in
        Html.(div
          ~a:[a_class ["parser-test-row"]]
          [ test_input
          ; R.Html.div (RList.from_signal output_s)
          ; remove_test_button
          ])
      )
    in

    Html.(div
      ~a:[a_class ["parser-context"]]
      [ parser_defn_input
      ; R.Html.div (RList.from_signal parser_error_elem)
      ; R.Html.div test_elems
      ; new_test_button
      ])

  let view model_s signal_update =
    let update evt = Controller.update evt model_s signal_update in
    let context_elems_s = model_s |> React.S.map (List.mapi ~f:(mk_context update)) in
    let new_parser_handler _evt = update AddParser; false in

    Html.div
      [ Html.(button ~a:[a_onclick new_parser_handler] [txt "create new parser"])
      ; R.Html.div (RList.from_signal context_elems_s)
      ]
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
