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

  let initial_model =
    [ new_context {|"foo" | "bar"|} [new_test "foo"]
    ; new_context ".*" [new_test "foo"]
    ]
end

module Action = struct
  type parser_no = int
  type test_no = int

  type t =
    | AddParser
    | AddTest of parser_no

    (* | UpdateParser of parser_no * string *)
end

module Controller = struct
  let update (action : Action.t) (model_s : Model.t React.S.t) signal_update =
    let contexts = React.S.value model_s in
    let test_signal_pool = Model.test_signal_pool in

    let model = match action with
      | AddParser
      -> Model.new_context "" [] :: contexts
      | AddTest parser_no
      -> Lvca_util.List.update_nth contexts ~i:parser_no ~f:(fun context ->
        Caml.Printf.printf "adding test\n";
        let key = Pool.Signal.add test_signal_pool (React.S.create "") in
        let _, evaluations_h = Pool.RList.find_exn Model.evaluations_pool context.evaluations_key in
        RList.snoc key evaluations_h;
        context
      )
      (*
      | UpdateParser (i, parser_str) ->
        let Model.{ parser_str_key; _ } = List.nth_exn contexts i in
        let _parser_str_s, parser_str_update =
          Pool.Signal.find_exn parser_defn_signal_pool parser_str_key
        in
        parser_str_update parser_str;
        contexts
        *)
    in
    signal_update model
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  let view model_s signal_update =
    let update evt = Controller.update evt model_s signal_update in

    let mk_error msg = [%html{|<div class="error">|}[Html.txt msg]{|</div>|}] in

    let mk_context parser_no Model.{ parser_str_key; evaluations_key } =
      let evaluations_l, _ = Pool.RList.find_exn Model.evaluations_pool evaluations_key in
      let parser_s, parser_str_update =
        Pool.Signal.find_exn Model.parser_defn_signal_pool parser_str_key
      in
      let parser_defn_input, parser_defn_input_event = Common.mk_multiline_input
        (React.S.Pair.fst parser_s)
      in

      let new_test_handler _evt = update (AddTest parser_no); false in
      let new_test_button =
        Html.(button ~a:[a_onclick new_test_handler] [txt "new test"])
      in

      let (_ : unit React.event) = parser_defn_input_event
        |> React.E.map (function
          | Common.InputUpdate str ->
            let parser_defn = match parse_parser str with
              | Ok parser -> Model.Parsed parser
              | Error msg -> FailedParse msg
            in
            parser_str_update (str, parser_defn)
          | InputSelect _ -> ()
          | InputUnselect -> ()
          )
      in

      let parser_error_elem = parser_s
        |> React.S.map (fun (_, parser) -> match parser with
          | Model.NoInputYet | Parsed _ -> []
          | FailedParse msg -> [mk_error msg]
        )
      in

      let module Direct = Lvca_languages.Parser.Direct in
      let translate_direct, parse_direct = Direct.(translate_direct, parse_direct) in
      let parser_s = React.S.Pair.snd parser_s in

      let test_elems = evaluations_l
        |> RList.map (fun test_key ->
          let test_s, test_update =
            Pool.Signal.find_exn Model.test_signal_pool test_key
          in

          let test_input, test_input_event = Common.mk_single_line_input test_s in

          let (_ : unit React.event) = React.E.map test_update test_input_event in

          let output_s = React.S.l2
            (fun parser test -> match parser with
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
              )
            parser_s
            test_s
          in
          Html.div
            [ test_input
            ; R.Html.div (RList.from_signal output_s)
            ]
        )
      in

      Html.div
        [ parser_defn_input
        ; R.Html.div (RList.from_signal parser_error_elem)
        ; R.Html.div test_elems
        ; new_test_button
        ]
    in

    let context_elems_s = model_s
      |> React.S.map (fun ctx_list -> ctx_list
        |> List.mapi ~f:mk_context
      )
    in

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
