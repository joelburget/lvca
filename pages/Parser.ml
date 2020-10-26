open Base
open Lvca_core
open Lvca_syntax

module ParseCore = Core.Parse (ParseUtil.CComment)
module ParseParser = Lvca_languages.Parser.Parse (ParseUtil.CComment)

let parse_parser str = ParseUtil.parse_string (ParseParser.t ParseCore.term) str

module Model = struct
  type t =
    { parser_str: string
    ; parser: (Lvca_languages.Parser.t, string) Result.t
    ; test: string
    }
  let parser_str = {|"foo" | "bar"|}
  let initial_model =
    { parser_str
    ; parser = parse_parser parser_str
    ; test = "foo"
    }
end

module Action = struct
  type t =
    | UpdateParser of string
    | UpdateTest of string
end

module Controller = struct
  let update (action : Action.t) (model_s : Model.t React.S.t) signal_update =
    let Model.{ parser_str; parser; test } = React.S.value model_s in

    let model = match action with
      | UpdateParser parser_str
      ->
        let parser = parse_parser parser_str in
        Model.{ parser_str; parser; test }
      | UpdateTest test -> { parser_str; parser; test }
    in
    signal_update model
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  let view model_s signal_update =
    let parser_defn_input, parser_defn_input_event =
      (* TODO: this seems like an antipattern: these values should be synce automatically.
         Also weird we're not using model_s here *)
      Common.mk_multiline_input (React.S.const Model.initial_model.parser_str)
    in
    let test_input, test_input_event =
      Common.mk_single_line_input (React.S.const Model.initial_model.test)
    in

    let (_ : unit React.event) = parser_defn_input_event
      |> React.E.map (function
        | Common.InputUpdate str
        -> Controller.update (UpdateParser str) model_s signal_update
        | InputSelect _ -> ()
        | InputUnselect -> ()
        )
    in

    let (_ : unit React.event) = test_input_event
      |> React.E.map (fun str -> Controller.update (UpdateTest str) model_s signal_update)
    in

    let module Direct = Lvca_languages.Parser.Direct in

    let empty_div = [%html{|<div></div>|}] in

    (* XXX: clean up extra divs and stuff *)
    let parser_parse_err, test_output =
      let signals = model_s
        |> React.S.map (fun Model.{ parser; test; _ } -> match parser with
          | Error msg -> [%html{|<div>|}[Html.txt msg]{|</div>|}], empty_div
          | Ok parser ->
              let output =
                match Direct.parse_direct (Direct.translate_direct parser) test with
                | Error (msg, _) -> msg
                | Ok tm -> Fmt.str "%a" (Nominal.pp_term_ranges Primitive.pp) tm
              in
              empty_div, [%html{|<div>|}[Html.txt output]{|</div>|}]
        )
      in
      ReactiveData.RList.singleton_s (React.S.Pair.fst signals),
        ReactiveData.RList.singleton_s (React.S.Pair.snd signals)
    in

    [%html{|
      <div>
        |}
          [ parser_defn_input
          ; R.Html.div parser_parse_err
          ; test_input
          ; R.Html.div test_output
          ]
        {|
      </div>
    |}]
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
