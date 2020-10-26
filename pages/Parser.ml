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
      | UpdateParser parser_str ->
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
      (* TODO: this seems like an antipattern: these values should be synced
         automatically. Also weird we're not using model_s here *)
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

    let elems_signal = model_s
      |> React.S.map (fun Model.{ parser; test; _ } -> match parser with
        | Error msg ->
          [ parser_defn_input
          ; [%html{|<div class="error">|}[Html.txt msg]{|</div>|}]
          ; test_input
          ]
        | Ok parser ->
          let parse_direct, translate_direct =
            Lvca_languages.Parser.Direct.(parse_direct, translate_direct)
          in
          let output = match parse_direct (translate_direct parser) test with
            | Error (msg, _) -> [%html{|<div class="error">|}[Html.txt msg]{|</div>|}]
            | Ok tm ->
              let str = Fmt.str "%a" (Nominal.pp_term_ranges Primitive.pp) tm in
              [%html{|<div>|}[Html.txt str]{|</div>|}]
          in
          [ parser_defn_input
          ; test_input
          ; output
          ]
      )
    in


    R.Html.div (ReactiveData.RList.from_signal elems_signal)
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
