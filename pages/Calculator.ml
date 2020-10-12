open Base
open Lvca_syntax

module Model = struct
  type t = string

  let initial_model = {|1 + 1|}
end

module Action = struct
  type t = Evaluate of string
end

module Controller = struct
  let update (action : Action.t) _model_s signal_update =
    (* let open Model in *)
    let new_model = match action with Evaluate str -> str in
    signal_update new_model
  ;;
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events
  module Parse = Lvca_languages.Calculator.Parse (ParseUtil.CComment)

  let view model_s signal_update =
    let input, input_event = Common.mk_input model_s in
    let (_ : unit React.event) = input_event
      |> React.E.map (function
        | Common.InputUpdate str ->
          Controller.update (Action.Evaluate str) model_s signal_update
        | _ -> ())
    in

    let result = model_s
      |> React.S.map (fun str -> match ParseUtil.parse_string Parse.t str with
        | Error msg -> msg
        | Ok tm -> match Lvca_languages.Calculator.interpret tm with
          | Error (_tm, msg) -> msg
          | Ok real -> Constructive_real.ConstructiveReal.eval_to_string real
      )
    in

    [%html {|
      <div>
        <h2>Calculator</h2>
        <div class="container">
          <div class="side">
            <h3>|}[ input ]{|</h3>
          </div>
          <div class="side">
            <h3>result</h3>
            |}[ R.Html.txt result ]{|
          </div>
        </div>
      </div>
    |}]
  ;;
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
