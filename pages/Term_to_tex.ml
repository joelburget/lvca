open Base
open Brr
open Note
open Prelude

module Model = struct
  type t = string

  let initial_model = {|\overbrace{a+b+c}^{\text{note}}|}
end

module Action = struct
  type t = Evaluate of string
end

module Controller = struct
  let update (action : Action.t) _model = match action with Evaluate str -> str
end

module View = struct
  let div, h2, h3, txt' = El.(div, h2, h3, txt')

  let view model_s =
    let input, input_event = Multiline_input.mk model_s in
    let katex_area = div [] in
    let _sink : Logr.t = S.log model_s (Katex.render katex_area) in
    let evt : Action.t event =
      input_event
      |> E.filter_map (function
             | Common.EvaluateInput str -> Some (Action.Evaluate str)
             | _ -> None)
    in
    let elem =
      div
        [ h2 [ txt' "Term to TeX" ]
        ; div ~at:[ class' "container" ] [ input ]
        ; div ~at:[ class' "side" ] [ h3 [ txt' "(rendered)" ]; katex_area ]
        ]
    in
    evt, elem
  ;;
end

let stateless_view () =
  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum (S.value model_s) do_action in
    model_s', (model_s', elem)
  in
  let model_s, elem = S.fix Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
