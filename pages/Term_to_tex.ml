open Base
open Brr
open Note
open Prelude

module Model = struct
  type t = string

  let initial_model = {|\overbrace{a+b+c}^{\text{note}}|}
  let ( = ) = String.( = )
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
             | Common.Evaluate_input str -> Some (Action.Evaluate str)
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

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
