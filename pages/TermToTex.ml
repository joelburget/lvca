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
  let update (action : Action.t) _model_s signal_update =
    (* let open Model in *)
    let new_model = match action with Evaluate str -> str in
    signal_update new_model
  ;;
end

module View = struct
  let view model_s signal_update =
    let katex_area = El.div [] in
    (* let katex_dom = To_dom.of_div katex_area in *)
    let set_katex = Katex.render katex_area in
    let input, input_event = MultilineInput.mk model_s in
    let (_ : unit event) =
      input_event
      |> E.map (function
             | Common.InputUpdate str ->
               set_katex str;
               (* XXX *)
               Controller.update (Action.Evaluate str) model_s signal_update
             | _ -> ())
    in

    let div, h2, h3 = El.(div, h2, h3) in
    div
      [ h2 [ txt "Term to TeX"]
      ; div ~at:[class' "container"] [input]
      ; div ~at:[class' "side"]
        [ h3 [ txt "(rendered)" ]
        ; katex_area
        ]
      ]
  ;;
end

let stateless_view () =
  let model_s, signal_update = S.create Model.initial_model in
  View.view model_s signal_update
;;
