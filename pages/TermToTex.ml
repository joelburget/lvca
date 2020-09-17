open Base
(* open Js_of_ocaml *)
(* open Lvca_syntax *)
(* open ReactiveData *)

module Model = struct
  type t = string

  let initial_model = {|\overbrace{a+b+c}^{\text{note}}|}
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit

module Action = struct
  type t = Evaluate of string
end

module Controller = struct
  let update (action : Action.t) _model_s signal_update =
    (* let open Model in *)
    let new_model = match action with
      | Evaluate str -> str
    in
    signal_update new_model
  ;;
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let view _model_s _signal_update =
    Html5.(
      div
        [ h2 [ txt "Term to TeX" ]
        (*
        ; div
            ~a:[ a_class [ "container" ] ]
            [ div
                ~a:[ a_class [ "side" ] ]
                [ h3 [ R.Html5.txt input_desc ]; mk_input model_s signal_update ]
            ; div
                ~a:[ a_class [ "switch-languages" ] ]
                [ button
                    ~a:
                      [ a_onclick (fun _evt ->
                            Controller.update SwitchInputLang model_s signal_update;
                            false)
                      ]
                    [ txt "switch input languages" ]
                ]
            ; div
                ~a:[ a_class [ "side" ] ]
                [ h3 [ R.Html5.txt output_desc ]; mk_output model_s ]
            ]
        *)
        ])
  ;;
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
