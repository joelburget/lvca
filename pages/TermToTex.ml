open Base

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
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let view model_s signal_update =
    let katex_area = Html5.div [] in
    let katex_dom = To_dom.of_div katex_area in
    let set_katex = Katex.render katex_dom in
    let input, input_event = Common.mk_multiline_input model_s in
    let (_ : unit React.event) =
      input_event
      |> React.E.map (function
             | Common.InputUpdate str ->
               Caml.Printf.printf {|printing "%s"\n|} str;
               set_katex str;
               (* XXX *)
               Controller.update (Action.Evaluate str) model_s signal_update
             | _ -> ())
    in

    [%html {|
      <div>
        <h2>Term to TeX</h2>
        <div class="container">
          <div class="side">
            <h3>|}[ input ]{|</h3>
          </div>
          <div class="side">
            <h3>output (rendered)</h3>
            |}[ katex_area ]{|
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
