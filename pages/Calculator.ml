open Base
open Lvca_syntax

module Model = struct
  type t = string * int

  let initial_model = {|1 + 1|}, 10
end

module Action = struct
  type digits_update =
    | SetDigits of int
    | IncrDigits
    | DecrDigits

  type t =
    | Evaluate of string
    | ChangePrecision of digits_update
end

module Controller = struct
  let update (action : Action.t) model_s signal_update =
    (* let open Model in *)
    let str, digits = React.S.value model_s in
    let new_model = match action with
      | Evaluate str -> str, digits
      | ChangePrecision (SetDigits digits)
      -> str, digits
      | ChangePrecision IncrDigits
      -> str, digits + 1
      | ChangePrecision DecrDigits
      -> str, digits - 1
    in
    signal_update new_model
  ;;
end

module View = struct
  open Js_of_ocaml
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events
  module Parse = Lvca_languages.Calculator.Parse (ParseUtil.CComment)

  let mk_digits_entry digits_s =
    let digits_event, signal_digits_event = React.E.create () in
    let input = [%html{|<input type="text">|}] in
    let input_dom = To_dom.of_input input in
    Common.bind_event Ev.keydowns input_dom (fun evt ->
      let key_name = evt##.code
        |> Js.Optdef.to_option
        |> Option.value_exn
        |> Js.to_string
      in
      let result = match key_name with
        | "Enter" -> (
         Dom.preventDefault evt;
         try
           signal_digits_event
             (Action.SetDigits (Int.of_string (Js.to_string input_dom##.value)))
         with
           _ -> ()
        )
        | "ArrowUp" | "ArrowRight" ->
          Dom.preventDefault evt;
          signal_digits_event Action.IncrDigits
        | "ArrowDown" | "ArrowLeft" ->
          Dom.preventDefault evt;
          signal_digits_event Action.DecrDigits
        | _ -> ()
      in
      Lwt.return result
    );

    let (_: unit React.event) = digits_s
      (* Create an event when the input has changed *)
      |> React.S.map ~eq:Caml.(=) Fn.id
      |> React.S.changes
      |> React.E.map (fun i -> input_dom##.value := Js.string (Int.to_string i))
    in
    input, digits_event

  let view model_s signal_update =
    let input, input_event = Common.mk_input (model_s |> React.S.map fst) in
    let digits_entry, digits_event = mk_digits_entry (model_s |> React.S.map snd) in

    let (_ : unit React.event) = input_event
      |> React.E.map (function
        | Common.InputUpdate str ->
          Controller.update (Action.Evaluate str) model_s signal_update
        | _ -> ())
    in

    let (_ : unit React.event) = digits_event
      |> React.E.map (fun update ->
        Controller.update (ChangePrecision update) model_s signal_update)
    in

    let result = model_s
      |> React.S.map (fun (str, digits) -> match ParseUtil.parse_string Parse.t str with
        | Error msg -> msg
        | Ok tm -> match Lvca_languages.Calculator.interpret tm with
          | Error (_tm, msg) -> msg
          | Ok real ->
            let digits = Int32.of_int_exn digits in
            Constructive_real.ConstructiveReal.eval_to_string real ~digits
      )
    in

    let eval_button = Html.(button [txt "evaluate"]) in

    let instructions = match WebUtil.platform_special_combo () with
      | None -> Html.txt ""
      | Some instructions -> Html.(span ([ txt "by pressing " ] @ instructions))
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
        <div>
          <p>|}[eval_button; instructions]{|</p>
          <p>digits: |}[digits_entry]{|</p>
        </div>
      </div>
    |}]
  ;;
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
