open Base
open Lvca_syntax

(* TODO: consider blocking asking for more than, say 5000 digits? *)

module Model = struct
  type evaluation = string * int
  type t = evaluation Queue.t

  let initial_model : t = Queue.create () (* Queue.of_list [{|1 + 1|}, 10] *)
end

module Action = struct
  type t =
    | Evaluate of string
    | ChangePrecision of int * Common.digits_update
end

module Controller = struct
  let update (action : Action.t) model_s signal_update =
    let queue = React.S.value model_s |> Queue.copy in
    let new_model =
      let () = match action with
      | Evaluate str -> Queue.enqueue queue (str, 10)
      | ChangePrecision (i, prec_cmd) ->
        let str, digits = Queue.get queue i in
        let digits' = match prec_cmd with
          | SetDigits digits -> digits
          | IncrDigits -> digits + 1
          | DecrDigits -> digits - 1
        in
        Queue.set queue i (str, digits')
      in

      queue
    in
    signal_update new_model
  ;;
end

let mk_example str =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let result = Html.(code [ txt str ]) in
  let result_dom = To_dom.of_code result in
  let click_event, signal_event = React.E.create () in
  Common.bind_event Common.Ev.clicks result_dom (fun _evt ->
      signal_event str;
      Lwt.return ());
  result, click_event
;;

let language_chart =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  [%html{|
    <table class="language_chart">
      <thead>
        <tr>
          <th>Name</th>
          <th>Syntax</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        <tr> <td>add</td> <td>expr + expr</td> <td></td> </tr>
        <tr> <td>sub</td> <td>expr - expr</td> <td></td> </tr>
        <tr> <td>mul</td> <td>expr * expr</td> <td></td> </tr>
        <tr> <td>div</td> <td>expr / expr</td> <td></td> </tr>

        <tr> <td>negate</td> <td>- expr</td> <td></td> </tr>
        <tr> <td>sqrt</td> <td>sqrt expr</td> <td></td> </tr>
        <tr> <td>abs</td> <td>abs expr</td> <td></td> </tr>
        <tr> <td>exp</td> <td>exp expr</td> <td></td> </tr>
        <tr> <td>ln</td> <td>ln expr</td> <td></td> </tr>
        <tr> <td>sin</td> <td>sin expr</td> <td></td> </tr>
        <tr> <td>cos</td> <td>cos expr</td> <td></td> </tr>
        <tr> <td>tan</td> <td>tan expr</td> <td></td> </tr>
        <tr> <td>asin</td> <td>asin expr</td> <td>arcsin ie inverse sin</td> </tr>
        <tr> <td>acos</td> <td>acos expr</td> <td>arccos ie inverse cos</td> </tr>
        <tr> <td>atan</td> <td>atan expr</td> <td>arctan ie inverse tan</td> </tr>

        <tr> <td>pi</td> <td>pi</td> </tr>
        <tr> <td>e</td> <td>e</td> </tr>
      </tbody>
    </table>
  |}]
;;

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events
  module Parse = Lvca_languages.Calculator.Parse (ParseUtil.CComment)

  let view model_s signal_update =
    let input, input_event = Common.mk_input (React.S.const "1 + 1") in

    let (_ : unit React.event) =
      input_event
      |> React.E.map (function
             | Common.InputUpdate str ->
               Controller.update (Evaluate str) model_s signal_update
             | _ -> ())
    in

    let eval_button = Html.(button [ txt "evaluate" ]) in
    let instructions =
      match WebUtil.platform_special_combo () with
      | None -> Html.txt ""
      | Some instructions -> Html.(span ([ txt "by pressing " ] @ instructions))
    in
    let examples, example_update_es =
      [ "1 + 1"; "pi"; "cos (pi / 4)"; "sqrt 2 / 2" (* ; "ln (e * e)" *) ]
      |> List.map ~f:mk_example
      |> List.unzip
    in
    let (_ : unit React.event) =
      example_update_es
      |> React.E.select
      |> React.E.map (fun example ->
             Controller.update (Evaluate example) model_s signal_update)
    in
    let examples =
      Html.(
        ul
          (examples
          |> List.map ~f:(fun example -> li ~a:[ a_class [ "example" ] ] [ example ])))
    in

    let result str digits = match ParseUtil.parse_string Parse.t str with
      | Error msg -> msg
      | Ok tm ->
        (match Lvca_languages.Calculator.interpret tm with
        | Error (_tm, msg) -> msg
        | Ok real ->
          let digits = Int32.of_int_exn digits in
          ConstructiveReal.eval_to_string real ~digits)
    in

    let row row_num input digits =
      (* TODO: const here is hacky? *)
      let digits_entry, digits_event = Common.mk_digits_entry (React.S.const digits) in
      let (_ : unit React.event) = digits_event
        |> React.E.map (fun update ->
           Controller.update (ChangePrecision (row_num, update)) model_s signal_update)
      in

      (*
      let digits' = digits
        |> React.S.map (result input)
        |> R.Html.txt
      in
      *)

      [%html{|
        <div class="row column-container">
          <div>
            <pre>|}[ Html.txt input ]{|</pre>
          </div>
          <div>
            <pre>|}[ Html.txt (result input digits) ]{|</pre>
          </div>
          <p>digits: |}[ digits_entry ]{|</p>
        </div>
      |}]
    in

    let rows = model_s
      |> React.S.map (fun q -> q
        |> Queue.to_list
        |> List.mapi ~f:(fun row_num (input, digits) -> row row_num input digits))
      |> ReactiveData.RList.from_signal
      |> R.Html.div
    in

    [%html {|
      <div>
        <h2>Calculator</h2>
        <div class="row-container">
          <div>|}[ input ]{|</div>
          |}[ rows ]{|
        </div>
        <div>
          <p>|}[ eval_button; instructions ]{|</p>
          <p>Try an example:</p>
          |}[ examples ]{|
          <h3>language chart</h3>
          |}[ language_chart ]{|
        </div>
      </div>
    |}]
  ;;
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
