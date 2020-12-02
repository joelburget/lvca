open Base
open Lvca_syntax
open ReactiveData
module Calculator = Lvca_languages.Calculator
module Parse = Calculator.Parse (ParseUtil.CComment)

module Model = struct
  type digits_signal_pool = int Pool.Signal.t

  module Evaluation = struct
    type t =
      { input: string
      ; parsed: Calculator.term
      ; pool_key: Pool.Signal.key
      }
  end

  type t =
    { evaluations: Evaluation.t list
    ; error_msg: string option
    }

  let initial_model : t = { evaluations = []; error_msg = None }
  let digits_signal_pool = Pool.Signal.create ()
end

module Action = struct
  type t =
    | Evaluate of string
    | ChangePrecision of int * Common.digits_update
    | DeleteRow of int
end

module Controller = struct
  let update (action : Action.t) (model_s : Model.t React.S.t) signal_update =
    let Model.{ evaluations; error_msg } = React.S.value model_s in
    let digits_signal_pool = Model.digits_signal_pool in

    match action with
      | Evaluate input ->
        let model = match ParseUtil.parse_string Parse.t input with
          | Error msg -> Model.{ evaluations; error_msg = Some msg }
          | Ok parsed ->
            let pool_key = Pool.Signal.add digits_signal_pool (React.S.create 10) in
            Model.
              { evaluations = { input; parsed; pool_key } :: evaluations
              ; error_msg = None
              }
        in signal_update model
      | DeleteRow i ->
        let Model.Evaluation.{ pool_key; _ } = List.nth_exn evaluations i in
        Pool.Signal.remove digits_signal_pool pool_key;

        signal_update { evaluations = Lvca_util.List.remove_nth evaluations i; error_msg }
      | ChangePrecision (i, prec_cmd) ->
        let Model.Evaluation.{ pool_key; _ } = List.nth_exn evaluations i in
        let digits_s, digits_update = Pool.Signal.find_exn digits_signal_pool pool_key in
        (* TODO: there has to be a better way (than using value) *)
        let prev_val = React.S.value digits_s in
        let new_val = match prec_cmd with
          | SetDigits digits -> digits
          | IncrDigits -> Int.succ prev_val
          | DecrDigits -> Int.pred prev_val
        in
        digits_update new_val
  ;;
end

let mk_example str =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let result = Html.(code
    ~a:[a_class ["bg-gray-50"; "p-1"; "font-mono"; "text-sm"; "cursor-pointer"]]
    [ txt str ])
  in
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
    <table class="w-full">
      <thead>
        <tr class="border-b">
          <th>Name</th>
          <th>Syntax</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        <tr class="border-b"> <td>add</td> <td class="font-mono">expr + expr</td> </tr>
        <tr class="border-b"> <td>sub</td> <td class="font-mono">expr - expr</td> </tr>
        <tr class="border-b"> <td>mul</td> <td class="font-mono">expr * expr</td> </tr>
        <tr class="border-b"> <td>div</td> <td class="font-mono">expr / expr</td> </tr>

        <tr class="border-b"> <td>negate</td> <td class="font-mono">negate expr</td> </tr>
        <tr class="border-b"> <td>sqrt</td>   <td class="font-mono">sqrt expr</td> </tr>
        <tr class="border-b"> <td>abs</td>    <td class="font-mono">abs expr</td> </tr>
        <tr class="border-b"> <td>exp</td>    <td class="font-mono">exp expr</td> </tr>
        <tr class="border-b"> <td>ln</td>     <td class="font-mono">ln expr</td> </tr>
        <tr class="border-b"> <td>sin</td>    <td class="font-mono">sin expr</td> </tr>
        <tr class="border-b"> <td>cos</td>    <td class="font-mono">cos expr</td> </tr>
        <tr class="border-b"> <td>tan</td>    <td class="font-mono">tan expr</td> </tr>
        <tr class="border-b"> <td>asin</td>   <td class="font-mono">asin expr</td> <td>arcsin ie inverse sin</td> </tr>
        <tr class="border-b"> <td>acos</td>   <td class="font-mono">acos expr</td> <td>arccos ie inverse cos</td> </tr>
        <tr class="border-b"> <td>atan</td>   <td class="font-mono">atan expr</td> <td>arctan ie inverse tan</td> </tr>

        <tr class="border-b"> <td>pi</td> <td class="font-mono">pi</td> </tr>
        <tr class="border-b"> <td>e</td>  <td class="font-mono">e</td> </tr>
      </tbody>
    </table>
  |}]
;;

let mk_button ?cls:(cls=[]) contents =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let click_event, signal_event = React.E.create () in
  let handler _evt = signal_event (); false in
  let button = Html.(button ~a:[a_onclick handler; a_class cls] contents) in
  button, click_event

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let view model_s signal_update =
    let input, input_event = Common.mk_single_line_input (React.S.const "1 + 1") in

    let (_ : unit React.event) = input_event
      |> React.E.map (fun str -> Controller.update (Evaluate str) model_s signal_update)
    in

    let examples, example_update_es =
      [ "pi"
      ; "cos (pi / 4)"
      ; "sqrt 2 / 2"
      ; "4 * (4 * atan (1 / 5) - atan (1 / 239))"
      ; "20 * atan (1 / 7) + 8 * atan (3 / 79)"
      ; "ln (e * e)"
      ; "(1.356 + 1.355) / 2.0"
      ]
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
      Html.(ul (examples |> List.map ~f:(fun example -> li [ example ])))
    in

    let row row_num input_str parsed digits_s _digits_update =
      let digits_entry, digits_event = Common.mk_digits_entry digits_s in
      let (_ : unit React.event) = digits_event
        |> React.E.map (fun update ->
           Controller.update (ChangePrecision (row_num, update)) model_s signal_update)
      in

      let digits' = digits_s
        |> React.S.map (fun digits -> match Calculator.interpret parsed with
          | Error (_tm, msg) -> [Html.(span ~a:[a_class ["error"]] [txt msg])]
          | Ok real ->
            let str =
              ConstructiveReal.eval_to_string real ~digits:(Int32.of_int_exn digits)
            in
            [Html.(span [txt str])])
        |> RList.from_signal
        |> R.Html.pre ~a:[Html.a_class ["whitespace-pre-wrap"; "break-all"]]
      in

      let delete_button, delete_event = mk_button
        ~cls:["inline-block p-1 border-2 border-indigo-900 rounded"]
        [ Html.txt "remove" ]
      in
      let (_ : unit React.event) = delete_event
        |> React.E.map (fun () ->
            Controller.update (DeleteRow row_num) model_s signal_update)
      in

      [%html{|
        <tr class="border-b">
          <td class="p-4">
            <pre class="whitespace-pre-wrap break-word">|}[ Html.txt input_str ]{|</pre>
          </td>
          <td class="p-4">|}[ digits' ]{|</td>
          <td class="p-4">|}[ digits_entry ]{|</td>
          <td>|}[ delete_button ]{|</td>
        </tr>
      |}]
    in

    let thead = [%html{|
      <tr class="border-b">
        <th class="w-1/2">input</th>
        <th class="w-1/3">output</th>
        <th class="w-1/12">digits</th>
        <th class="w-1/12"></th>
      </tr>
      |}]
    in

    let tbody = model_s
      |> React.S.map (fun model ->
        model.Model.evaluations
        |> List.mapi ~f:(fun row_num { input; parsed; pool_key }  ->
            let digits_s, digits_update =
              Pool.Signal.find_exn Model.digits_signal_pool pool_key
            in
            row row_num input parsed digits_s digits_update))
      |> RList.from_signal
    in

    let error_msg = model_s
      |> React.S.map (fun model -> match model.Model.error_msg with
        | None -> []
        | Some msg -> [Html.(span [txt msg])])
      |> RList.from_signal
      |> R.Html.div
    in

    [%html {|
      <div>
        <div>|}[ input ]{|</div>
        <div class="error">|}[ error_msg ]{|</div>
        |}[ Components.table ~classes:["w-full"; "mb-6"] thead tbody ]{|
        <div>
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
