open Base
open Lvca_syntax
module Calculator = Lvca_languages.Calculator
module Parse = Calculator.Parse (ParseUtil.CComment)

module IntMap = struct
  type 'a t = (int, 'a, Base.Int.comparator_witness) Base.Map.t
  let empty = Base.Map.empty (module Base.Int)
end

module Model = struct
  type signal = int React.S.t * (?step:React.step -> int -> unit)
  type signal_pool = signal IntMap.t

  module Evaluation = struct
    type t =
      { input: string
      ; parsed: Calculator.term
      ; pool_idx: int
      }
  end

  type t =
    { evaluations: Evaluation.t list
    ; pool_idx: int
    ; error_msg: string option
    }

  let initial_model : t =
    { evaluations = []; pool_idx = 0; error_msg = None }
end

module Action = struct
  type t =
    | Evaluate of string
    | ChangePrecision of int * Common.digits_update
    | DeleteRow of int
end

module Controller = struct
  let signal_pool = ref IntMap.empty

  let update (action : Action.t) (model_s : Model.t React.S.t) signal_update =
    let Model.{ evaluations; pool_idx; error_msg }
      = React.S.value model_s
    in

    match action with
      | Evaluate input ->
        let model = match ParseUtil.parse_string Parse.t input with
          | Error msg -> Model.{ evaluations; pool_idx; error_msg = Some msg }
          | Ok parsed ->
            signal_pool := Map.set !signal_pool ~key:pool_idx ~data:(React.S.create 10);
            Model.
              { evaluations = { input; parsed; pool_idx } :: evaluations
              ; pool_idx = Int.succ pool_idx
              ; error_msg = None
              }
        in signal_update model
      | DeleteRow i ->
        let Model.Evaluation.{ pool_idx; _ } = List.nth_exn evaluations i in
        signal_pool := Map.remove !signal_pool pool_idx;

        signal_update
          { evaluations = Lvca_util.List.remove_nth evaluations i
          ; pool_idx
          ; error_msg
          }
      | ChangePrecision (i, prec_cmd) ->
        let Model.Evaluation.{ pool_idx; _ } = List.nth_exn evaluations i in
        let digits_s, digits_update = Map.find_exn !signal_pool pool_idx in
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
    <table class="language-chart">
      <thead>
        <tr>
          <th>Name</th>
          <th>Syntax</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        <tr> <td>add</td> <td class="syntax">expr + expr</td> </tr>
        <tr> <td>sub</td> <td class="syntax">expr - expr</td> </tr>
        <tr> <td>mul</td> <td class="syntax">expr * expr</td> </tr>
        <tr> <td>div</td> <td class="syntax">expr / expr</td> </tr>

        <tr> <td>negate</td> <td class="syntax">negate expr</td> </tr>
        <tr> <td>sqrt</td>   <td class="syntax">sqrt expr</td> </tr>
        <tr> <td>abs</td>    <td class="syntax">abs expr</td> </tr>
        <tr> <td>exp</td>    <td class="syntax">exp expr</td> </tr>
        <tr> <td>ln</td>     <td class="syntax">ln expr</td> </tr>
        <tr> <td>sin</td>    <td class="syntax">sin expr</td> </tr>
        <tr> <td>cos</td>    <td class="syntax">cos expr</td> </tr>
        <tr> <td>tan</td>    <td class="syntax">tan expr</td> </tr>
        <tr> <td>asin</td>   <td class="syntax">asin expr</td> <td>arcsin ie inverse sin</td> </tr>
        <tr> <td>acos</td>   <td class="syntax">acos expr</td> <td>arccos ie inverse cos</td> </tr>
        <tr> <td>atan</td>   <td class="syntax">atan expr</td> <td>arctan ie inverse tan</td> </tr>

        <tr> <td>pi</td> <td class="syntax">pi</td> </tr>
        <tr> <td>e</td>  <td class="syntax">e</td> </tr>
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
      Html.(
        ul
          (examples
          |> List.map ~f:(fun example -> li ~a:[ a_class [ "example" ] ] [ example ])))
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
        |> ReactiveData.RList.from_signal
        |> R.Html.pre
      in

      let delete_button, delete_event =
        mk_button ~cls:["result-delete"] Html.[ txt "remove" ]
      in
      let (_ : unit React.event) = delete_event
        |> React.E.map (fun () ->
            Controller.update (DeleteRow row_num) model_s signal_update)
      in

      [%html{|
        <tr class="row">
          <td class="result-input"> <pre>|}[ Html.txt input_str ]{|</pre> </td>
          <td class="result-output">|}[ digits' ]{|</td>
          <td class="result-digits">digits: |}[ digits_entry ]{|</td>
          <td>|}[ delete_button ]{|</td>
        </tr>
      |}]
    in

    let rows = model_s
      |> React.S.map (fun model ->
        model.Model.evaluations
        |> List.mapi ~f:(fun row_num { input; parsed; pool_idx }  ->
            let digits_s, digits_update =
              Map.find_exn !Controller.signal_pool pool_idx
            in
            row row_num input parsed digits_s digits_update))
      |> ReactiveData.RList.from_signal
    in

    let error_msg = model_s
      |> React.S.map (fun model -> match model.Model.error_msg with
        | None -> []
        | Some msg -> [Html.(span [txt msg])])
      |> ReactiveData.RList.from_signal
      |> R.Html.div
    in

    [%html {|
      <div>
        <div>|}[ input ]{|</div>
        <div class="error">|}[ error_msg ]{|</div>
        |}[ R.Html.table rows ]{|
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
