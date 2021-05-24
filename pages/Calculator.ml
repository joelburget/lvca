open Base
open Brr
open Brr_note
open Lvca_syntax
open Note
open Prelude
module Calculator = Lvca_languages.Calculator

module Model = struct
  module Evaluation = struct
    type t =
      { input : string
      ; parsed : Calculator.term
      ; digits : int signal * int S.set
      }

    let ( = ) ev1 ev2 =
      String.(ev1.input = ev2.input)
      && phys_equal ev1.parsed ev2.parsed
      && phys_equal ev1.digits ev2.digits
    ;;
  end

  type t =
    { evaluations : Evaluation.t list
    ; error_msg : string option
    }

  let ( = ) m1 m2 =
    List.equal Evaluation.( = ) m1.evaluations m2.evaluations
    && Option.equal String.( = ) m1.error_msg m2.error_msg
  ;;

  let initial_model : t = { evaluations = []; error_msg = None }
end

module Action = struct
  type t =
    | Evaluate of string
    | ChangePrecision of int * DigitsEntry.digits_update
    | DeleteRow of int
end

module Controller = struct
  let update (action : Action.t) (model : Model.t) =
    let Model.{ evaluations; error_msg } = model in
    match action with
    | Evaluate input ->
      let model =
        match ParseUtil.parse_string Calculator.Parse.t input with
        | Error msg -> Model.{ evaluations; error_msg = Some msg }
        | Ok parsed ->
          let digits = S.create ~eq:Int.( = ) 10 in
          Model.
            { evaluations = { input; parsed; digits } :: evaluations; error_msg = None }
      in
      model
    | DeleteRow i -> { evaluations = Lvca_util.List.remove_nth evaluations i; error_msg }
    | ChangePrecision (i, prec_cmd) ->
      let Model.Evaluation.{ digits; _ } = List.nth_exn evaluations i in
      let digits_s, digits_update = digits in
      (* TODO: there has to be a better way (than using value) *)
      let prev_val = S.value digits_s in
      let new_val =
        match prec_cmd with
        | SetDigits digits -> digits
        | IncrDigits -> Int.succ prev_val
        | DecrDigits -> Int.pred prev_val
      in
      digits_update new_val;
      model
  ;;
end

let mk_example str =
  let result =
    El.code ~at:(classes "bg-gray-50 p-1 font-mono text-sm cursor-pointer") [ txt str ]
  in
  let click_event = Evr.on_el Ev.click (fun _evt -> str) result in
  result, click_event
;;

let row cells = El.tr ~at:[ class' "border-b" ] cells

let language_chart =
  let cell str = El.td ~at:[ class' "py-2" ] [ txt str ] in
  let mono_cell str = El.td ~at:[ class' "py-2"; class' "font-mono" ] [ txt str ] in
  El.table
    ~at:[ class' "w-full" ]
    [ El.thead
        ~at:[ class' "border-b" ]
        [ El.th ~at:[ class' "text-left" ] [ txt "Name" ]
        ; El.th ~at:[ class' "text-left" ] [ txt "Syntax" ]
        ; El.th ~at:[ class' "text-left" ] [ txt "Description" ]
        ]
    ; El.tbody
        [ row [ cell "add"; mono_cell "expr + expr" ]
        ; row [ cell "sub"; mono_cell "expr - expr" ]
        ; row [ cell "mul"; mono_cell "expr * expr" ]
        ; row [ cell "div"; mono_cell "expr / expr" ]
        ; row [ cell "negate"; mono_cell "negate expr" ]
        ; row [ cell "sqrt"; mono_cell "sqrt expr" ]
        ; row [ cell "abs"; mono_cell "abs expr" ]
        ; row [ cell "exp"; mono_cell "exp expr" ]
        ; row [ cell "ln"; mono_cell "ln expr" ]
        ; row [ cell "sin"; mono_cell "sin expr" ]
        ; row [ cell "cos"; mono_cell "cos expr" ]
        ; row [ cell "tan"; mono_cell "tan expr" ]
        ; row [ cell "asin"; mono_cell "asin expr"; cell "arcsin ie inverse sin" ]
        ; row [ cell "acos"; mono_cell "acos expr"; cell "arccos ie inverse cos" ]
        ; row [ cell "atan"; mono_cell "atan expr"; cell "arctan ie inverse tan" ]
        ; row [ cell "pi"; mono_cell "pi" ]
        ; row [ cell "e"; mono_cell "e" ]
        ]
    ]
;;

module View = struct
  let div, span, p, h3, button, ul, li, td, th, pre =
    El.(div, span, p, h3, button, ul, li, td, th, pre)
  ;;

  let view model_s =
    let input, input_event = SingleLineInput.mk (S.const ~eq:String.( = ) "1 + 1") in
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
    let examples = ul (examples |> List.map ~f:(fun example -> li [ example ])) in
    let row' row_num input_str parsed digits_s _digits_update =
      let digits_entry, digits_event = DigitsEntry.mk digits_s in
      let digits_s =
        digits_s
        |> S.map ~eq:Common.html_eq (fun digits ->
               match Calculator.interpret parsed with
               | Error (_tm, msg) -> span ~at:[ class' "error" ] [ txt msg ]
               | Ok real ->
                 let str =
                   ConstructiveReal.eval_to_string real ~digits:(Int32.of_int_exn digits)
                 in
                 span [ txt str ])
      in
      let digits' =
        mk_reactive'
          pre
          ~eq:Common.htmls_eq
          ~at:(classes "whitespace-pre-wrap break-all")
          digits_s
      in
      let delete_button =
        button
          ~at:(classes "inline-block p-1 border-2 border-indigo-900 rounded")
          [ txt "remove" ]
      in
      let evts =
        E.select
          [ digits_event |> E.map (fun update -> Action.ChangePrecision (row_num, update))
          ; Evr.on_el Ev.click (fun _evt -> Action.DeleteRow row_num) delete_button
          ]
      in
      let elem =
        row
          [ td
              ~at:(classes "py-4 pr-1")
              [ pre ~at:(classes "whitespace-pre-wrap break-word") [ txt input_str ] ]
          ; td ~at:(classes "py-4 pr-1") [ digits' ]
          ; td ~at:(classes "py-4 pr-1") [ digits_entry ]
          ; td [ delete_button ]
          ]
      in
      elem, evts
    in
    let thead =
      row
        [ th ~at:(classes "w-1/2 text-left") [ txt "input" ]
        ; th ~at:(classes "w-1/3 text-left") [ txt "output" ]
        ; th ~at:(classes "w-1/12 text-left") [ txt "digits" ]
        ; th ~at:(classes "w-1/12") []
        ]
    in
    let tbody, tbody_evts =
      let never_eq _ _ = false in
      let eq = Lvca_util.Tuple2.equal Common.htmls_eq never_eq in
      let s =
        model_s
        |> S.map ~eq (fun model ->
               model.Model.evaluations
               |> List.mapi ~f:(fun row_num Model.Evaluation.{ input; parsed; digits } ->
                      let digits_s, digits_update = digits in
                      row' row_num input parsed digits_s digits_update)
               |> List.unzip)
      in
      S.Pair.fst ~eq:Common.htmls_eq s, S.Pair.snd ~eq:never_eq s
    in
    let error_msg =
      model_s
      |> S.map ~eq:Common.htmls_eq (fun model ->
             match model.Model.error_msg with
             | None -> []
             | Some msg -> [ span [ txt msg ] ])
      |> mk_reactive div
    in
    let actions =
      E.select
        [ input_event
          |> E.filter_map (function
                 | Common.InputUpdate str -> Some (Action.Evaluate str)
                 | _ -> None)
        ; example_update_es |> E.select |> E.map (fun example -> Action.Evaluate example)
        ; tbody_evts
          |> S.map ~eq:phys_equal E.select (* Select one event from a list of events *)
          |> E.swap
          (* Extract current signal's event *)
        ]
    in
    let elem =
      div
        [ div ~at:[ class' "my-2" ] [ input ]
        ; div ~at:(classes "error my-2") [ error_msg ]
        ; Components.table ~classes:[ "w-full"; "mb-6" ] thead tbody
        ; div
            ~at:[ class' "my-2" ]
            [ p [ txt "Try an example" ]
            ; examples
            ; h3 [ txt "language chart" ]
            ; language_chart
            ]
        ]
    in
    actions, elem
  ;;
end

let stateless_view () =
  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum ~eq:Model.( = ) (S.value model_s) do_action in
    model_s', (model_s', elem)
  in
  let model_s, elem = S.fix ~eq:Model.( = ) Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
