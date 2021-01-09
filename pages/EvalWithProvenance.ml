open Base
open Common
open Lvca_syntax
open Note
open Prelude
open Result.Let_syntax

module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

let eval = Lvca_languages.LambdaCalculus.eval

module Model = struct
  type t =
    { input : string
    ; result : (term, string) Result.t
    ; selected : OptRange.t
    }

  let print { input; result; selected } =
    Fmt.pr
      "{ input = %s; result = %a; selected = %a }\n"
      input
      (fun ppf tm_result ->
        match tm_result with
        | Error msg -> Fmt.pf ppf "%s" msg
        | Ok tm -> Nominal.pp_term Primitive.pp ppf tm)
      result
      OptRange.pp
      selected
  ;;
end

module Controller = struct
  let update (action : Action.t) Model.{ input; result; selected } =
    let open Model in
    match action with
      | Evaluate str ->
        let result = str
          |> ParseUtil.parse_string LambdaParse.t
          |> Result.bind ~f:eval
        in
        { input; result; selected }
      | Unselect -> { input; result; selected = None }
      | Select (start, finish) ->
        { input; result; selected = Some Range.{ start; finish } }
      | SwitchInputLang ->
        let input, result = match result with
          | Error _ -> "", result
          | Ok tm ->
            (* TODO: clean up / explain *)
            let result_str = Fmt.str "%a" lambda_pretty tm in
            Fmt.pr "result_str: %s\n" result_str;
            result_str, ParseUtil.parse_string LambdaParse.t result_str
        in
        (* TODO: update not with result but input *)
        { input; selected = None; result }
  ;;
end

module View = struct
  let mk_output' model_s =
    let range_s : SourceRanges.t signal = model_s
      |> S.map (fun Model.{ selected; _ } ->
          SourceRanges.of_opt_range ~buf:"input" selected)
    in
    let formatted_s = model_s
      |> S.map (fun Model.{ result; _ } ->
             let elt, formatter, _clear = RangeFormatter.mk
               ~selection_s:range_s
               ~set_selection:(fun _ -> () (* TODO *))
             in
             (match result with
             | Ok tm -> Fmt.pf formatter "%a" lambda_pretty tm
             | Error msg -> Fmt.pf formatter "%s" msg);
             Fmt.flush formatter ();
             elt)
    in
    mk_output formatted_s
  ;;

  let view model_s =
    let input, input_event =
      MultilineInput.mk (model_s |> S.map (fun model -> model.Model.input))
    in

    let elem, click_evt = demo_template
      (txt "input")
      input
      (txt "output")
      (mk_output' model_s)
    in

    let evts = E.select
      [ click_evt |> E.map (fun _click_evt -> Action.SwitchInputLang)
      ; input_event
        |> E.map (function
          | InputUpdate str -> Action.Evaluate str
          | InputSelect (start, finish) -> Select (start, finish)
          | InputUnselect -> Unselect
        )
      ]
    in

    evts, elem
  ;;
end

let stateless_view () =
  let initial_model : Model.t =
    let input = {|(\x -> \y -> x) z w|} in
    let result =
      let%bind parsed = ParseUtil.parse_string LambdaParse.t input in
      eval parsed
    in
    { input; result; selected = None }
  in

  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum (S.value model_s) do_action in
    model_s', (model_s', elem)
  in

  let model_s, elem = S.fix initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
