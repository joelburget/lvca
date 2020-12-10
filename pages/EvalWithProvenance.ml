open Base
open Lvca_syntax
open Result.Let_syntax
open Common

module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

let eval = Lvca_languages.LambdaCalculus.eval

module Model = struct
  type t =
    { input : string
    ; result : (term, string) Result.t
    ; selected : OptRange.t
    }

  (* TODO: evaluate ppx_deriving *)

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
  let update (action : Action.t) model_s signal_update =
    let open Model in
    let { input; result; selected } = React.S.value model_s in
    let new_model =
      match action with
      | Evaluate str ->
        let result =
          let%bind parsed = ParseUtil.parse_string LambdaParse.t str in
          eval parsed
        in
        { input; result; selected }
      | Unselect -> { input; result; selected = None }
      | Select (start, finish) ->
        { input; result; selected = Some Range.{ start; finish } }
      | SwitchInputLang ->
        let input', result' =
          match result with
          | Error _ -> "", result
          | Ok tm ->
            (* TODO: clean up / explain *)
            let result'_str = Fmt.str "%a" lambda_pretty tm in
            Fmt.pr "result'_str: %s\n" result'_str;
            result'_str, ParseUtil.parse_string LambdaParse.t result'_str
        in
        (* TODO: update not with result but input *)
        { input = input'; selected = None; result = result' }
    in
    signal_update new_model
  ;;
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let mk_output' model_s =
    let range_s : SourceRanges.t React.signal = model_s
      |> React.S.map (fun Model.{ selected; _ } ->
          SourceRanges.of_opt_range ~buf:"input" selected)
    in
    let formatted_s =
      model_s
      |> React.S.map (fun Model.{ result; _ } ->
             let elt, formatter = RangeFormatter.mk
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

  let view model_s signal_update =
    let handler _evt =
      Controller.update SwitchInputLang model_s signal_update;
      false
    in
    let input, input_event =
      MultilineInput.mk (model_s |> React.S.map (fun model -> model.Model.input))
    in
    let (_ : unit React.event) = input_event
      |> React.E.map (fun evt ->
             let evt' =
               match evt with
               | InputUpdate str -> Action.Evaluate str
               | InputSelect (start, finish) -> Select (start, finish)
               | InputUnselect -> Unselect
             in
             Controller.update evt' model_s signal_update)
    in
    demo_template
      handler
      (Html.txt "input")
      input
      (Html.txt "output")
      (mk_output' model_s)
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
  let model_s, signal_update = React.S.create initial_model in
  View.view model_s signal_update
;;
