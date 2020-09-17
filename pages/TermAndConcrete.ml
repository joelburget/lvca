open Base
open Lvca_syntax
open Common

module Model = struct
  type t =
    { input : string
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; selected : OptRange.t
    }

  let print { input; input_lang; result; selected } =
    let input_lang_str = match input_lang with Lambda -> "Lambda" | Term -> "Term" in
    Fmt.pr
      "{ input = %s; input_lang = %s; result = %a; selected = %a }\n"
      input
      input_lang_str
      (fun ppf tm_result ->
        match tm_result with
        | Error msg -> Fmt.pf ppf "%s" msg
        | Ok tm -> Nominal.pp_term Primitive.pp ppf tm)
      result
      OptRange.pp
      selected
  ;;
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit

module Controller = struct
  let update (action : Action.t) model_s signal_update =
    let open Model in
    let { input; result; input_lang; selected } = React.S.value model_s in
    let new_model =
      match action with
      | Evaluate str ->
        let result = ParseUtil.parse_string (parser_of input_lang) str in
        { input; input_lang; result; selected }
      | Unselect -> { input; result; input_lang; selected = None }
      | Select (start, finish) ->
        { input; result; input_lang; selected = Some Range.{ start; finish } }
      | SwitchInputLang ->
        let input_lang', formatter =
          match input_lang with
          | Lambda -> Term, term_pretty
          | Term -> Lambda, lambda_pretty
        in
        let input', result' =
          match result with
          | Error _ -> "", result
          | Ok tm ->
            (* TODO: clean up / explain *)
            let result'_str = Fmt.str "%a" formatter tm in
            Fmt.pr "result'_str: %s\n" result'_str;
            result'_str, ParseUtil.parse_string (parser_of input_lang') result'_str
        in
        { input = input'; input_lang = input_lang'; selected = None; result = result' }
    in
    signal_update new_model
  ;;
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let mk_output' model_s =
    let range_s : OptRange.t React.signal =
      model_s |> React.S.map (fun Model.{ selected; _ } -> selected)
    in
    let formatted_s = model_s
      |> React.S.map (fun Model.{ result; input_lang; _ } ->
             let elt, formatter = RangeFormatter.mk range_s in
             (match result, input_lang with
             | Ok tm, Lambda -> Fmt.pf formatter "%a" term_pretty tm
             | Ok tm, Term -> Fmt.pf formatter "%a" lambda_pretty tm
             | Error msg, Lambda | Error msg, Term -> Fmt.pf formatter "%s" msg);
             Fmt.flush formatter ();
             elt)
    in
    mk_output formatted_s

  let make_descriptions model_s =
    model_s
    |> React.S.map (fun Model.{ input_lang; _ } ->
           match input_lang with
           | Lambda -> "input (concrete)", "output (abstract)"
           | Term -> "input (abstract)", "output (concrete)")
  ;;

  let view model_s signal_update =
    let descriptions_s = make_descriptions model_s in
    let input_desc, output_desc = React.S.Pair.(fst descriptions_s, snd descriptions_s) in
    let handler _evt =
      Controller.update SwitchInputLang model_s signal_update;
      false
    in

    let input, input_event = Common.mk_input
      (model_s |> React.S.map (fun model -> model.Model.input))
    in

    let _ : unit React.event = input_event |> React.E.map (fun evt ->
      let evt' = match evt with
      | InputUpdate str -> Action.Evaluate str
      | InputSelect (start, finish) -> Select (start, finish)
      | InputUnselect -> Unselect
      in
      Controller.update evt' model_s signal_update
    )
    in

    demo_template handler
      (R.Html.txt input_desc) input
      (R.Html.txt output_desc) (mk_output' model_s)
  ;;
end

let stateless_view =
  let initial_model : Model.t =
    let input = {|\f -> \g -> \x -> f (g x)|} in
    let result = ParseUtil.parse_string LambdaParse.t input in
    { input; result; input_lang = Lambda; selected = None }
  in
  let model_s, signal_update = React.S.create initial_model in
  View.view model_s signal_update
;;

(* let insert_demo elem = let model_s, signal_update = React.S.create Model.initial_model
   in Dom.appendChild elem (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view model_s
   signal_update)); Lwt.return ()

   let (_ : unit) = Js.export "TermAndConcrete" (object%js method run = insert_demo end) *)
