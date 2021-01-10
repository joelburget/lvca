open Base
open Common
open Lvca_syntax
open Brr
open Note
open Prelude

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

module Controller = struct
  let update (action : Action.t) Model.{ input; result; input_lang; selected } =
    let open Model in
    match action with
    | Evaluate str ->
      let result = ParseUtil.parse_string (parser_of input_lang) str in
      { input; input_lang; result; selected }
    | Unselect -> { input; result; input_lang; selected = None }
    | Select (start, finish) ->
      { input; result; input_lang; selected = Some Range.{ start; finish } }
    | SwitchInputLang ->
      let input_lang, formatter = match input_lang with
        | Lambda -> Term, term_pretty
        | Term -> Lambda, lambda_pretty
      in
      let input, result = match result with
        | Error _ -> "", result
        | Ok tm ->
          let result_str = Fmt.str "%a" formatter tm in
          result_str, Ok tm
      in
      { input; input_lang = input_lang; selected = None; result }
  ;;
end

module View = struct
  let mk_output' model_s =
    let range_s : SourceRanges.t signal = model_s
      |> S.map ~eq:SourceRanges.(=) (fun Model.{ selected; _ } ->
          SourceRanges.of_opt_range ~buf:"input" selected)
    in
    let formatted_s = model_s
      |> S.map (fun Model.{ result; input_lang; _ } ->
             let elt, formatter, _selection_e, _clear = RangeFormatter.mk
               ~selection_s:range_s
             in
             (match result, input_lang with
             | Ok tm, Lambda -> Fmt.pf formatter "%a" term_pretty tm
             | Ok tm, Term -> Fmt.pf formatter "%a" lambda_pretty tm
             | Error msg, Lambda | Error msg, Term -> Fmt.pf formatter "%s" msg);
             Fmt.flush formatter ();
             elt)
    in
    mk_output formatted_s
  ;;

  let make_descriptions model_s =
    model_s
    |> S.map (fun Model.{ input_lang; _ } ->
           match input_lang with
           | Lambda -> "input (concrete)", "output (abstract)"
           | Term -> "input (abstract)", "output (concrete)")
    |> S.map (Lvca_util.Tuple2.map ~f:(fun str -> [txt str]))
  ;;

  let view model_s =
    let descriptions_s = make_descriptions model_s in
    let input_desc, output_desc = S.Pair.(fst descriptions_s, snd descriptions_s) in
    let input_desc_elem = mk_reactive El.span input_desc in
    let output_desc_elem = mk_reactive El.span output_desc in

    let input, input_event =
      MultilineInput.mk (model_s |> S.map (fun model -> model.Model.input))
    in

    let elem, click_evt =
      demo_template input_desc_elem input output_desc_elem (mk_output' model_s)
    in

    let evts = E.select
      [ click_evt |> E.map (fun _click -> Action.SwitchInputLang)
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
    let input = {|\f -> \g -> \x -> f (g x)|} in
    let result = ParseUtil.parse_string LambdaParse.t input in
    { input; result; input_lang = Lambda; selected = None }
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
