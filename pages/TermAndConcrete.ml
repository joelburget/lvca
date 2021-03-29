open Base
open Common
open Lvca_syntax
open Brr
open Note
open Prelude
module Tuple2 = Lvca_util.Tuple2

module Model = struct
  type t =
    { input : string
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; input_selected : OptRange.t
    ; output_selected : OptRange.t
    }

  let print { input; input_lang; result; input_selected; output_selected } =
    let input_lang_str = match input_lang with Lambda -> "Lambda" | Term -> "Term" in
    Fmt.pr
      "{ input = %s; input_lang = %s; result = %a; input_selected = %a; output_selected \
       = %a }\n"
      input
      input_lang_str
      (fun ppf tm_result ->
        match tm_result with
        | Error msg -> Fmt.pf ppf "%s" msg
        | Ok tm -> Nominal.Term.pp Primitive.pp ppf tm)
      result
      OptRange.pp
      input_selected
      OptRange.pp
      output_selected
  ;;

  let ( = ) m1 m2 =
    let result_eq =
      Result.equal (Nominal.Term.equal OptRange.( = ) Primitive.( = )) String.( = )
    in
    String.(m1.input = m2.input)
    && result_eq m1.result m2.result
    && OptRange.(m1.input_selected = m2.input_selected)
    && OptRange.(m1.output_selected = m2.output_selected)
  ;;
end

module Controller = struct
  let update (action : Action.t) model =
    let open Model in
    let { result; input_lang; _ } = model in
    match action with
    | Evaluate str ->
      let result = ParseUtil.parse_string (parser_of input_lang) str in
      { model with result; input_selected = None; output_selected = None }
    | InputSelect output_selected -> { model with output_selected; input_selected = None }
    | OutputSelect input_selected -> { model with input_selected; output_selected = None }
    | SwitchInputLang ->
      let input_lang, formatter =
        match input_lang with
        | Lambda -> Term, term_pretty
        | Term -> Lambda, lambda_pretty
      in
      let input, result =
        match result with
        | Error _ -> "", result
        | Ok tm -> Fmt.to_to_string formatter tm, Ok tm
      in
      { input; input_lang; input_selected = None; output_selected = None; result }
  ;;
end

module View = struct
  let mk_output' model_s =
    let selection_s : SourceRanges.t signal =
      model_s
      |> S.map ~eq:SourceRanges.( = ) (fun Model.{ input_selected; _ } ->
             SourceRanges.of_opt_range ~buf:"input" input_selected)
    in
    let s =
      model_s
      |> S.map
           ~eq:(Tuple2.equal phys_equal html_eq)
           (fun Model.{ result; input_lang; _ } ->
             let RangeFormatter.{ elem; formatter; selection_e = output_selection_e } =
               RangeFormatter.mk ~selection_s ()
             in
             let () =
               match result, input_lang with
               | Ok tm, Lambda -> Fmt.pf formatter "%a" term_pretty tm
               | Ok tm, Term -> Fmt.pf formatter "%a" lambda_pretty tm
               | Error msg, Lambda | Error msg, Term -> Fmt.pf formatter "%s" msg
             in
             Fmt.flush formatter ();
             output_selection_e, elem)
    in
    let output_selection_e = s |> S.Pair.fst ~eq:phys_equal |> E.swap in
    let formatted_s = s |> S.Pair.snd ~eq:html_eq in
    output_selection_e, mk_output formatted_s
  ;;

  let make_descriptions model_s =
    model_s
    |> S.map ~eq:(Tuple2.equal String.( = ) String.( = )) (fun Model.{ input_lang; _ } ->
           match input_lang with
           | Lambda -> "input (concrete)", "output (abstract)"
           | Term -> "input (abstract)", "output (concrete)")
    |> S.map ~eq:(Tuple2.equal htmls_eq htmls_eq) (Tuple2.map ~f:(fun str -> [ txt str ]))
  ;;

  let view model_s =
    let descriptions_s = make_descriptions model_s in
    let input_desc, output_desc = S.Pair.(fst descriptions_s, snd descriptions_s) in
    let input_desc_elem = mk_reactive El.span input_desc in
    let output_desc_elem = mk_reactive El.span output_desc in
    let input, input_event =
      MultilineInput.mk
        (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.input))
    in
    let output_selection_e, output_elem = mk_output' model_s in
    let elem, click_evt =
      demo_template input_desc_elem input output_desc_elem output_elem
    in
    let evts =
      E.select
        [ click_evt |> E.map (fun _click -> Action.SwitchInputLang)
        ; input_event
          |> E.map (function
                 | InputUpdate str -> Action.Evaluate str
                 | InputSelect rng -> InputSelect (Some rng)
                 | InputUnselect -> InputSelect None)
        ; output_selection_e
          |> E.map (fun rng ->
                 match Map.find rng "input" with
                 | Some [ rng ] -> Action.OutputSelect (Some rng)
                 | _ -> Action.OutputSelect None)
        ]
    in
    evts, elem
  ;;
end

let stateless_view () =
  let initial_model : Model.t =
    let input = {|\f -> \g -> \x -> f (g x)|} in
    let result = ParseUtil.parse_string LambdaParse.t input in
    { input; result; input_lang = Lambda; input_selected = None; output_selected = None }
  in
  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum ~eq:Model.( = ) (S.value model_s) do_action in
    model_s', (model_s', elem)
  in
  let model_s, elem = S.fix ~eq:Model.( = ) initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
