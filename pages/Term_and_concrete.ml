open Base
open Common
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Brr
open Note
open Prelude

module Model = struct
  type t =
    { input : string
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; input_selected : Opt_range.t
    ; output_selected : Opt_range.t
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
        | Error msg -> Fmt.string ppf msg
        | Ok tm -> Nominal.Term.pp ppf tm)
      result
      Opt_range.pp
      input_selected
      Opt_range.pp
      output_selected
  ;;

  let ( = ) m1 m2 =
    let result_eq = Result.equal Nominal.Term.( = ) String.( = ) in
    String.(m1.input = m2.input)
    && result_eq m1.result m2.result
    && Opt_range.(m1.input_selected = m2.input_selected)
    && Opt_range.(m1.output_selected = m2.output_selected)
  ;;

  let initial_model =
    let input = {|\f -> \g -> \x -> f (g x)|} in
    let result = Lvca_parsing.parse_string Lvca_languages.Lambda_calculus.Parse.t input in
    { input; result; input_lang = Lambda; input_selected = None; output_selected = None }
  ;;
end

module Controller = struct
  let update (action : Action.t) model =
    let open Model in
    let { result; input_lang; _ } = model in
    match action with
    | Evaluate str ->
      let result = Lvca_parsing.parse_string (parser_of input_lang) str in
      { model with result; input_selected = None; output_selected = None }
    | Input_select output_selected ->
      { model with output_selected; input_selected = None }
    | Output_select input_selected ->
      { model with input_selected; output_selected = None }
    | Switch_input_lang ->
      let input_lang, formatter =
        match input_lang with
        | Lambda -> Term, Nominal.Term.pp
        | Term -> Lambda, Lvca_languages.Lambda_calculus.pp
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
    let selection_s : Source_ranges.t signal =
      model_s
      |> S.map ~eq:Source_ranges.( = ) (fun Model.{ input_selected; _ } ->
             Source_ranges.of_opt_range ~buf:"input" input_selected)
    in
    let s =
      model_s
      |> S.map
           ~eq:(Tuple2.equal phys_equal html_eq)
           (fun Model.{ result; input_lang; _ } ->
             let Range_formatter.{ elem; formatter; selection_e = output_selection_e } =
               Range_formatter.mk ~selection_s ()
             in
             let () =
               match result, input_lang with
               | Ok tm, Lambda -> Nominal.Term.pp formatter tm
               | Ok tm, Term -> Lvca_languages.Lambda_calculus.pp formatter tm
               | Error msg, Lambda | Error msg, Term -> Fmt.string formatter msg
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
    |> S.map
         ~eq:(Tuple2.equal htmls_eq htmls_eq)
         (Tuple2.map ~f:(fun str -> [ El.txt' str ]))
  ;;

  let view model_s =
    let descriptions_s = make_descriptions model_s in
    let input_desc, output_desc = S.Pair.(fst descriptions_s, snd descriptions_s) in
    let input_desc_elem = mk_reactive El.span input_desc in
    let output_desc_elem = mk_reactive El.span output_desc in
    let input, input_event =
      Multiline_input.mk
        (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.input))
    in
    let output_selection_e, output_elem = mk_output' model_s in
    let elem, click_evt =
      demo_template input_desc_elem input output_desc_elem output_elem
    in
    let evts =
      E.select
        [ click_evt |> E.map (fun _click -> Action.Switch_input_lang)
        ; input_event
          |> E.map (function
                 | Evaluate_input str -> Action.Evaluate str
                 | Input_select rng -> Input_select (Some rng)
                 | Input_update _ | Input_unselect -> Input_select None)
        ; output_selection_e
          |> E.map (fun rng ->
                 match Map.find rng "input" with
                 | Some [ rng ] -> Action.Output_select (Some rng)
                 | _ -> Action.Output_select None)
        ]
    in
    evts, elem
  ;;
end

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
