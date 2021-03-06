open Base
open Common
open Lvca_syntax
open Note
open Prelude
module Tuple2 = Lvca_util.Tuple2
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

let eval = Lvca_languages.LambdaCalculus.eval

module Model = struct
  type t =
    { input : string
    ; parsed_input : (term, string) Result.t
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; input_selected : OptRange.t
    ; output_selected : OptRange.t
    }

  let print { input; parsed_input; input_lang; result; input_selected; output_selected } =
    let pp_tm_result ppf tm_result =
      match tm_result with
      | Error msg -> Fmt.pf ppf "%s" msg
      | Ok tm -> Nominal.pp_term Primitive.pp ppf tm
    in
    let input_lang_str = match input_lang with Lambda -> "Lambda" | Term -> "Term" in
    Fmt.pr
      "{ input = %s; parsed_input = %a; input_lang = %s; result = %a; input_selected = \
       %a; output_selected = %a }\n"
      input
      pp_tm_result
      parsed_input
      input_lang_str
      pp_tm_result
      result
      OptRange.pp
      input_selected
      OptRange.pp
      output_selected
  ;;

  let ( = ) m1 m2 =
    let result_eq =
      Result.equal (Nominal.equal OptRange.( = ) Primitive.( = )) String.( = )
    in
    String.(m1.input = m2.input)
    && result_eq m1.parsed_input m2.parsed_input
    && result_eq m1.result m2.result
    && OptRange.(m1.input_selected = m2.input_selected)
    && OptRange.(m1.output_selected = m2.output_selected)
  ;;
end

module Controller = struct
  let update (action : Action.t) model =
    let open Model in
    let { input; parsed_input; input_lang; _ } = model in
    match action with
    | Evaluate str ->
      let parsed_input = ParseUtil.parse_string LambdaParse.t str in
      let result = Result.bind parsed_input ~f:eval in
      { model with parsed_input; result; input_selected = None; output_selected = None }
    | InputSelect output_selected ->
      Brr.Console.log
        [ Jstr.v "output_selected"
        ; output_selected |> Fmt.to_to_string OptRange.pp |> Jstr.v
        ];
      { model with output_selected; input_selected = None }
    | OutputSelect input_selected ->
      Brr.Console.log
        [ Jstr.v "input_selected"
        ; input_selected |> Fmt.to_to_string OptRange.pp |> Jstr.v
        ];
      { model with input_selected; output_selected = None }
    | SwitchInputLang ->
      let input_lang, formatter =
        match input_lang with
        | Lambda -> Term, term_pretty
        | Term -> Lambda, lambda_pretty
      in
      let input =
        match parsed_input with
        | Error _ -> input
        | Ok tm -> Fmt.to_to_string formatter tm
      in
      { model with input; input_lang; input_selected = None; output_selected = None }
  ;;
end

module View = struct
  let cvt_loc = SourceRanges.of_opt_range ~buf:"input"

  let mk_output' model_s =
    let selection_s : SourceRanges.t signal =
      model_s
      |> S.map ~eq:SourceRanges.( = ) (fun Model.{ input_selected; _ } ->
             cvt_loc input_selected)
    in
    let s =
      model_s
      |> S.map ~eq:(Tuple2.equal phys_equal html_eq) (fun Model.{ result; _ } ->
             let RangeFormatter.{ elem; formatter; selection_e = output_selection_e } =
               RangeFormatter.mk ~selection_s ()
             in
             let () =
               match result with
               | Ok tm ->
                 Brr.Console.log
                   [ Jstr.v ("tm loc: " ^ Fmt.to_to_string OptRange.pp (Nominal.info tm))
                   ];
                 let tm = Nominal.map_info tm ~f:cvt_loc in
                 Fmt.pf formatter "%a" lambda_ranges_pretty tm
               | Error msg -> Fmt.pf formatter "%s" msg
             in
             Fmt.flush formatter ();
             output_selection_e, elem)
    in
    let output_selection_e = s |> S.Pair.fst ~eq:phys_equal |> E.swap in
    let formatted_s = s |> S.Pair.snd ~eq:html_eq in
    output_selection_e, mk_output formatted_s
  ;;

  let view model_s =
    let input, input_event =
      MultilineInput.mk
        (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.input))
    in
    let output_selection_e, output_elem = mk_output' model_s in
    let elem, click_evt = demo_template (txt "input") input (txt "output") output_elem in
    let evts =
      E.select
        [ click_evt |> E.map (fun _click_evt -> Action.SwitchInputLang)
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
    let input = {|(\x -> \y -> x) z w|} in
    let parsed_input = ParseUtil.parse_string LambdaParse.t input in
    let result = Result.bind parsed_input ~f:eval in
    { input
    ; parsed_input
    ; input_lang = Lambda
    ; result
    ; input_selected = None
    ; output_selected = None
    }
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
