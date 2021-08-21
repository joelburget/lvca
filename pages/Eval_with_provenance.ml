open Base
open Common
open Lvca_provenance
open Lvca_syntax
open Note
module Tuple2 = Lvca_util.Tuple2

let eval = Lvca_languages.Lambda_calculus.eval

module Model = struct
  module Internal =
  [%lvca.abstract_syntax_module
  {|
term : *
string : *
opt_range : *
result : * -> * -> *
lang : *

model := Model(string; result term string; lang; result term string; opt_range; opt_range)
    |}]

  type t =
    { input : string
    ; parsed_input : (term, string) Result.t
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; input_selected : Opt_range.t
    ; output_selected : Opt_range.t
    }

  let print { input; parsed_input; input_lang; result; input_selected; output_selected } =
    let pp_tm_result ppf tm_result =
      match tm_result with
      | Error msg -> Fmt.string ppf msg
      | Ok tm -> Nominal.Term.pp ppf tm
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
      Opt_range.pp
      input_selected
      Opt_range.pp
      output_selected
  ;;

  let ( = ) m1 m2 =
    let result_eq =
      Result.equal (Nominal.Term.equal ~info_eq:Opt_range.( = )) String.( = )
    in
    String.(m1.input = m2.input)
    && result_eq m1.parsed_input m2.parsed_input
    && result_eq m1.result m2.result
    && Opt_range.(m1.input_selected = m2.input_selected)
    && Opt_range.(m1.output_selected = m2.output_selected)
  ;;

  let initial_model =
    let input = {|(\x -> \y -> x) z w|} in
    let parsed_input =
      Lvca_parsing.parse_string Lvca_languages.Lambda_calculus.Parse.t input
    in
    let result = Result.bind parsed_input ~f:eval in
    { input
    ; parsed_input
    ; input_lang = Lambda
    ; result
    ; input_selected = None
    ; output_selected = None
    }
  ;;
end

module Controller = struct
  let update (action : Action.t) model =
    let open Model in
    let { input; parsed_input; input_lang; _ } = model in
    match action with
    | Evaluate str ->
      let parsed_input =
        Lvca_parsing.parse_string Lvca_languages.Lambda_calculus.Parse.t str
      in
      let result = Result.bind parsed_input ~f:eval in
      { model with parsed_input; result; input_selected = None; output_selected = None }
    | Input_select output_selected ->
      Brr.Console.log
        [ Jstr.v "output_selected"
        ; output_selected |> Fmt.to_to_string Opt_range.pp |> Jstr.v
        ];
      { model with output_selected; input_selected = None }
    | Output_select input_selected ->
      Brr.Console.log
        [ Jstr.v "input_selected"
        ; input_selected |> Fmt.to_to_string Opt_range.pp |> Jstr.v
        ];
      { model with input_selected; output_selected = None }
    | Switch_input_lang ->
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
  let cvt_loc = Source_ranges.of_opt_range ~buf:"input"

  let mk_output' model_s =
    let selection_s : Source_ranges.t signal =
      model_s
      |> S.map ~eq:Source_ranges.( = ) (fun Model.{ input_selected; _ } ->
             cvt_loc input_selected)
    in
    let s =
      model_s
      |> S.map ~eq:(Tuple2.equal phys_equal html_eq) (fun Model.{ result; _ } ->
             let Range_formatter.{ elem; formatter; selection_e = output_selection_e } =
               Range_formatter.mk ~selection_s ()
             in
             let () =
               match result with
               | Ok tm ->
                 Brr.Console.log
                   [ Jstr.v
                       ("tm loc: " ^ Fmt.to_to_string Opt_range.pp (Nominal.Term.info tm))
                   ];
                 let tm = Nominal.Term.map_info tm ~f:cvt_loc in
                 lambda_ranges_pretty formatter tm
               | Error msg -> Fmt.string formatter msg
             in
             Fmt.flush formatter ();
             output_selection_e, elem)
    in
    let output_selection_e = s |> S.Pair.fst ~eq:phys_equal |> E.swap in
    let formatted_s = s |> S.Pair.snd ~eq:html_eq in
    output_selection_e, mk_output formatted_s
  ;;

  let view model_s =
    let txt' = Brr.El.txt' in
    let input, input_event =
      Multiline_input.mk
        (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.input))
    in
    let output_selection_e, output_elem = mk_output' model_s in
    let elem, click_evt =
      demo_template (txt' "input") input (txt' "output") output_elem
    in
    let evts =
      E.select
        [ click_evt |> E.map (fun _click_evt -> Action.Switch_input_lang)
        ; input_event
          |> E.map (function
                 | Evaluate_input str -> Action.Evaluate str
                 | Input_select rng -> Input_select (Some rng)
                 | Input_unselect | Input_update _ -> Input_select None)
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
