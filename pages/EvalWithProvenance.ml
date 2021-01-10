open Base
open Common
open Lvca_syntax
open Note
open Prelude

module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

let eval = Lvca_languages.LambdaCalculus.eval

module Model = struct
  type t =
    { input : string
    ; parsed_input : (term, string) Result.t
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; selected : OptRange.t
    }

  let print { input; parsed_input; input_lang; result; selected } =
    let pp_tm_result ppf tm_result = match tm_result with
      | Error msg -> Fmt.pf ppf "%s" msg
      | Ok tm -> Nominal.pp_term Primitive.pp ppf tm
    in
    let input_lang_str = match input_lang with Lambda -> "Lambda" | Term -> "Term" in
    Fmt.pr
      "{ input = %s; parsed_input = %a; input_lang = %s; result = %a; selected = %a }\n"
      input
      pp_tm_result parsed_input
      input_lang_str
      pp_tm_result result
      OptRange.pp
      selected
  ;;

  let (=) m1 m2 =
    let result_eq = Result.equal (Nominal.equal OptRange.(=) Primitive.(=)) String.(=) in
    String.(m1.input = m2.input) &&
    result_eq m1.parsed_input m2.parsed_input &&
    result_eq m1.result m2.result &&
    OptRange.(m1.selected = m2.selected)
end

module Controller = struct
  let update (action : Action.t) model =
    let open Model in
    let { input; parsed_input; input_lang; _ } = model in
    match action with
      | Evaluate str ->
        let parsed_input = ParseUtil.parse_string LambdaParse.t str in
        let result = Result.bind parsed_input ~f:eval in
        { model with parsed_input; result; selected = None }
      | Unselect -> { model with selected = None }
      | Select (start, finish) ->
        { model with selected = Some Range.{ start; finish } }
      | SwitchInputLang ->
        let input_lang, formatter = match input_lang with
          | Lambda -> Term, term_pretty
          | Term -> Lambda, lambda_pretty
        in
        let input = match parsed_input with
          | Error _ -> input
          | Ok tm -> Fmt.str "%a" formatter tm
        in
        { model with input; input_lang; selected = None }
  ;;
end

module View = struct
  let mk_output' model_s =
    let range_s : SourceRanges.t signal = model_s
      |> S.map ~eq:SourceRanges.(=) (fun Model.{ selected; _ } ->
          SourceRanges.of_opt_range ~buf:"input" selected)
    in
    let formatted_s = model_s
      |> S.map ~eq:html_eq (fun Model.{ result; _ } ->
         Brr.Console.log [Jstr.v "here"];
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
      MultilineInput.mk (model_s |> S.map ~eq:String.(=) (fun model -> model.Model.input))
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
    let parsed_input = ParseUtil.parse_string LambdaParse.t input in
    let result = Result.bind parsed_input ~f:eval in
    { input; parsed_input; input_lang = Lambda; result; selected = None }
  in

  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum ~eq:Model.(=) (S.value model_s) do_action in
    model_s', (model_s', elem)
  in

  let model_s, elem = S.fix ~eq:Model.(=) initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
