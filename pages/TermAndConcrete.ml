open Base
open Common
open Lvca_syntax
open Brr
open Brr_note
open Note

module PrimitiveParse = Primitive.Parse (ParseUtil.NoComment)
module TermParse = Nominal.Parse (ParseUtil.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

type lang =
  | Lambda
  | Term

let parser_of = function Lambda -> LambdaParse.t | Term -> TermParse.t PrimitiveParse.t
let term_pretty = Nominal.pp_term_range Primitive.pp (* XXX why used twice? *)

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
  let update (action : Action.t) model_s signal_update =
    let open Model in
    let { input; result; input_lang; selected } = S.value model_s in
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
            result'_str, Ok tm
              (* ParseUtil.parse_string (parser_of input_lang') result'_str *)
        in
        { input = input'; input_lang = input_lang'; selected = None; result = result' }
    in
    signal_update new_model
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
             let elt, formatter, _clear = RangeFormatter.mk
               ~selection_s:range_s
               ~set_selection:(fun _ -> () (* TODO *))
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
    |> S.map (Lvca_util.Tuple2.map ~f:(fun str -> [El.txt (Jstr.v str)]))
  ;;

  let view model_s signal_update =
    let descriptions_s = make_descriptions model_s in
    let input_desc, output_desc = S.Pair.(fst descriptions_s, snd descriptions_s) in
    let input_desc_elem = El.span [] in
    let output_desc_elem = El.span [] in
    let () = Elr.def_children input_desc_elem input_desc in
    let () = Elr.def_children output_desc_elem output_desc in

    let handler _evt =
      Controller.update SwitchInputLang model_s signal_update;
      false
    in
    let input, input_event =
      MultilineInput.mk (model_s |> S.map (fun model -> model.Model.input))
    in
    let input_event : Action.t event =
      input_event
      |> E.map (fun evt ->
        match evt with
          | InputUpdate str -> Action.Evaluate str
          | InputSelect (start, finish) -> Select (start, finish)
          | InputUnselect -> Unselect)
    in
    let _sink : Logr.t option =
      E.log input_event (fun evt -> Controller.update evt model_s signal_update)
    in

    demo_template handler input_desc_elem input output_desc_elem (mk_output' model_s)
  ;;
end

let stateless_view () =
  let initial_model : Model.t =
    let input = {|\f -> \g -> \x -> f (g x)|} in
    let result = ParseUtil.parse_string LambdaParse.t input in
    { input; result; input_lang = Lambda; selected = None }
  in
  let model_s, signal_update = S.create initial_model in
  View.view model_s signal_update
;;
