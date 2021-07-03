open Base
open Brr
open Lvca_provenance
open Lvca_syntax
open Note
module Pfpl_pcf = Lvca_languages.Pfpl_pcf
module Provenance = Pfpl_pcf.Provenance
open Pfpl_pcf.Lang
open Result.Let_syntax
open Prelude

let ( >> ) = Lvca_util.( >> )

type term = Opt_range.t Provenance.t Exp.t

let buf = "input"

let parse str =
  let%map parsed = Lvca_parsing.(parse_string (whitespace *> Exp.parse)) str in
  Exp.map_info ~f:(fun info -> Provenance.Root info) parsed
;;

let parsed_to_result = function
  | Ok tm ->
    let steps, result = Pfpl_pcf.eval tm in
    steps, Result.map_error result ~f:(fun (msg, tm) -> msg, Some tm)
  | Error msg -> [], Error (msg, None)
;;

module Model = struct
  type t =
    { input : string
    ; intermediate_results : term list
    ; result : (term, string * term option) Result.t
    ; ranges : Ranges.t
    }

  let initial_model =
    let input = "Succ(Ifz(Zero(); x. x; Zero()))" in
    let intermediate_results, result = input |> parse |> parsed_to_result in
    { input; intermediate_results; result; ranges = [] }
  ;;

  let pp ppf { input; intermediate_results; result; ranges } =
    let pp_tm_result ppf tm_result =
      match tm_result with
      | Error (msg, Some tm) -> Fmt.(pair string Exp.pp) ppf (msg, tm)
      | Error (msg, None) -> Fmt.string ppf msg
      | Ok tm -> Exp.pp ppf tm
    in
    Fmt.pf
      ppf
      "{ input = %s; intermediate_results = %a; result = %a; ranges = %a }"
      input
      Fmt.(list Exp.pp)
      intermediate_results
      pp_tm_result
      result
      Ranges.pp
      ranges
  ;;

  let ( = ) m1 m2 =
    let exp_eq = Exp.equal ~info_eq:(Provenance.equal ~info_eq:Opt_range.( = )) in
    let result_eq =
      Result.equal exp_eq (Lvca_util.Tuple2.equal String.( = ) (Option.equal exp_eq))
    in
    String.(m1.input = m2.input)
    && result_eq m1.result m2.result
    && Ranges.( = ) m1.ranges m2.ranges
  ;;
end

module Action = struct
  type t =
    | Evaluate of string
    | SetInputHighlights of Ranges.t
end

module Controller = struct
  let update (action : Action.t) model =
    match action with
    | Evaluate input ->
      let intermediate_results, result = input |> parse |> parsed_to_result in
      Model.{ input; intermediate_results; result; ranges = [] }
    | SetInputHighlights ranges -> { model with ranges }
  ;;
end

let mk_tree_view tm =
  let nom_tm =
    tm
    |> Exp.to_nominal
    |> Nominal.Term.map_info
         ~f:(Provenance.get_root_info >> Source_ranges.of_opt_range ~buf)
  in
  let tree_view, tree_selection_e =
    Tree_view.view_tm ~source_column:false ~range_column:false nom_tm
  in
  let set_highlight_e =
    tree_selection_e
    |> E.filter_map (fun source_ranges ->
           Map.find source_ranges buf
           |> Option.map ~f:(fun ranges -> Action.SetInputHighlights ranges))
  in
  set_highlight_e, tree_view
;;

module View = struct
  let view model_s =
    let div = El.div in
    let input_s = S.map ~eq:String.( = ) (fun Model.{ input; _ } -> input) model_s in
    let highlights_s =
      S.map ~eq:Ranges.( = ) (fun Model.{ ranges; _ } -> ranges) model_s
    in
    let input_elem, input_evt = Single_line_input.mk input_s ~highlights_s in
    let enter_input_e =
      input_evt
      |> E.filter_map (function
             | Common.EvaluateInput str -> Some (Action.Evaluate str)
             | InputUpdate _ -> Some (SetInputHighlights [])
             | _ -> None)
    in
    let set_highlight_e1, intermediate_tree_views =
      let s =
        model_s
        |> S.map ~eq:phys_equal (fun Model.{ intermediate_results; _ } ->
               intermediate_results)
        |> S.map
             ~eq:(Lvca_util.Tuple2.equal phys_equal phys_equal)
             (fun intermediate_results ->
               let events, elems =
                 intermediate_results |> List.map ~f:mk_tree_view |> List.unzip
               in
               E.select events, elems)
      in
      S.Pair.fst ~eq:phys_equal s, S.Pair.snd ~eq:phys_equal s
    in
    let set_highlight_e2, output_tree_view =
      let s =
        model_s
        |> S.map ~eq:phys_equal (fun Model.{ result; _ } -> result)
        |> S.map ~eq:(Lvca_util.Tuple2.equal phys_equal phys_equal) (fun result ->
               match result with
               | Error (msg, _tm_opt) -> E.never, div [ txt msg ]
               | Ok tm -> mk_tree_view tm)
      in
      S.Pair.fst ~eq:phys_equal s, S.Pair.snd ~eq:phys_equal s
    in
    let actions =
      E.select [ E.swap set_highlight_e1; E.swap set_highlight_e2; enter_input_e ]
    in
    let elem =
      div
        [ input_elem
        ; mk_reactive div intermediate_tree_views
        ; mk_reactive' div output_tree_view
        ]
    in
    actions, elem
  ;;
end

let stateless_view () =
  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum ~eq:Model.( = ) (S.value model_s) do_action in
    model_s', (model_s', elem)
  in
  let model_s, elem = S.fix ~eq:Model.( = ) Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
