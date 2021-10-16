open Base
open Brr
open Lvca_provenance
open Note
open Lvca_languages.Pfpl_pcf
open Prelude

let ( >> ) = Lvca_util.( >> )

type term = Exp.t

let buf = "input"
let parse str = Lvca_parsing.(parse_string (whitespace *> Exp.parse)) str

let parsed_to_result = function
  | Ok tm ->
    let steps, result = eval tm in
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
    let result_eq =
      Result.equal
        Exp.( = )
        (Lvca_util.Tuple2.equal String.( = ) (Option.equal Exp.( = )))
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
  let nom_tm = Exp.to_nominal tm in
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
    let div, txt' = El.(div, txt') in
    let input_s = S.map ~eq:String.( = ) (fun Model.{ input; _ } -> input) model_s in
    let highlights_s =
      S.map ~eq:Ranges.( = ) (fun Model.{ ranges; _ } -> ranges) model_s
    in
    let input_elem, input_evt = Single_line_input.mk input_s ~highlights_s in
    let enter_input_e =
      input_evt
      |> E.filter_map (function
             | Common.Evaluate_input str -> Some (Action.Evaluate str)
             | Input_update _ -> Some (SetInputHighlights [])
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
               | Error (msg, _tm_opt) -> E.never, div [ txt' msg ]
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

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
