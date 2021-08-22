open Base
open Brr
open Brr_note
open Note
open Prelude
open Lvca_provenance
open Lvca_syntax

module Model = struct
  module Internal =
  [%lvca.abstract_syntax_module
  {|
string : *  // module Lvca_syntax.Primitive.String

t :=
  | Selected_tag(string)
  | Selected_hash(string)
  | No_selection()
    |}]

  include Internal.T.Plain

  let initial_model = No_selection
end

module Action = struct
  type t =
    | Select_tag of string
    | Select_hash of string
end

module Controller = struct
  let update (action : Action.t) _model =
    match action with
    | Select_tag str -> Model.Selected_tag str
    | Select_hash str -> Selected_hash str
  ;;
end

module View = struct
  let div, h2, h3, li, span, txt', ul = El.(div, h2, h3, li, span, txt', ul)

  let convert_info commented =
    commented |> Commented.get_range |> Source_ranges.of_opt_range ~buf:"store"
  ;;

  let view model_s =
    let go mk_evt tbl =
      tbl
      |> Hashtbl.keys
      |> List.map ~f:(fun str ->
             let elem = li ~at:[ class' "cursor-pointer" ] [ txt' str ] in
             let click_event = Evr.on_el Ev.click (fun _evt -> mk_evt str) elem in
             click_event, elem)
      |> List.unzip
    in
    let tag_evts, tag_list =
      go (fun str -> Action.Select_tag str) Store.Tag_store.singleton
    in
    let hash_evts, content_list =
      go (fun str -> Action.Select_hash str) Store.Content_store.singleton
    in
    let rendered_term =
      model_s
      |> S.map
           (let go find kind str =
              match find str with
              | Some nom ->
                nom
                |> Nominal.Term.map_info ~f:convert_info
                |> Tree_view.view_tm ~source_column:false ~range_column:false
                |> fst
              | None -> txt' (Fmt.str "%s not found" kind)
            in
            function
            | Model.Selected_tag tag -> go Store.find "tag" tag
            | Selected_hash hash -> go Store.Content_store.find "hash" hash
            | No_selection -> txt' "no selection")
      |> mk_reactive' div
    in
    let evt : Action.t event = E.select (tag_evts @ hash_evts) in
    let elem =
      div
        [ h2 [ txt' "Store viewer" ]
        ; div ~at:[ class' "container" ] [ ul tag_list; ul content_list ]
        ; div ~at:[ class' "side" ] [ h3 [ txt' "Term" ]; rendered_term ]
        ]
    in
    evt, elem
  ;;
end

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
