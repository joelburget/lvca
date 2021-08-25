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
  | Selected_filter(string)
  | No_selection()
    |}]

  include Internal.T.Plain

  let initial_model = No_selection
end

module Action = struct
  type t =
    | Select_tag of string
    | Select_hash of string
    | Select_filter of string
end

module Controller = struct
  let update (action : Action.t) _model =
    match action with
    | Select_tag str -> Model.Selected_tag str
    | Select_hash str -> Selected_hash str
    | Select_filter str -> Selected_filter str
  ;;
end

module View = struct
  let div, h2, h3, li, span, txt', ul = El.(div, h2, h3, li, span, txt', ul)
  let buf = "store"

  let convert_info commented =
    commented |> Commented.get_range |> Source_ranges.of_opt_range ~buf
  ;;

  let view_tm ?highlighted_ranges tm =
    tm
    |> Nominal.Term.map_info ~f:convert_info
    |> Tree_view.view_tm ?highlighted_ranges ~source_column:false ~range_column:false
    |> fst
  ;;

  let adapt_ranges matches =
    let ranges =
      matches
      |> List.filter_map ~f:(fun Commented.{ range; _ } -> range)
      |> Ranges.of_list
    in
    Lvca_util.String.Map.singleton buf ranges
  ;;

  let find_matches filter_str =
    match
      Lvca_parsing.(
        parse_string
          (whitespace *> Binding_aware_pattern.parse ~comment:no_comment)
          filter_str)
    with
    | Error msg ->
      Fmt.pr "failed to parse binding-aware-pattern: %s\n" msg;
      []
    | Ok pat ->
      Store.Content_store.singleton
      |> Hashtbl.data
      |> List.filter_map ~f:(fun tm ->
             let matches = Binding_aware_pattern.match_all pat tm in
             match matches with [] -> None | _ -> Some (tm, matches))
      |> List.map ~f:(fun (tm, matches) ->
             view_tm ~highlighted_ranges:(adapt_ranges matches) tm)
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
    let input, input_event =
      Single_line_input.mk ~autofocus:true (S.const ~eq:String.( = ) "")
    in
    let input_event =
      input_event
      |> E.filter_map (function
             | Common.Evaluate_input str -> Some (Action.Select_filter str)
             | _ -> None)
    in
    let rendered_terms =
      model_s
      |> S.map
           (let go find kind str =
              let it =
                match find str with
                | Some nom -> view_tm nom
                | None -> txt' (Fmt.str "%s not found" kind)
              in
              [ it ]
            in
            function
            | Model.Selected_tag tag -> go Store.find "tag" tag
            | Selected_hash hash -> go Store.Content_store.find "hash" hash
            | Selected_filter str ->
              let matches = find_matches str in
              txt' (Fmt.str "filtered results: (%d)" (List.length matches)) :: matches
            | No_selection -> [ txt' "no selection" ])
      |> mk_reactive div
    in
    let evt : Action.t event = E.select ((input_event :: tag_evts) @ hash_evts) in
    let elem =
      div
        [ h2 [ txt' "Store viewer" ]
        ; div ~at:[ class' "container" ] [ ul tag_list; ul content_list; input ]
        ; div ~at:[ class' "side" ] [ h3 [ txt' "Term" ]; rendered_terms ]
        ]
    in
    evt, elem
  ;;
end

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
