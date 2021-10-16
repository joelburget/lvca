open Base
open Brr
open Brr_note
open Lvca_syntax
open Lvca_util
open Note

let buf = "input"

module Evaluation = struct
  module Lang =
  [%lvca.abstract_syntax_module
  {|
string : *
nominal : *

evaluation := Evaluation(string; nominal)
    |}
  , { string = "Primitive.String"; nominal = "Nominal.Term" }]

  include Lang.Evaluation

  (* TODO: generate this *)
  let equivalent ~info_eq (Evaluation (i1, s1, n1)) (Evaluation (i2, s2, n2)) =
    info_eq i1 i2
    && Primitive.String.equivalent ~info_eq s1 s2
    && Nominal.Term.equivalent ~info_eq n1 n2
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
end

module Model = struct
  type t =
    { evaluations : Evaluation.t list
    ; error_msg : string option
    }

  let initial_model = { evaluations = []; error_msg = None }

  let ( = ) x y =
    List.equal Evaluation.( = ) x.evaluations y.evaluations
    && Option.equal String.( = ) x.error_msg y.error_msg
  ;;
end

module Action = struct
  type t =
    | Evaluate of string
    | DeleteRow of int
end

module Controller = struct
  let update (action : Action.t) Model.{ evaluations; error_msg } =
    match action with
    | Evaluate str ->
      (match Common.parse_term str with
      | Ok tm ->
        let info = Provenance.of_here [%here] in
        Model.
          { evaluations = Evaluation (info, (info, str), tm) :: evaluations
          ; error_msg = None
          }
      | Error msg -> { evaluations; error_msg = Some msg })
    | DeleteRow i -> { evaluations = List.remove_nth evaluations i; error_msg }
  ;;
end

module View = struct
  open El
  open Prelude

  let row cells = El.tr ~at:[ class' "border-b" ] cells

  let view model_s =
    let input, input_event =
      Single_line_input.mk (S.const ~eq:String.( = ) "lam(x. x)")
    in
    let thead =
      row
        [ th ~at:(classes "w-1/2 text-left") [ txt' "input" ]
        ; th ~at:(classes "w-1/3 text-left") [ txt' "output" ]
        ; th ~at:(classes "w-1/6") []
        ]
    in
    let row' row_num input_str tm =
      let delete_button =
        button
          ~at:(classes "inline-block p-1 border-2 border-indigo-900 rounded")
          [ txt' "remove" ]
      in
      let evts =
        Evr.on_el Ev.click (fun _evt -> Action.DeleteRow row_num) delete_button
      in
      let tree_view, _tree_selection_e =
        Tree_view.view_tm ~source_column:false ~range_column:false tm
      in
      let elem =
        row
          [ td
              ~at:(classes "py-4 pr-1")
              [ pre ~at:(classes "whitespace-pre-wrap break-word") [ txt' input_str ] ]
          ; td [ tree_view ]
          ; td [ delete_button ]
          ]
      in
      elem, evts
    in
    let tbody, tbody_evts =
      let never_eq _ _ = false in
      let eq = Tuple2.equal Common.htmls_eq never_eq in
      let s =
        model_s
        |> S.map ~eq (fun model ->
               model.Model.evaluations
               |> List.mapi
                    ~f:(fun row_num (Evaluation.Evaluation (_, (_, input), parsed)) ->
                      row' row_num input parsed)
               |> List.unzip)
      in
      S.Pair.fst ~eq:Common.htmls_eq s, S.Pair.snd ~eq:never_eq s
    in
    let error_msg =
      model_s
      |> S.map ~eq:Common.htmls_eq (fun model ->
             match model.Model.error_msg with
             | None -> []
             | Some msg -> [ span [ txt' msg ] ])
      |> mk_reactive div
    in
    let elem =
      div
        [ div ~at:[ class' "my-2" ] [ input ]
        ; div ~at:(classes "error my-2") [ error_msg ]
        ; Components.table ~classes:[ "w-full"; "mb-6" ] thead tbody
        ]
    in
    let actions =
      E.select
        [ input_event
          |> E.filter_map (function
                 | Common.Evaluate_input str -> Some (Action.Evaluate str)
                 | _ -> None)
        ; tbody_evts
          |> S.map ~eq:phys_equal E.select (* Select one event from a list of events *)
          |> E.swap
          (* Extract current signal's event *)
        ]
    in
    actions, elem
  ;;
end

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
