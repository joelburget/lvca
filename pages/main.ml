open Base
open Js_of_ocaml
open ReactiveData

module Model = struct
  type page =
    | TermAndConcretePage
    | EvalWithProvenancePage

  type t = { page : page }

  let initial_model = { page = TermAndConcretePage }
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit

module Action = struct
  type t = ChangePage of Model.page
end

module Controller = struct
  let update (action : Action.t) _model_s signal_update =
    let (ChangePage page) = action in
    signal_update Model.{ page }
  ;;
end

module View = struct
  open Model
  open Js_of_ocaml_tyxml.Tyxml_js

  let page_description = function
    | TermAndConcretePage -> "01: term and concrete"
    | EvalWithProvenancePage -> "0x: evaluation with provenance"
  ;;

  let stateless_view = function
    | TermAndConcretePage -> TermAndConcrete.stateless_view
    | EvalWithProvenancePage -> EvalWithProvenance.stateless_view
  ;;

  let all_pages = [ TermAndConcretePage; EvalWithProvenancePage ]
  let wrapper_div = Html5.div []
  let wrapper_dom = To_dom.of_div wrapper_div

  let handler signal_update evt =
    let elem = evt##.target |> Js.Opt.to_option |> Option.value_exn in
    let select_elem =
      elem |> Dom_html.CoerceTo.select |> Js.Opt.to_option |> Option.value_exn
    in
    let i = select_elem##.value |> Js.to_string |> Int.of_string in
    Caml.Printf.printf "%i\n" i;
    let page =
      match List.nth all_pages i with None -> failwith "TODO: error" | Some page -> page
    in
    signal_update Model.{ page };
    false
  ;;

  let view model_s signal_update =
    Html5.(
      div
        [ h2 [ txt "LVCA demos" ]
        ; div
            [ select
                ~a:[ a_onchange (handler signal_update) ]
                (all_pages
                |> List.mapi ~f:(fun i page ->
                       option
                         ~a:[ a_value (Int.to_string i) ]
                         (page |> page_description |> txt)))
            ; R.Html5.div
                (model_s
                |> React.S.map (fun { page } -> stateless_view page)
                |> RList.singleton_s)
            ]
        ])
  ;;
end

let insert_demo elem =
  let model_s, signal_update = React.S.create Model.initial_model in
  Dom.appendChild
    elem
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view model_s signal_update));
  Lwt.return ()
;;

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app")) (fun () -> assert false)
  in
  insert_demo parent
;;

let (_ : unit Lwt.t) = Lwt.Infix.(Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main)
