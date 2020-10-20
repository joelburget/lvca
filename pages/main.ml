open Base
open Js_of_ocaml
open ReactiveData

module Model = struct
  type page =
    | TermAndConcretePage
    | EvalWithProvenancePage
    | TermToTexPage
    | CalculatorPage

  (* | TermToDocument *)

  type t = { page : page }

  let initial_model = { page = CalculatorPage }

  let all_pages =
    [ TermAndConcretePage; EvalWithProvenancePage; TermToTexPage; CalculatorPage ]
  ;;
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
    | TermToTexPage -> "0x: term to tex"
    | CalculatorPage -> "0x: calculator"
  ;;

  let stateless_view = function
    | TermAndConcretePage -> TermAndConcrete.stateless_view
    | EvalWithProvenancePage -> EvalWithProvenance.stateless_view
    | TermToTexPage -> TermToTex.stateless_view
    | CalculatorPage -> Calculator.stateless_view
  ;;

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
      match List.nth Model.all_pages i with
      | None -> failwith "TODO: error"
      | Some page -> page
    in
    signal_update Model.{ page };
    false
  ;;

  let view model_s signal_update =
    let page_selector =
      Model.all_pages
      |> List.mapi ~f:(fun i page ->
             Html.option
               ~a:[ Html.a_value (Int.to_string i) ]
               (page |> page_description |> Html.txt))
    in
    let page_view =
      R.Html5.div
        (model_s |> React.S.map (fun { page } -> stateless_view page) |> RList.singleton_s)
    in

    [%html{|
      <div>
        <h2>LVCA demos</h2>
        <div>
          <select onchange=|} (handler signal_update) {|>
            |} (page_selector) {|
          </select>
          |}[ page_view ]{|
        </div>
      </div>
    |}]
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
