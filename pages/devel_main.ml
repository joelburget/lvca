open Base
open Brr
open Brr_note_kit
open Note
open Prelude

module Model = struct
  type page =
    | TermAndConcretePage
    | CalculatorPage
    | EvalWithProvenancePage
    | TermToTexPage
    | ParserPage
    | ScopeViewerPage
    | EditsPage

  (* | TermToDocument *)

  type t = { page : page }

  let initial_model = { page = EvalWithProvenancePage }

  let all_pages =
    [ TermAndConcretePage
    ; CalculatorPage
    ; ParserPage
    ; ScopeViewerPage
    ; EvalWithProvenancePage
    ; TermToTexPage
    ; EditsPage
    ]
  ;;
end

module View = struct
  open Model

  let page_description = function
    | TermAndConcretePage -> "01: term and concrete"
    | CalculatorPage -> "02: calculator"
    | ParserPage -> "03: parser"
    | ScopeViewerPage -> "04: scope viewer"
    | EditsPage -> "0x: edits"
    | EvalWithProvenancePage -> "0x: evaluation with provenance"
    | TermToTexPage -> "0x: term to tex"
  ;;

  let stateless_view = function
    | TermAndConcretePage -> TermAndConcrete.stateless_view
    | CalculatorPage -> Calculator.stateless_view
    | EvalWithProvenancePage -> EvalWithProvenance.stateless_view
    | TermToTexPage -> TermToTex.stateless_view
    | ParserPage -> Parser.stateless_view
    | ScopeViewerPage -> ScopeViewer.stateless_view
    | EditsPage -> Edits.stateless_view
  ;;

  let view model_s =
    let div, h2 = El.(div, h2) in
    let page_selector =
      Ui.Value_selector.Button.v
        ~dir:`V
        (fun page -> S.const [ page |> page_description |> txt ])
        (S.const Model.all_pages)
        (S.const None)
    in
    let page_view =
      mk_reactive div (model_s |> S.map (fun { page } -> [ stateless_view page () ]))
    in
    let elems =
      [ main
          ~at:(classes "container flex flex-col md:grid md:grid-cols-8")
          [ div ~at:[ class' "col-span-1" ] []
          ; div
              ~at:[ class' "col-span-7" ]
              [ h2 [ txt "LVCA demos" ]; Ui.Group.el page_selector; page_view ]
          ]
      ]
    in
    Ui.Group.action page_selector, elems
  ;;
end

let main () =
  let open Fut.Syntax in
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in
  let wrapper model_s =
    let evts, children = View.view model_s in
    let do_action = E.map (fun page _model -> Model.{ page }) evts in
    let model_s' = S.accum (S.value model_s) do_action in
    model_s', (model_s', children)
  in
  let model_s, children = S.fix Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  (match Document.find_el_by_id G.document (Jstr.v "app") with
  | None -> assert false
  | Some elem -> El.set_children elem children);
  Fut.return ()
;;

let (_ : unit Fut.t) = main ()
