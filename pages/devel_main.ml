open Base
open Brr
open Brr_note
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

  let initial_model = { page = ScopeViewerPage }

  let all_pages =
    [ TermAndConcretePage
    ; CalculatorPage
    ; ParserPage
    ; EvalWithProvenancePage
    ; TermToTexPage
    ; ScopeViewerPage
    ; EditsPage
    ]
  ;;
end

type signal = Model.t Note.signal

(*
module Action = struct
  type t = ChangePage of Model.page
end

module Controller = struct
  let update (action : Action.t) _model_s signal_update =
    let (ChangePage page) = action in
    signal_update Model.{ page }
  ;;
end
*)

module View = struct
  open Model

  let page_description = function
    | TermAndConcretePage -> "01: term and concrete"
    | CalculatorPage -> "02: calculator"
    | ParserPage -> "03: parser"
    | ScopeViewerPage -> "0x: scope viewer"
    | EditsPage -> "0x: edits"
    | EvalWithProvenancePage -> "0x: evaluation with provenance"
    | TermToTexPage -> "0x: term to tex"
  ;;

  let stateless_view =
    let txt str = El.txt (Jstr.v str) in
    function
    | TermAndConcretePage -> TermAndConcrete.stateless_view ()
    | CalculatorPage -> Calculator.stateless_view ()
    | EvalWithProvenancePage ->
      txt "TODO (EvalWithProvenancePage)"
      (* EvalWithProvenance.stateless_view *)
    | TermToTexPage ->
      txt "TODO (TermToTexPage)"
      (* TermToTex.stateless_view *)
    | ParserPage ->
      txt "TODO (ParserPage)"
      (* Parser.stateless_view *)
    | ScopeViewerPage ->
      txt "TODO (ScopeViewerPage)"
      (* ScopeViewer.stateless_view *)
    | EditsPage ->
      txt "TODO (EditsPage)"
      (* Edits.stateless_view *)
  ;;

  let handler signal_update elem _evt =
    let i = El.prop El.Prop.value elem |> Jstr.to_string |> Int.of_string in
    let page =
      match List.nth Model.all_pages i with
      | None -> failwith "TODO: error"
      | Some page -> page
    in
    signal_update Model.{ page };
    false
  ;;

  let view model_s signal_update =
    let pages = Model.all_pages
      |> List.mapi ~f:(fun i page -> El.option
        ~at:At.[ value (Jstr.v (Int.to_string i)) ]
        [page |> page_description |> Jstr.v |> El.txt]
      )
    in
    let page_selector = El.select ~at:(classes "mb-8 mt-2") pages in
    (* XXX use event instead of setting in handler *)
    let _ : bool event = Evr.on_el Ev.change (handler signal_update page_selector) page_selector in

    let page_view = El.div [] in
    let () = Elr.def_children page_view
      (model_s |> S.map (fun { page } -> [stateless_view page]))
    in

    El.(div
      [ main ~at:(classes "container flex flex-col md:grid md:grid-cols-8")
        [ div ~at:At.[class' (Jstr.v "col-span-1")] []
        ; div ~at:At.[class' (Jstr.v "col-span-7")]
          [ h2 [ El.txt (Jstr.v "LVCA demos") ]
          ; page_selector
          ; page_view
          ]
        ]
      ]
    )
  ;;
end

let main () =
  let open Fut.Syntax in
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in

  let model_s, model_set = S.create Model.initial_model in

  (match Document.find_el_by_id G.document (Jstr.v "app") with
    | None -> assert false
    | Some elem -> El.set_children elem [View.view model_s model_set]);
  Fut.return ()

let () = ignore (main ())
