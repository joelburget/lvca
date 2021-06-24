open Base
open Brr
open Brr_note_kit
open Note
open Prelude

module Model = struct
  type page =
    | Term_and_concrete_page
    | Calculator_page
    | Eval_with_provenance_page
    | Term_to_tex_page
    | Parser_page
    | Scope_viewer_page
    | Edits_page
    | Check_term_page
    | Ast_operations_page
    | List_nat_page
    | Pcf_page

  (* | TermToDocument *)

  type t = { page : page }

  let initial_model = { page = Pcf_page }

  let all_pages =
    [ Term_and_concrete_page
    ; Calculator_page
    ; Parser_page
    ; Scope_viewer_page
    ; Eval_with_provenance_page
    ; Term_to_tex_page
    ; Edits_page
    ; Check_term_page
    ; Ast_operations_page
    ; List_nat_page
    ; Pcf_page
    ]
  ;;
end

module View = struct
  open Model

  let page_description = function
    | Term_and_concrete_page -> "01: term and concrete"
    | Calculator_page -> "02: calculator"
    | Parser_page -> "03: parser"
    | Scope_viewer_page -> "04: scope viewer"
    | Edits_page -> "0x: edits"
    | Eval_with_provenance_page -> "0x: evaluation with provenance"
    | Term_to_tex_page -> "0x: term to tex"
    | Check_term_page -> "0x: check term"
    | Ast_operations_page -> "0x: operations on ASTs"
    | List_nat_page -> "0x: List_nat"
    | Pcf_page -> "0x: PCF"
  ;;

  let stateless_view = function
    | Term_and_concrete_page -> Term_and_concrete.stateless_view
    | Calculator_page -> Calculator.stateless_view
    | Eval_with_provenance_page -> Eval_with_provenance.stateless_view
    | Term_to_tex_page -> Term_to_tex.stateless_view
    | Parser_page -> Parser.stateless_view
    | Scope_viewer_page -> Scope_viewer.stateless_view
    | Edits_page -> Edits.stateless_view
    | Check_term_page -> Check_term.stateless_view
    | Ast_operations_page -> Ast_operations.stateless_view
    | List_nat_page -> List_nat.stateless_view
    | Pcf_page -> Pcf.stateless_view
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
