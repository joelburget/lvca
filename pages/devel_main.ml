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
    | Term_and_document_page
    | Repl_page
    | Ide_page

  let ( = ) p1 p2 =
    match p1, p2 with
    | Term_and_concrete_page, Term_and_concrete_page
    | Calculator_page, Calculator_page
    | Eval_with_provenance_page, Eval_with_provenance_page
    | Term_to_tex_page, Term_to_tex_page
    | Parser_page, Parser_page
    | Scope_viewer_page, Scope_viewer_page
    | Edits_page, Edits_page
    | Check_term_page, Check_term_page
    | Ast_operations_page, Ast_operations_page
    | List_nat_page, List_nat_page
    | Pcf_page, Pcf_page
    | Term_and_document_page, Term_and_document_page
    | Repl_page, Repl_page
    | Ide_page, Ide_page ->
      true
    | _, _ -> false
  ;;

  let default_page = Repl_page

  let all_pages =
    [ Term_and_concrete_page, "term-and-concrete"
    ; Calculator_page, "calculator"
    ; Parser_page, "parser"
    ; Scope_viewer_page, "scope-viewer"
    ; Eval_with_provenance_page, "eval-with-provenance"
    ; Term_to_tex_page, "term-to-tex"
    ; Edits_page, "edits"
    ; Check_term_page, "check-term"
    ; Ast_operations_page, "ast-operations"
    ; List_nat_page, "list-nat"
    ; Pcf_page, "pcf"
    ; Term_and_document_page, "term-and-document"
    ; Repl_page, "repl"
    ; Ide_page, "ide"
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
    | Term_and_document_page -> "0x: Term and document"
    | Repl_page -> "0x: Repl"
    | Ide_page -> "0x: IDE"
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
    | Term_and_document_page -> Term_and_document.stateless_view
    | Repl_page -> Repl.stateless_view
    | Ide_page -> Ide.stateless_view
  ;;

  let view model_s =
    let div, h2, txt' = El.(div, h2, txt') in
    let page_selector =
      Ui.Value_selector.Menu.v
        Lvca_util.(page_description >> Jstr.v)
        (Model.all_pages |> List.map ~f:fst |> S.const)
        model_s
    in
    let page_view =
      mk_reactive div (model_s |> S.map (fun page -> [ stateless_view page () ]))
    in
    let elems =
      [ main
          ~at:(classes "container flex flex-col md:grid md:grid-cols-8")
          [ div ~at:[ class' "col-span-1" ] []
          ; div
              ~at:[ class' "col-span-7" ]
              [ h2 [ txt' "LVCA demos" ]
              ; Ui.Value_selector.Menu.el page_selector
              ; page_view
              ]
          ]
      ]
    in
    Ui.Value_selector.Menu.action page_selector, elems
  ;;
end

let main () =
  let open Fut.Syntax in
  let* _ev = Ev.next Ev.load (Window.as_target G.window) in
  let initial_uri = G.window |> Window.location in
  let wrapper model_s =
    let evts, children = View.view model_s in
    let do_action =
      E.map
        (fun page _model ->
          let page_name =
            match
              List.find Model.all_pages ~f:(fun (page', _) -> Model.(page' = page))
            with
            | None -> assert false
            | Some (_, name) -> name
          in
          let path = Jstr.v ("/" ^ page_name ^ "/") in
          (match Uri.with_uri ~path initial_uri with
          | Ok uri -> Window.set_location G.window uri
          | Error _ -> ());
          page)
        evts
    in
    let model_s' = S.accum (S.value model_s) do_action in
    model_s', (model_s', children)
  in
  let path_components =
    initial_uri |> Uri.path |> Jstr.to_string |> String.split ~on:'/'
  in
  let page =
    match path_components with
    | [ ""; pc_name; "" ] ->
      (match List.find Model.all_pages ~f:(fun (_, name) -> String.(name = pc_name)) with
      | None -> Model.default_page
      | Some (page, _) -> page)
    | _ -> Model.default_page
  in
  let model_s, children = S.fix page wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  (match Document.find_el_by_id G.document (Jstr.v "app") with
  | None -> assert false
  | Some elem -> El.set_children elem children);
  Fut.return ()
;;

let (_ : unit Fut.t) = main ()
