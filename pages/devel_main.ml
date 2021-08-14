open Base
open Brr
open Brr_note_kit
open Note
open Prelude

module Model = struct
  module Internal =
  [%lvca.abstract_syntax_module
  {|
  page :=
    | Term_and_concrete()
    | Calculator()
    | Eval_with_provenance()
    | Term_to_tex()
    | Parser()
    | Scope_viewer()
    | Edits()
    | Check_term()
    | Ast_operations()
    | List_nat()
    | Pcf()
    | Term_and_document()
    | Repl()
    | Ide()
    | Code_review()
    |}]

  module Page = Internal.Page.Plain

  let default_page = Page.Repl

  let all_pages =
    Page.
      [ Term_and_concrete, "term-and-concrete"
      ; Calculator, "calculator"
      ; Parser, "parser"
      ; Scope_viewer, "scope-viewer"
      ; Eval_with_provenance, "eval-with-provenance"
      ; Term_to_tex, "term-to-tex"
      ; Edits, "edits"
      ; Check_term, "check-term"
      ; Ast_operations, "ast-operations"
      ; List_nat, "list-nat"
      ; Pcf, "pcf"
      ; Term_and_document, "term-and-document"
      ; Repl, "repl"
      ; Ide, "ide"
      ; Code_review, "code-review"
      ]
  ;;
end

module View = struct
  open Model

  let page_description = function
    | Page.Term_and_concrete -> "01: term and concrete"
    | Calculator -> "02: calculator"
    | Parser -> "03: parser"
    | Scope_viewer -> "04: scope viewer"
    | Edits -> "0x: edits"
    | Eval_with_provenance -> "0x: evaluation with provenance"
    | Term_to_tex -> "0x: term to tex"
    | Check_term -> "0x: check term"
    | Ast_operations -> "0x: operations on ASTs"
    | List_nat -> "0x: List_nat"
    | Pcf -> "0x: PCF"
    | Term_and_document -> "0x: Term and document"
    | Repl -> "0x: Repl"
    | Ide -> "0x: IDE"
    | Code_review -> "0x: Code review"
  ;;

  let stateless_view = function
    | Page.Term_and_concrete -> Term_and_concrete.stateless_view
    | Calculator -> Calculator.stateless_view
    | Eval_with_provenance -> Eval_with_provenance.stateless_view
    | Term_to_tex -> Term_to_tex.stateless_view
    | Parser -> Parser.stateless_view
    | Scope_viewer -> Scope_viewer.stateless_view
    | Edits -> Edits.stateless_view
    | Check_term -> Check_term.stateless_view
    | Ast_operations -> Ast_operations.stateless_view
    | List_nat -> List_nat.stateless_view
    | Pcf -> Pcf.stateless_view
    | Term_and_document -> Term_and_document.stateless_view
    | Repl -> Repl.stateless_view
    | Ide -> Ide.stateless_view
    | Code_review ->
      fun () ->
        [%blob "make-code-review-easier.md"]
        |> Lvca_languages.Document.parse
        |> Md_viewer.view
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
              List.find Model.all_pages ~f:(fun (page', _) -> Model.Page.(page' = page))
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
