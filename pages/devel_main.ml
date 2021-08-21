open Base
open Brr
open Brr_note_kit
open CalendarLib
open Note
open Prelude
open Lvca_util

module Model = struct
  module Internal =
  [%lvca.abstract_syntax_module
  {|
  page :=
    | Term_and_concrete()
    | Calculator()
    | Eval_with_provenance()
    | Term_to_tex()
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

    // moved from blog
    | Finding_terms()
    | Huttons_razor()
    | Lambda_concrete_and_abstract()
    | Making_concrete_and_abstract()
    | Never_waste_a_refactor()
    | Software_evolution()
    | Sorts_and_kind_checking()
    | The_interop_story()
    | Universes()
    | What_is_a_pl()
    | What_lvca_doesnt_do()
    | Why_is_lvca_interesting()
    | Checking_terms_and_patterns()
    | Church_and_curry()
    | Comments_are_metadata()
    | Constructive_real_calculator()
    | Binding_aware_patterns()
    | Binding_viewer()
    | Are_constructors_functions()
    | Bidirectional_typechecking()
    | Parsing_language()
    | Garage_door()
    | Introduction()
    | Abstract_syntax()
    | Semantic_diffs()
    | Semantic_diffs_and_broken_tests()
    | Progress_august_8_2020()
    | Progress_december_23_2020()
    | Progress_december_28_2020()
    | Progress_july_24_2020()
    | Progress_july_25_2021()
    | Progress_june_9_2021()
    | Progress_may_24_2021()
    | Progress_november_7_2020()
    | Progress_october_8_2020()
    | Progress_september_23_2020()
    |}]

  module Page = Internal.Page.Plain

  module Page_info = struct
    type t =
      { slug : string
      ; title : string
      ; published : Date.t option
      ; edited : Date.t option
      ; tags : string list
      ; view : unit -> El.t
      }

    let mk ?published ?(edited = None) ?(tags = []) slug title view =
      { slug; published; edited; tags; title; view }
    ;;
  end

  let lang = Internal.language

  let comments =
    match lang.sort_defs with
    | [ (_, Sort_def (_, operator_defs)) ] ->
      List.map operator_defs ~f:(fun (Operator_def (info, name, _arity)) ->
          name, Lvca_provenance.Commented.get_comment info)
      |> String.Map.of_alist_exn
    | _ -> failwith "Unexpected number of sort defs"
  ;;

  let default_page = Page.Repl
  let mk_doc blob () = Md_viewer.of_string blob

  let page_info =
    let mk = Page_info.mk in
    function
    | Page.Term_and_concrete ->
      mk "term-and-concrete" "01: term and concrete" Term_and_concrete.Stateless_view.view
    | Calculator -> mk "calculator" "02: calculator" Calculator.Stateless_view.view
    | Scope_viewer ->
      mk "scope-viewer" "04: scope viewer" Scope_viewer.Stateless_view.view
    | Edits -> mk "edits" "0x: edits" Edits.stateless_view
    | Eval_with_provenance ->
      mk
        "eval-with-provenance"
        "0x: evaluation with provenance"
        Eval_with_provenance.Stateless_view.view
    | Term_to_tex -> mk "term-to-tex" "0x: term to tex" Term_to_tex.Stateless_view.view
    | Check_term -> mk "check-term" "0x: check term" Check_term.Stateless_view.view
    | Ast_operations ->
      mk "ast-operations" "0x: operations on ASTs" Ast_operations.stateless_view
    | List_nat -> mk "list-nat" "0x: List_nat" List_nat.stateless_view
    | Pcf -> mk "pcf" "0x: PCF" Pcf.Stateless_view.view
    | Term_and_document ->
      mk "term-and-document" "0x: Term and document" Term_and_document.Stateless_view.view
    | Repl -> mk "repl" "0x: Repl" Repl.Stateless_view.view
    | Ide -> mk "ide" "0x: IDE" Ide.Stateless_view.view
    | Code_review ->
      mk "code-review" "0x: Code review" (mk_doc [%blob "md/make-code-review-easier.md"])
    | Finding_terms ->
      mk "finding-terms" "Finding Terms" (mk_doc [%blob "md/finding-terms.md"])
    | Huttons_razor ->
      mk
        ~published:(Date.make 2020 4 7)
        "huttons-razor"
        "Hutton's Razor (draft)"
        (mk_doc [%blob "md/huttons-razor.md"])
    | Lambda_concrete_and_abstract ->
      mk
        ~published:(Date.make 2019 8 23)
        "lambda-concrete-and-abstract"
        "Lambda Calculus: Concrete and Abstract"
        (mk_doc [%blob "md/lambda-concrete-and-abstract.md"])
    | Making_concrete_and_abstract ->
      mk
        ~published:(Date.make 2020 9 8)
        "making-concrete-and-abstract"
        "Making Lambda Calculus: Concrete and Abstract"
        (mk_doc [%blob "md/making-concrete-and-abstract.md"])
    | Never_waste_a_refactor ->
      mk
        ~published:(Date.make 2021 6 5)
        "never-waste-a-refactor"
        "Never Waste a Refactor"
        (mk_doc [%blob "md/never-waste-a-refactor.md"])
    | Software_evolution ->
      mk
        "software-evolution"
        "Software Evolution"
        (mk_doc [%blob "md/software-evolution.md"])
    | Sorts_and_kind_checking ->
      mk
        ~published:(Date.make 2020 1 21)
        ~tags:[ "update"; "garage-door" ]
        "sorts-and-kind-checking"
        "Sorts and Kind Checking"
        (mk_doc [%blob "md/sorts-and-kind-checking.md"])
    | The_interop_story ->
      mk
        "the-interop-story"
        "The Interop Story (draft)"
        (mk_doc [%blob "md/the-interop-story.md"])
    | Universes ->
      mk
        ~published:(Date.make 2020 4 12)
        "universes"
        "Universes (draft)"
        (mk_doc [%blob "md/universes.md"])
    | What_is_a_pl ->
      mk
        "what-is-a-pl"
        "What is a programming language?"
        (mk_doc [%blob "md/what-is-a-pl.md"])
    | What_lvca_doesnt_do ->
      mk
        "what-lvca-doesnt-do"
        "What LVCA Doesn't Do (draft)"
        (mk_doc [%blob "md/what-lvca-doesnt-do.md"])
    | Why_is_lvca_interesting ->
      mk
        ~published:(Date.make 2020 4 8)
        "why-is-lvca-interesting"
        "Why is LVCA Interesting?"
        (mk_doc [%blob "md/why-is-lvca-interesting.md"])
    | Checking_terms_and_patterns ->
      mk
        "checking-terms-and-patterns"
        "Checking Terms and Patterns"
        (mk_doc [%blob "md/checking-terms-and-patterns.md"])
    | Church_and_curry ->
      mk
        ~published:(Date.make 2020 7 1)
        "church-and-curry"
        "Church and Curry"
        (mk_doc [%blob "md/church-and-curry.md"])
    | Comments_are_metadata ->
      mk
        ~published:(Date.make 2021 6 4)
        "comments-are-metadata"
        "Comments are Metadata"
        (mk_doc [%blob "md/comments-are-metadata.md"])
    | Constructive_real_calculator ->
      mk
        ~published:(Date.make 2020 10 23)
        "constructive-real-calculator"
        "Constructive Real Calculator"
        (mk_doc [%blob "md/constructive-real-calculator.md"])
    | Binding_aware_patterns ->
      mk
        ~tags:[ "binding" ]
        "binding-aware-patterns"
        "Binding-aware Patterns"
        (mk_doc [%blob "md/binding-aware-patterns.md"])
    | Binding_viewer ->
      mk
        ~published:(Date.make 2021 1 1)
        ~tags:[ "binding" ]
        "binding-viewer"
        "Binding Viewer"
        (mk_doc [%blob "md/binding-viewer.md"])
    | Are_constructors_functions ->
      mk
        ~published:(Date.make 2021 2 15)
        "are-constructors-functions"
        "Are Constructors Functions?"
        (mk_doc [%blob "md/are-constructors-functions.md"])
    | Bidirectional_typechecking ->
      mk
        ~published:(Date.make 2021 4 16)
        "bidirectional-typechecking"
        "Bidirectional Typechecking"
        (mk_doc [%blob "md/bidirectional-typechecking.md"])
    | Parsing_language ->
      mk
        ~published:(Date.make 2020 12 3)
        ~tags:[ "parsing" ]
        "parsing-language"
        "Rethinking Parsing with a Dedicated Language"
        Parser.stateless_view
    | Garage_door ->
      mk
        ~published:(Date.make 2020 6 23)
        "garage-door"
        "Working with the Garage Door Up"
        (mk_doc [%blob "md/garage-door.md"])
    | Introduction ->
      mk
        ~published:(Date.make 2019 7 5)
        "introduction"
        "Introduction"
        (mk_doc [%blob "md/introduction.md"])
    | Abstract_syntax ->
      mk
        ~published:(Date.make 2019 10 8)
        "abstract-syntax"
        "Abstract syntax"
        (mk_doc [%blob "md/abstract-syntax.md"])
    | Semantic_diffs ->
      mk
        ~published:(Date.make 2020 4 9)
        "semantic-diffs"
        "Semantic Diffs"
        (mk_doc [%blob "md/semantic-diffs.md"])
    | Semantic_diffs_and_broken_tests ->
      mk
        ~published:(Date.make 2021 2 14)
        "semantic-diffs-and-broken-tests"
        "Semantic Diffs and Broken Tests"
        (mk_doc [%blob "md/semantic-diffs-and-broken-tests.md"])
    | Progress_august_8_2020 ->
      mk
        ~published:(Date.make 2020 8 8)
        ~tags:[ "update"; "garage-door" ]
        "progress-august-8-2020"
        "Progress Update (August 8, 2020)"
        (mk_doc [%blob "md/progress-august-8-2020.md"])
    | Progress_december_23_2020 ->
      mk
        ~published:(Date.make 2020 12 23)
        ~tags:[ "update"; "garage-door" ]
        "progress-december-23-2020"
        "Progress Update (December 23, 2020)"
        (mk_doc [%blob "md/progress-december-23-2020.md"])
    | Progress_december_28_2020 ->
      mk
        ~published:(Date.make 2020 12 28)
        ~tags:[ "update"; "garage-door" ]
        "progress-december-28-2020"
        "Progress Update (December 28, 2020)"
        (mk_doc [%blob "md/progress-december-28-2020.md"])
    | Progress_july_24_2020 ->
      mk
        ~published:(Date.make 2020 7 24)
        ~tags:[ "update"; "garage-door" ]
        "progress-july-24-2020"
        "Progress Update (July 24, 2020)"
        (mk_doc [%blob "md/progress-july-24-2020.md"])
    | Progress_july_25_2021 ->
      mk
        ~published:(Date.make 2021 7 25)
        ~tags:[ "update"; "garage-door" ]
        "progress-july-25-2021"
        "Progress Update (July 25, 2021)"
        (mk_doc [%blob "md/progress-july-25-2021.md"])
    | Progress_june_9_2021 ->
      mk
        ~published:(Date.make 2020 6 9)
        ~tags:[ "update"; "garage-door" ]
        "progress-june-9-2021"
        "Progress Update (June 9, 2020)"
        (mk_doc [%blob "md/progress-june-9-2021.md"])
    | Progress_may_24_2021 ->
      mk
        ~published:(Date.make 2021 4 24)
        ~tags:[ "update"; "garage-door" ]
        "progress-may-24-2021"
        "Progress Update (May 24, 2021)"
        (mk_doc [%blob "md/progress-may-24-2021.md"])
    | Progress_november_7_2020 ->
      mk
        ~published:(Date.make 2020 11 7)
        ~tags:[ "update"; "garage-door"; "parsing" ]
        "progress-november-7-2020"
        "Progress Update (November 7, 2020)"
        (mk_doc [%blob "md/progress-november-7-2020.md"])
    | Progress_october_8_2020 ->
      mk
        ~published:(Date.make 2020 10 8)
        ~tags:[ "update"; "garage-door"; "parsing"; "constructive-real" ]
        "progress-october-8-2020"
        "Progress Update (October 8, 2020)"
        (mk_doc [%blob "md/progress-october-8-2020.md"])
    | Progress_september_23_2020 ->
      mk
        ~published:(Date.make 2020 9 23)
        ~tags:[ "update"; "garage-door" ]
        "progress-september-23-2020"
        "Progress Update (September 8, 2020)"
        (mk_doc [%blob "md/progress-september-23-2020.md"])
  ;;

  let all_pages =
    Page.
      [ Term_and_concrete
      ; Calculator
      ; Scope_viewer
      ; Eval_with_provenance
      ; Term_to_tex
      ; Edits
      ; Check_term
      ; Ast_operations
      ; List_nat
      ; Pcf
      ; Term_and_document
      ; Repl
      ; Ide
      ; Code_review
      ; Finding_terms
      ; Huttons_razor
      ; Lambda_concrete_and_abstract
      ; Making_concrete_and_abstract
      ; Never_waste_a_refactor
      ; Software_evolution
      ; Sorts_and_kind_checking
      ; The_interop_story
      ; Universes
      ; What_is_a_pl
      ; What_lvca_doesnt_do
      ; Why_is_lvca_interesting
      ; Checking_terms_and_patterns
      ; Church_and_curry
      ; Comments_are_metadata
      ; Constructive_real_calculator
      ; Binding_aware_patterns
      ; Binding_viewer
      ; Are_constructors_functions
      ; Bidirectional_typechecking
      ; Parsing_language
      ; Garage_door
      ; Introduction
      ; Abstract_syntax
      ; Semantic_diffs
      ; Semantic_diffs_and_broken_tests
      ; Progress_august_8_2020
      ; Progress_december_23_2020
      ; Progress_december_28_2020
      ; Progress_july_24_2020
      ; Progress_july_25_2021
      ; Progress_june_9_2021
      ; Progress_may_24_2021
      ; Progress_november_7_2020
      ; Progress_october_8_2020
      ; Progress_september_23_2020
      ]
  ;;
end

module View = struct
  let title page = Model.((page_info page).title)
  let view page = Model.((page_info page).view)

  let view model_s =
    let div, h2, txt' = El.(div, h2, txt') in
    let page_selector =
      Ui.Value_selector.Menu.v (title >> Jstr.v) (Model.all_pages |> S.const) model_s
    in
    let page_view = mk_reactive div (model_s |> S.map (fun page -> [ view page () ])) in
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
              List.find Model.all_pages ~f:(fun page' -> Model.Page.(page' = page))
            with
            | None -> assert false
            | Some page -> Model.((page_info page).slug)
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
      (match
         List.find Model.all_pages ~f:(fun page ->
             String.(Model.((page_info page).slug) = pc_name))
       with
      | None -> Model.default_page
      | Some page -> page)
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
