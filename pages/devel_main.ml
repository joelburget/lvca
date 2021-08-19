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
    | Concrete_syntax()
    | Diagram_language()
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

  let default_page = Page.Repl

  let all_pages =
    Page.
      [ Term_and_concrete, "term-and-concrete"
      ; Calculator, "calculator"
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
      ; Concrete_syntax, "concrete-syntax"
      ; Diagram_language, "diagram-language"
      ; Finding_terms, "finding-terms"
      ; Huttons_razor, "huttons-razor"
      ; Lambda_concrete_and_abstract, "lambda-concrete-and-abstract"
      ; Making_concrete_and_abstract, "making-concrete-and-abstract"
      ; Never_waste_a_refactor, "never-waste-a-refactor"
      ; Software_evolution, "software-evolution"
      ; Sorts_and_kind_checking, "sorts-and-kind-checking"
      ; The_interop_story, "the-interop-story"
      ; Universes, "universes"
      ; What_is_a_pl, "what-is-a-pl"
      ; What_lvca_doesnt_do, "what-lvca-doesnt-do"
      ; Why_is_lvca_interesting, "why-is-lvca-interesting"
      ; Checking_terms_and_patterns, "checking-terms-and-patterns"
      ; Church_and_curry, "church-and-curry"
      ; Comments_are_metadata, "comments-are-metadata"
      ; Constructive_real_calculator, "constructive-real-calculator"
      ; Binding_aware_patterns, "binding-aware-patterns"
      ; Binding_viewer, "binding-viewer"
      ; Are_constructors_functions, "are-constructors-functions"
      ; Bidirectional_typechecking, "bidirectional-typechecking"
      ; Parsing_language, "parsing-language"
      ; Garage_door, "garage-door"
      ; Introduction, "introduction"
      ; Abstract_syntax, "abstract-syntax"
      ; Semantic_diffs, "semantic-diffs"
      ; Semantic_diffs_and_broken_tests, "semantic-diffs-and-broken-tests"
      ; Progress_august_8_2020, "progress-august-8-2020"
      ; Progress_december_23_2020, "progress-december-23-2020"
      ; Progress_december_28_2020, "progress-december-28-2020"
      ; Progress_july_24_2020, "progress-july-24-2020"
      ; Progress_july_25_2021, "progress-july-25-2021"
      ; Progress_june_9_2021, "progress-june-9-2021"
      ; Progress_may_24_2021, "progress-may-24-2021"
      ; Progress_november_7_2020, "progress-november-7-2020"
      ; Progress_october_8_2020, "progress-october-8-2020"
      ; Progress_september_23_2020, "progress-september-23-2020"
      ]
  ;;
end

module View = struct
  open Model

  let page_title = function
    | Page.Term_and_concrete -> "01: term and concrete"
    | Calculator -> "02: calculator"
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
    | Concrete_syntax -> "Concrete Syntax"
    | Diagram_language -> "A Diagram Language"
    | Finding_terms -> "Finding Terms"
    | Huttons_razor -> "Hutton's Razor (draft)"
    | Lambda_concrete_and_abstract -> "Lambda Calculus: Concrete and Abstract"
    | Making_concrete_and_abstract -> "Making Lambda Calculus: Concrete and Abstract"
    | Never_waste_a_refactor -> "Never Waste a Refactor"
    | Software_evolution -> "Software Evolution"
    | Sorts_and_kind_checking -> "Sorts and Kind Checking"
    | The_interop_story -> "The Interop Story (draft)"
    | Universes -> "Universes (draft)"
    | What_is_a_pl -> "What is a programming language?"
    | What_lvca_doesnt_do -> "What LVCA Doesn't Do (draft)"
    | Why_is_lvca_interesting -> "Why is LVCA Interesting?"
    | Checking_terms_and_patterns -> "Checking Terms and Patterns"
    | Church_and_curry -> "Church and Curry"
    | Comments_are_metadata -> "Comments are Metadata"
    | Constructive_real_calculator -> "Constructive Real Calculator"
    | Binding_aware_patterns -> "Binding-aware Patterns"
    | Binding_viewer -> "Binding Viewer"
    | Are_constructors_functions -> "Are Constructors Functions?"
    | Bidirectional_typechecking -> "Bidirectional Typechecking"
    | Parsing_language -> "Rethinking Parsing with a Dedicated Language"
    | Garage_door -> "Working with the Garage Door Up"
    | Introduction -> "Introduction"
    | Abstract_syntax -> "Abstract syntax"
    | Semantic_diffs -> "Semantic Diffs"
    | Semantic_diffs_and_broken_tests -> "Semantic Diffs and Broken Tests"
    | Progress_august_8_2020 -> "Progress Update (August 8, 2020)"
    | Progress_december_23_2020 -> "Progress Update (December 23, 2020)"
    | Progress_december_28_2020 -> "Progress Update (December 28, 2020)"
    | Progress_july_24_2020 -> "Progress Update (July 24, 2020)"
    | Progress_july_25_2021 -> "Progress Update (July 25, 2021)"
    | Progress_june_9_2021 -> "Progress Update (June 9, 2020)"
    | Progress_may_24_2021 -> "Progress Update (May 24, 2021)"
    | Progress_november_7_2020 -> "Progress Update (November 7, 2020)"
    | Progress_october_8_2020 -> "Progress Update (October 8, 2020)"
    | Progress_september_23_2020 -> "Progress Update (September 8, 2020)"
  ;;

  let page_date = function
    | Page.Concrete_syntax -> Some "January 28, 2019"
    | Diagram_language -> Some "May 26, 2020"
    | Finding_terms -> None
    | Huttons_razor -> Some "April 7, 2020"
    | Lambda_concrete_and_abstract -> Some "08/23/2019"
    | Making_concrete_and_abstract -> Some "September 8, 2020"
    | Never_waste_a_refactor -> Some "June 5, 2021"
    | Software_evolution -> None
    | Sorts_and_kind_checking -> Some "January 21, 2020"
    | The_interop_story -> None
    | Universes -> Some "April 12, 2020"
    | What_is_a_pl -> None
    | What_lvca_doesnt_do -> None
    | Why_is_lvca_interesting -> Some "April 8, 2020"
    | Checking_terms_and_patterns -> Some "January xx, 2020"
    | Church_and_curry -> Some "July 1, 2020"
    | Comments_are_metadata -> Some "June 4, 2021"
    | Constructive_real_calculator -> Some "October 23, 2020"
    | Binding_aware_patterns -> Some "January xx, 2020"
    | Binding_viewer -> Some "January 1, 2021"
    | Are_constructors_functions -> Some "Feb 15, 2021"
    | Bidirectional_typechecking -> Some "April 16, 2020"
    | Parsing_language -> Some "Dec 3, 2020"
    | Garage_door -> Some "June 23, 2020"
    | Introduction -> Some "July 5, 2019"
    | Abstract_syntax -> Some "October 8, 2019"
    | Semantic_diffs -> Some "April 9, 2020"
    | Semantic_diffs_and_broken_tests -> Some "Feb 14, 2021"
    | Progress_august_8_2020 -> Some "August 8, 2020"
    | Progress_december_23_2020 -> Some "December 23, 2020"
    | Progress_december_28_2020 -> Some "December 28, 2020"
    | Progress_july_24_2020 -> Some "July 24, 2020"
    | Progress_july_25_2021 -> Some "July 25, 2021"
    | Progress_june_9_2021 -> Some "June 9, 2021"
    | Progress_may_24_2021 -> Some "May 24, 2021"
    | Progress_november_7_2020 -> Some "Nobember 7, 2020"
    | Progress_october_8_2020 -> Some "October 8, 2020"
    | Progress_september_23_2020 -> Some "September 23, 2020"
    | _ -> None
  ;;

  let page_tags = function
    | Page.Parsing_language -> [ "parsing" ]
    | Sorts_and_kind_checking -> [ "update"; "garage-door" ]
    | Binding_viewer -> [ "binding" ]
    | Binding_aware_patterns -> [ "binding" ]
    | Progress_august_8_2020 -> [ "update"; "garage-door" ]
    | Progress_december_23_2020 -> [ "update"; "garage-door" ]
    | Progress_december_28_2020 -> [ "update"; "garage-door" ]
    | Progress_july_24_2020 -> [ "update"; "garage-door" ]
    | Progress_july_25_2021 -> [ "update"; "garage-door" ]
    | Progress_june_9_2021 -> [ "update"; "garage-door" ]
    | Progress_may_24_2021 -> [ "update"; "garage-door" ]
    | Progress_november_7_2020 -> [ "update"; "garage-door"; "parsing" ]
    | Progress_october_8_2020 ->
      [ "update"; "garage-door"; "parsing"; "constructive-real" ]
    | Progress_september_23_2020 -> [ "update"; "garage-door" ]
    | _ -> []
  ;;

  let page_edited = function Page.Abstract_syntax -> Some "January 1, 2021" | _ -> None
  let mk_doc blob () = Md_viewer.of_string blob

  let stateless_view = function
    | Page.Term_and_concrete -> Term_and_concrete.stateless_view
    | Calculator -> Calculator.stateless_view
    | Eval_with_provenance -> Eval_with_provenance.stateless_view
    | Term_to_tex -> Term_to_tex.stateless_view
    | Scope_viewer -> Scope_viewer.stateless_view
    | Edits -> Edits.stateless_view
    | Check_term -> Check_term.stateless_view
    | Ast_operations -> Ast_operations.stateless_view
    | List_nat -> List_nat.stateless_view
    | Pcf -> Pcf.stateless_view
    | Term_and_document -> Term_and_document.stateless_view
    | Repl -> Repl.stateless_view
    | Ide -> Ide.stateless_view
    | Parsing_language -> Parser.stateless_view
    | Code_review -> mk_doc [%blob "md/make-code-review-easier.md"]
    | Concrete_syntax -> mk_doc [%blob "md/concrete-syntax.md"]
    | Diagram_language -> mk_doc [%blob "md/diagram-language.md"]
    | Finding_terms -> mk_doc [%blob "md/finding-terms.md"]
    | Huttons_razor -> mk_doc [%blob "md/huttons-razor.md"]
    | Lambda_concrete_and_abstract -> mk_doc [%blob "md/lambda-concrete-and-abstract.md"]
    | Making_concrete_and_abstract -> mk_doc [%blob "md/making-concrete-and-abstract.md"]
    | Never_waste_a_refactor -> mk_doc [%blob "md/never-waste-a-refactor.md"]
    | Software_evolution -> mk_doc [%blob "md/software-evolution.md"]
    | Sorts_and_kind_checking -> mk_doc [%blob "md/sorts-and-kind-checking.md"]
    | The_interop_story -> mk_doc [%blob "md/the-interop-story.md"]
    | Universes -> mk_doc [%blob "md/universes.md"]
    | What_is_a_pl -> mk_doc [%blob "md/what-is-a-pl.md"]
    | What_lvca_doesnt_do -> mk_doc [%blob "md/what-lvca-doesnt-do.md"]
    | Why_is_lvca_interesting -> mk_doc [%blob "md/why-is-lvca-interesting.md"]
    | Checking_terms_and_patterns -> mk_doc [%blob "md/checking-terms-and-patterns.md"]
    | Church_and_curry -> mk_doc [%blob "md/church-and-curry.md"]
    | Comments_are_metadata -> mk_doc [%blob "md/comments-are-metadata.md"]
    | Constructive_real_calculator -> mk_doc [%blob "md/constructive-real-calculator.md"]
    | Binding_aware_patterns -> mk_doc [%blob "md/binding-aware-patterns.md"]
    | Binding_viewer -> mk_doc [%blob "md/binding-viewer.md"]
    | Are_constructors_functions -> mk_doc [%blob "md/are-constructors-functions.md"]
    | Bidirectional_typechecking -> mk_doc [%blob "md/bidirectional-typechecking.md"]
    | Garage_door -> mk_doc [%blob "md/garage-door.md"]
    | Introduction -> mk_doc [%blob "md/introduction.md"]
    | Abstract_syntax -> mk_doc [%blob "md/abstract-syntax.md"]
    | Semantic_diffs -> mk_doc [%blob "md/semantic-diffs.md"]
    | Semantic_diffs_and_broken_tests ->
      mk_doc [%blob "md/semantic-diffs-and-broken-tests.md"]
    | Progress_august_8_2020 -> mk_doc [%blob "md/progress-august-8-2020.md"]
    | Progress_december_23_2020 -> mk_doc [%blob "md/progress-december-23-2020.md"]
    | Progress_december_28_2020 -> mk_doc [%blob "md/progress-december-28-2020.md"]
    | Progress_july_24_2020 -> mk_doc [%blob "md/progress-july-24-2020.md"]
    | Progress_july_25_2021 -> mk_doc [%blob "md/progress-july-25-2021.md"]
    | Progress_june_9_2021 -> mk_doc [%blob "md/progress-june-9-2021.md"]
    | Progress_may_24_2021 -> mk_doc [%blob "md/progress-may-24-2021.md"]
    | Progress_november_7_2020 -> mk_doc [%blob "md/progress-november-7-2020.md"]
    | Progress_october_8_2020 -> mk_doc [%blob "md/progress-october-8-2020.md"]
    | Progress_september_23_2020 -> mk_doc [%blob "md/progress-september-23-2020.md"]
  ;;

  let view model_s =
    let div, h2, txt' = El.(div, h2, txt') in
    let page_selector =
      Ui.Value_selector.Menu.v
        Lvca_util.(page_title >> Jstr.v)
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
