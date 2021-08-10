open Base
open Brr
open Brr_note
open Lvca_provenance
open Lvca_syntax
open Note
open Prelude

let parse_tm =
  let open Lvca_parsing in
  let open Nominal.Term in
  parse_string
    (whitespace *> parse' ~comment:c_comment >>| map_info ~f:Commented.get_range)
;;

let buf = "input"
let initial_input = "fun(x. app(fun(x. app(f; x)); x))"

module Model = struct
  type t = string * Ranges.t

  let initial_model = initial_input, []

  let ( = ) (str1, ranges1) (str2, ranges2) =
    String.(str1 = str2) && Ranges.( = ) ranges1 ranges2
  ;;
end

module Examples = struct
  let mk str =
    let result =
      El.code
        ~at:(classes "bg-gray-50 p-1 font-mono text-sm cursor-pointer")
        [ El.txt' str ]
    in
    let click_event = Evr.on_el Ev.click (fun _evt -> str) result in
    result, click_event
  ;;

  let identity_ex, identity_e = mk "lam(x. x)"
  let k_ex, k_e = mk "lam(x. y. x)"
  let s_ex, s_e = mk "lam(x. y. z. ap(ap(x; z); ap(y; z)))"
  let y_ex, y_e = mk "lam(f. ap(lam(x. ap(f; ap(x; x))); lam(x. ap(f; ap(x; x)))))"
  let pattern_ex, pattern_e = mk "match(x; some(y). y; none(). 0)"
  let sum_ex, sum_e = mk "match(lst; cons(x; xs). add(x; ap(sum; xs)); empty(). 0)"
end

module Action = struct
  type t =
    | SetInput of string
    | SetInputHighlights of Ranges.t
end

module Controller = struct
  let update action (input, _ranges) =
    match action with
    | Action.SetInput input -> input, []
    | SetInputHighlights ranges -> input, ranges
  ;;
end

module View = struct
  let view model_s =
    let open Examples in
    let div, p, ul, li, code, txt' = El.(div, p, ul, li, code, txt') in
    let input_s = S.Pair.fst ~eq:String.( = ) model_s in
    let highlights_s = S.Pair.snd ~eq:Ranges.( = ) model_s in
    let click_example_e =
      Examples.[ identity_e; k_e; s_e; y_e; pattern_e; sum_e ]
      |> E.select
      |> E.map (fun str -> Action.SetInput str)
    in
    let input_elem, input_evt = Single_line_input.mk input_s ~highlights_s in
    let enter_input_e =
      input_evt
      |> E.filter_map (function
             | Common.EvaluateInput str -> Some (Action.SetInput str)
             | _ -> None)
    in
    let set_highlight_e, output_children =
      let s =
        input_s
        |> S.map (fun str ->
               match parse_tm str with
               | Error msg -> E.never, [ div [ txt' msg ] ]
               | Ok tm ->
                 let tm =
                   tm |> Nominal.Term.map_info ~f:(Source_ranges.of_opt_range ~buf)
                 in
                 let tree_view, tree_selection_e =
                   Tree_view.view_tm ~source_column:false ~range_column:false tm
                 in
                 let set_highlight_e =
                   tree_selection_e
                   |> E.filter_map (fun source_ranges ->
                          Map.find source_ranges buf
                          |> Option.map ~f:(fun ranges ->
                                 Action.SetInputHighlights ranges))
                 in
                 set_highlight_e, [ tree_view ])
      in
      S.Pair.fst s, S.Pair.snd s
    in
    let color_defn cls name desc =
      li [ code ~at:[ class' cls ] [ txt' name ]; txt' (" " ^ desc) ]
    in
    let actions = E.select [ click_example_e; enter_input_e; E.swap set_highlight_e ] in
    let elem =
      div
        [ p
            [ txt' "Try the identity function, "
            ; identity_ex
            ; txt' " or the constant function "
            ; k_ex
            ; txt' "."
            ]
        ; p
            [ txt' "Try more complicated combinators S "
            ; s_ex
            ; txt' "and Y "
            ; y_ex
            ; txt' "."
            ]
        ; p
            [ txt' "LVCA also supports pattern matching, for example "
            ; pattern_ex
            ; txt' "and "
            ; sum_ex
            ; txt' "."
            ]
        ; input_elem
        ; mk_reactive div output_children
        ; p [ txt' "Hover over a variable to see more information about it." ]
        ; ul
            [ color_defn "bg-blue-200" "blue" "shows all the uses of a variable"
            ; color_defn "bg-pink-200" "pink" "shows a variable's definition site"
            ; color_defn
                "bg-yellow-200"
                "yellow"
                "shows variables that the selected definition shadows"
            ; color_defn
                "bg-yellow-500"
                "orange"
                "shows variables that shadow the selected definition"
            ; color_defn "bg-green-50" "green" "shows the extent of a variable's scope"
            ]
        ]
    in
    actions, elem
  ;;
end

let stateless_view () =
  let wrapper model_s =
    let evts, child = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum ~eq:Model.( = ) (S.value model_s) do_action in
    model_s', (model_s', child)
  in
  let model_s, child = S.fix ~eq:Model.( = ) Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  child
;;
