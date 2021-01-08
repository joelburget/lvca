open Base
open Brr
open Brr_note
open Lvca_syntax
open Note
open Prelude

module ParseNominal = Nominal.Parse(ParseUtil.CComment)
module ParsePrimitive = Primitive.Parse(ParseUtil.CComment)

let parse_tm = ParseUtil.parse_string (ParseNominal.whitespace_t ParsePrimitive.t)

let buf = "input"
(* let initial_input = "foo(p(a; b). p'(b). bar(a. a; a; b; c))" *)
let initial_input = "fun(x. app(fun(x. app(f; x)); x))"

module Model = struct
  let initial_model = initial_input, []
  let (=) (str1, ranges1) (str2, ranges2) = String.(str1 = str2) && Ranges.(=) ranges1 ranges2
end

module Examples = struct
  let mk str =
    let result = El.code
      ~at:(classes "bg-gray-50 p-1 font-mono text-sm cursor-pointer")
      [ txt str ]
    in
    let click_event, signal_event = E.create () in
    let _ : unit event = Evr.on_el Ev.click (fun _evt -> signal_event str) result in
    result, click_event
  ;;

  let identity_ex, identity_e = mk "lam(x. x)"
  let k_ex, k_e = mk "lam(x. y. x)"
  let s_ex, s_e = mk "lam(x. y. z. ap(ap(x; z); ap(y; z)))"
  let y_ex, y_e = mk "lam(f. ap(lam(x. ap(f; ap(x; x))); lam(x. ap(f; ap(x; x)))))"
  let pattern_ex, pattern_e = mk "match(x; some(y). y; none(). 0)"
  let sum_ex, sum_e = mk "match(lst; cons(x; xs). add(x; ap(sum; xs)); empty(). 0)"
end

module View = struct
  let view _model =
    let model_s, set_model = S.create ~eq:Model.(=) Model.initial_model in
    let input_s = S.Pair.fst ~eq:String.(=) model_s in
    let highlights_s = S.Pair.snd ~eq:Ranges.(=) model_s in
    let set_input_highlights hls =
      let str, _ = S.value model_s in
      set_model (str, hls)
    in

    let (_ : unit event) = Examples.[identity_e; k_e; s_e; y_e; pattern_e; sum_e]
      |> E.select
      |> E.map (fun str -> set_model (str, []))
    in

    let input_elem, input_evt = SingleLineInput.mk input_s ~highlights_s in
    let _ : unit event = input_evt |> E.map (function
      | Common.InputUpdate str -> set_model (str, [])
      | _ -> ()
    )
    in

    let output_children = input_s
      |> S.map (fun str -> match parse_tm str with
      | Error msg -> El.div [txt msg]
      | Ok tm ->
        let tm = tm |> Nominal.map_loc ~f:(SourceRanges.of_opt_range ~buf) in
        let tree_view, tree_selection_e = TreeView.view_tm
          ~source_column:false ~range_column:false tm
        in
        let _ : unit event = tree_selection_e
          |> E.map (fun source_ranges -> match Map.find source_ranges buf with
            | None -> ()
            | Some ranges -> set_input_highlights ranges
          )
        in

        tree_view
      )
      |> S.map (fun elem -> [elem])
    in

    let output_elem = El.div [] in
    let () = Elr.def_children output_elem output_children in

    let open Examples in

    let div, p, ul, li, code = El.(div, p, ul, li, code) in

    let color_defn cls name desc = li
      [ code ~at:[class' cls] [ txt name ]
      ; txt (" " ^ desc)
      ]
    in

    div
      [ p [ txt "Try the identity function, "; identity_ex; txt " or the constant function "; k_ex; txt "."]
      ; p [ txt "Try more complicated combinators S "; s_ex; txt "and Y "; y_ex; txt "."]
      ; p [ txt "LVCA also supports pattern matching, for example "; pattern_ex; txt "and "; sum_ex; txt "."]
      ; input_elem
      ; output_elem
      ; p [ txt "Hover over a variable to see more information about it." ]
      ; ul
        [ color_defn "bg-blue-200" "blue" "shows all the uses of a variable"
        ; color_defn "bg-pink-200" "pink" "shows a variable's definition site"
        ; color_defn "bg-yellow-200" "yellow" "shows variables that the selected definition shadows"
        ; color_defn "bg-yellow-500" "orange" "shows variables that shadow the selected definition"
        ; color_defn "bg-green-50" "green" "shows the extent of a variable's scope"
        ]
      ]
end

let stateless_view () = View.view Model.initial_model
