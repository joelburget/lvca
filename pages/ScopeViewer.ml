open Base
open Lvca_syntax
open ReactiveData
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module RHtml = Js_of_ocaml_tyxml.Tyxml_js.R.Html

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
    let open Js_of_ocaml_tyxml.Tyxml_js in
    let result = Html.(code
      ~a:[a_class ["bg-gray-50"; "p-1"; "font-mono"; "text-sm"; "cursor-pointer"]]
      [ txt str ]
    )
    in
    let result_dom = To_dom.of_code result in
    let click_event, signal_event = React.E.create () in
    Common.bind_event Common.Ev.clicks result_dom (fun _evt ->
      signal_event str;
      Lwt.return ());
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
    let model_s, set_model = React.S.create ~eq:Model.(=) Model.initial_model in
    let input_s = React.S.Pair.fst ~eq:String.(=) model_s in
    let highlights_s = React.S.Pair.snd ~eq:Ranges.(=) model_s in
    let set_input_highlights hls =
      let str, _ = React.S.value model_s in
      set_model (str, hls)
    in

    let (_ : unit React.event) = Examples.[identity_e; k_e; s_e; y_e; pattern_e; sum_e]
      |> React.E.select
      |> React.E.map (fun str -> set_model (str, []))
    in

    let input_elem, input_evt = SingleLineInput.mk input_s ~highlights_s in
    let _ : unit React.event = input_evt |> React.E.map (function
      | Common.InputUpdate str -> set_model (str, [])
      | _ -> ()
    )
    in

    let output_elem = input_s
      |> React.S.map (fun str -> match parse_tm str with
      | Error msg -> [%html{|<div>|}[Html.txt msg]{|</div>|}]
      | Ok tm ->
        let tm = tm |> Nominal.map_loc ~f:(SourceRanges.of_opt_range ~buf) in
        let tree_view, tree_selection_e = TreeView.view_tm
          ~source_column:false ~range_column:false tm
        in
        let _ : unit React.event = tree_selection_e
          |> React.E.map (fun source_ranges -> match Map.find source_ranges buf with
            | None -> ()
            | Some ranges -> set_input_highlights ranges
          )
        in

        tree_view
      )
      |> RList.singleton_s
      |> RHtml.div
    in

    let open Examples in

    [%html{|
      <div>
      <p>Try the identity function, |}[identity_ex]{|, or the constant function |}[k_ex]{|.</p>
      <p>Try more complicated combinators S |}[s_ex]{| and Y |}[y_ex]{|.</p>
      <p>LVCA also supports pattern matching, for example |}[pattern_ex]{| and |}[sum_ex]{|.</p>
      |}[input_elem]{|
      |}[output_elem]{|
      <p>Hover over a variable to see more information about it.</p>
      <ul>
        <li><code class="bg-blue-200">blue</code> shows all the uses of a variable</li>
        <li><code class="bg-pink-200">pink</code> shows a variable's definition site</li>
        <li><code class="bg-yellow-200">yellow</code> shows variables that the selected definition shadows</li>
        <li><code class="bg-yellow-500">orange</code> shows variables that shadow the selected definition</li>
        <li><code class="bg-green-50">green</code> shows the extent of a variable's scope</li>
      </ul>
      </div>
    |}]
end

let stateless_view () = View.view Model.initial_model
