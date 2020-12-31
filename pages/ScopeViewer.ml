open Base
open Lvca_syntax
open ReactiveData
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module RHtml = Js_of_ocaml_tyxml.Tyxml_js.R.Html

module ParseNominal = Nominal.Parse(ParseUtil.CComment)
module ParsePrimitive = Primitive.Parse(ParseUtil.CComment)

let parse_tm = ParseUtil.parse_string (ParseNominal.whitespace_t ParsePrimitive.t)

module Model = struct
  let initial_model = ()
end

let buf = "input"
(* let initial_input = "foo(p(a; b). p'(b). bar(a. a; a; b; c))" *)
let initial_input = "fun(x. app(fun(x. app(f; x)); x))"

module View = struct
  let view _model =
    let input_s, set_input = React.S.create ~eq:String.(=) initial_input in

    let highlights_s, set_input_highlights = React.S.create [] in

    let input_elem, input_evt = SingleLineInput.mk input_s ~highlights_s in
    let _ : unit React.event = input_evt |> React.E.map (function
      | Common.InputUpdate str -> set_input str
      | _ -> ()
    )
    in

    let output_elem = input_s
      |> React.S.map (fun str -> match parse_tm str with
      | Error msg -> [%html{|<div>|}[Html.txt msg]{|</div>|}]
      | Ok tm ->
        let tm = tm |> Nominal.map_loc ~f:(SourceRanges.of_opt_range ~buf) in
        let tree_view, tree_selection_e = TreeView.view_tm ~source_column:false tm in
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

    [%html{|
      <div>
      |}[input_elem]{|
      |}[output_elem]{|
        <p>Hover over a variable to see more information about it.</p>
        <ul>
          <li><code class="bg-blue-200">blue</code> shows all the uses of a variable</li>
          <li><code class="bg-pink-200">pink</code> shows a variable's definition site</li>
          <li><code class="bg-yellow-200">yellow</code> shows variables that the selected definition shadows</li>
          <li><code class="bg-yellow-500">orange</code> shows variables that are shadowed by the selected definition</li>
          <li><code class="bg-green-50">green</code> shows the extent of a variable's scope</li>
        </ul>
      </div>
    |}]
end

let stateless_view () = View.view Model.initial_model
