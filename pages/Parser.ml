open Base
open Lvca_core
open Lvca_syntax
open ReactiveData

module P = Lvca_languages.Parser
module ParseCore = Core.Parse (ParseUtil.CComment)
module ParseParser = P.Parse (ParseUtil.CComment)
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
module Html = Tyxml_js.Html
module RHtml = Tyxml_js.R.Html
module Ev = Js_of_ocaml_lwt.Lwt_js_events
module React = SafeReact

open Components

let html_eq : 'a Html.elt -> 'a Html.elt -> bool = Caml.(=)
let rhtml_eq : 'a RHtml.elt -> 'a RHtml.elt -> bool = Caml.(=)

module Model = struct
  type input_sig = string React.signal * (string -> unit)

  module TraceSnapshot = struct
    type t =
      { success: bool
      ; pre_pos: int
      ; post_pos: int
      ; parser: SourceRanges.t P.t
      ; snapshots: t list
      }

    let rec (=) t1 t2 = Bool.(t1.success = t2.success) &&
      Int.(t1.pre_pos = t2.pre_pos) &&
      Int.(t1.post_pos = t2.post_pos) &&
      List.equal (=) t1.snapshots t2.snapshots &&
      P.equal SourceRanges.(=) t1.parser t2.parser

    let rec restrict_snapshot : P.Direct.trace_snapshot -> t
      = fun { success; pre_pos; post_pos; parser; snapshots; _ } ->
        { success; pre_pos; post_pos; parser;
          snapshots = List.map snapshots ~f:restrict_snapshot }
  end

  type t =
    { any_char_input: input_sig
    ; char_input: input_sig
    ; string_input: input_sig
    ; satisfy1_input: input_sig
    ; satisfy_is_alpha_input: input_sig
    ; satisfy_is_digit_input: input_sig
    ; star_input: input_sig
    ; plus_input: input_sig
    ; choice1_input: input_sig
    ; choice2_input: input_sig
    ; choice3_input: input_sig
    ; count_input: input_sig
    ; let_input: input_sig
    ; fail_input: input_sig
    ; sequence1_input: input_sig
    ; sequence2_input: input_sig
    ; fix_input: input_sig
    ; playground_input: input_sig
    }

  let mk str =
    let s, update = React.S.create ~eq:String.(=) str in
    let handler str = update str in
    s, handler

  let initial_model =
    { any_char_input = mk "c"
    ; char_input = mk "cat"
    ; string_input = mk "food"
    ; satisfy1_input = mk "c"
    ; satisfy_is_alpha_input = mk "c"
    ; satisfy_is_digit_input = mk "c"
    ; star_input = mk "ccc"
    ; plus_input = mk "ccc"
    ; choice1_input = mk "foo"
    ; choice2_input = mk "ab"
    ; choice3_input = mk "foo"
    ; count_input = mk "cc"
    ; let_input = mk "ccc"
    ; fail_input = mk "doesn't matter"
    ; sequence1_input = mk "1 + 2"
    ; sequence2_input = mk "1 + 2"
    ; fix_input = mk "x + 90 + y"
    ; playground_input = mk "1 + 2"
    }
end

let parse_parser = ParseUtil.parse_string (ParseParser.t ParseCore.term)

module Examples = struct
  let any_char = "."
  let char = "'c'"
  let string = {|"foo"|}
  let many = "'c'*"
  let plus = "'c'+"
  (* let count = "'c'{{2}}" *)
  let zero_choice = {|choice ()|}
  let two_choice = {|choice ('c' | "foo")|}
  let multi_choice = {|choice (
  | "abc"
  | "ab"
  | "a"
  | "abcd" // never matches
)|}
  let let_ = {|let p1 = "str" in let p2 = 'c'* in choice (p1 | p2)|}
  let fail = {|fail {"some reason for failing"}|}
  let satisfy1 = {|satisfy (c -> match c with {
  | 'c' -> {true()}
  | _ -> {false()}
})|}
  let satisfy_is_alpha = "satisfy(c -> {is_alpha(c)})"
  let satisfy_is_digit = "satisfy(c -> {is_digit(c)})"
  let sequence1 = {|. ' '* '+' ' '* . -> {"parsed an addition"}|}
  let sequence2 = "a=. ' '* '+' ' '* b=. -> {plus(a; b)}"
  let fix = {|let atom = choice (name | literal) in
fix (expr -> choice (
  | a=atom ' '* '+' ' '* e=expr -> {plus(a; e)}
  | a=atom -> {a}
))|}
end

module Prelude = struct
  let parse_parser_exn str = str
    |> ParseUtil.parse_string (ParseParser.t ParseCore.term)
    |> Result.ok_or_failwith

  let alpha = parse_parser_exn Examples.satisfy_is_alpha
  let digit = parse_parser_exn Examples.satisfy_is_digit
  let name = parse_parser_exn {|chars=alpha+ -> {var(chars)}|}
  let literal = parse_parser_exn {|chars=digit+ -> {literal(chars)}|}

  let ctx : P.Direct.parser_ctx
    = Lvca_util.String.Map.of_alist_exn
      [ "alpha", alpha
      ; "digit", digit
      ; "name", name
      ; "literal", literal
      ]
      |> Map.map ~f:(P.map_loc ~f:(SourceRanges.of_opt_range ~buf:"prelude"))
end

let pp_view ~highlight_s tm fmt =
  let selection_s, set_selection =
    React.S.create ~eq:SourceRanges.(=) SourceRanges.empty
  in
  let elt, formatter, clear = RangeFormatter.mk ~selection_s:highlight_s ~set_selection in

  (* TODO tie this to actual font size *)
  let font_size = 14. in
  let font_adjust = font_size *. 0.6 (* 0.6 is an estimate of font width to height *) in
  let initial_width = 40. *. font_adjust in

  let px_size_s, set_px_size = React.S.create ~eq:Float.(=) initial_width in

  let open Js_of_ocaml in
  if (ResizeObserver.is_supported ()) then (
    let open Tyxml_js in
    let target = To_dom.of_code elt in
    let node = (target :> Dom.node Js.t) in

    let f entries _observer =
      match Js.array_get entries 0 |> Js.Optdef.to_option with
        | None -> ()
        | Some entry0 ->
          match Js.array_get entry0##.contentBoxSize 0 |> Js.Optdef.to_option with
            | None -> ()
            | Some box -> set_px_size box##.inlineSize
    in

    let _ : ResizeObserver.resizeObserver Js.t = ResizeObserver.observe ~node ~f () in
    ()
  ) else ();

  let char_size_s = px_size_s |> React.S.map ~eq:Int.(=)
    (fun size -> Int.of_float (size /. font_adjust))
  in
  let _ : unit React.signal = char_size_s |> React.S.map ~eq:Unit.(=)
    (fun size ->
      clear ();
      Caml.Format.pp_set_margin formatter size;
      Fmt.pf formatter "%a" fmt tm;
      Fmt.flush formatter ()
    )
  in

  let px_user_data = px_size_s
    |> React.S.map ~eq:String.(=) Float.to_string
    |> RHtml.a_user_data "px-size"
  in
  let char_user_data = char_size_s
    |> React.S.map ~eq:String.(=) Int.to_string
    |> RHtml.a_user_data "char-size"
  in
  let elt = Html.div ~a:[px_user_data; char_user_data] [elt] in

  elt, selection_s

let view_term ~highlight_s tm =
  let pp_view, tm_selection_s =
    pp_view ~highlight_s tm (Nominal.pp_term_ranges Primitive.pp)
  in
  success_msg [pp_view], tm_selection_s

let view_core ~highlight_s core =
  let pp_view, tm_selection_s = pp_view ~highlight_s core Core.pp in
  success_msg [pp_view], tm_selection_s

let view_parser ~highlight_s ~success parser =
  let elt, tm_selection_s = pp_view ~highlight_s parser P.pp_ranges in
  Html.(div ~a:[a_class [if success then "success" else "error"]] [elt]), tm_selection_s

let view_parser_ignore_selection ~highlight_s ~success parser =
  let elem, _ = view_parser parser ~highlight_s ~success in elem

let string_location ~str ~loc =
  if String.(str = "")
  then
    [%html{|<div class="font-mono mx-2 bg-gray-100 underline">(empty string)</div>|}]
  else
    let before = String.subo str ~len:loc in
    let after = String.subo str ~pos:loc in

    let before_elem = if String.(before = "") then Html.wbr () else Html.txt before in
    let after_elem = if String.(after = "") then Html.wbr () else Html.txt after in

    [%html{|
    <div class="flex flex-row font-mono mx-2 bg-gray-100 underline">
      <div class="inline-block">
        <span class="text-gray-500">|}[before_elem]{|</span>
      </div>
      <div style="width: 0" class="inline-block">
        <div
           style="width: 2px; margin-top: 0.125rem"
           class="h-5 relative bg-black"
        ></div>
      </div>
      <div class="inline-block">
        <span>|}[after_elem]{|</span>
      </div>
    </div>
    |}]

(* Produce a div saying "advances the input n chars to ...". Used in both
 *)
let snapshot_advanced_view str Model.TraceSnapshot.{ pre_pos; post_pos; _ } =
  let n = post_pos - pre_pos in
  let chars = match n with
    | 1 -> "1 character"
    | _ -> Printf.sprintf "%d characters" n
  in

  Html.(div
    [ span [txt ("advances the input " ^ chars ^ " to")]
    ; inline_block (string_location ~str ~loc:post_pos)
    ])

let snapshot_controls str snapshots (path_h : int RList.handle) =
  let header = [%html{|
    <tr>
      <td class="p-2 border-t-2 border-b-2 border-r-2 w-1/2">parser</td>
      <td class="p-2 border-t-2 border-b-2 border-r-2">action</td>
      <td class="p-2 border-t-2 border-b-2"></td>
    </tr>
  |}]
  in

  let body = snapshots
    |> List.mapi ~f:(fun i snapshot ->
      let Model.TraceSnapshot.{ success; parser; _ } = snapshot in
      let onclick _ = RList.snoc i path_h; false in
      let btn = button ~onclick "view" in

      let highlight_s = React.S.const SourceRanges.empty in
      Html.(tr
        [ td ~a:[a_class ["p-2 border-t-2 border-r-2"]]
          [view_parser_ignore_selection ~highlight_s ~success parser]
        ; td ~a:[a_class ["p-2 border-t-2 border-r-2"]]
          [snapshot_advanced_view str snapshot]
        ; td ~a:[a_class ["p-2 border-t-2"]] [btn]
        ]))
    |> RList.const
  in

  table ~classes:["table-fixed"] header body

let view_controls str path_h snapshots =
  let n_snaps = List.length snapshots in
  let msg = match n_snaps with
    | 0 -> "this parser calls no subparsers"
    | 1 -> "this parser calls 1 subparser"
    | _ -> Caml.Printf.sprintf "this parser calls %n subparsers" n_snaps
  in

  rows
    ~classes:[]
    (if List.length snapshots > 0
    then [ txt msg; snapshot_controls str snapshots path_h ]
    else [ txt msg ])

type path_traversal =
  { bottom_snapshot: Model.TraceSnapshot.t
  ; stack: Model.TraceSnapshot.t list
  }

(* Traverse the path, returning the snapshot at the bottom and all the snapshots along the
 * way. *)
let traverse_path ~root ~path =
  let current_snapshot = ref root in
  let stack = path
    |> List.map ~f:(fun i ->
      let snapshot = !current_snapshot in
      let Model.TraceSnapshot.{ snapshots; _ } = snapshot in
      current_snapshot := List.nth_exn snapshots i;
      snapshot
    )
  in
  { bottom_snapshot = !current_snapshot; stack }

let view_stack root path_h path_s = path_s
  |> RList.signal
  |> React.S.map ~eq:(List.equal html_eq) (fun path ->
    let stack_lst = (traverse_path ~root ~path).stack in
    let len = List.length stack_lst in
    let highlight_s = React.S.const SourceRanges.empty in
    stack_lst
    |> List.mapi ~f:(fun i snapshot ->
      let Model.TraceSnapshot.{ parser; success; _ } = snapshot in
      let p_view = view_parser_ignore_selection ~highlight_s ~success parser in
      let onclick _ = RList.set path_h (List.take path i); false in
      let btn = button ~onclick "return here" in
      let classes = List.filter_map ~f:Fn.id
        [ if i > 0 then Some "border-t-2" else None
        ; if i < len - 1 then Some "border-b-2" else None
        ]
      in
      Html.(tr
        ~a:[a_class classes]
        [ td ~a:[a_class ["p-2"; "w-1/4"]] [btn]
        ; td ~a:[a_class ["p-2"; "w-3/4"]] [p_view]
        ]))
  )
  |> RList.from_signal
  |> RHtml.table ~a:[Html.a_class ["w-full"; "table-fixed"]]

(* TODO: is there a cleaner way to do this? *)
let mk_div r_elem = r_elem |> RList.singleton_s |> RHtml.div

let view_root_snapshot str root =
  let path_s, path_h = RList.create [] in

  let current_snapshot_s = path_s
    |> RList.signal
    |> React.S.map ~eq:Model.TraceSnapshot.(=)
      (fun path -> (traverse_path ~root ~path).bottom_snapshot)
  in

  let stack_view = view_stack root path_h path_s in

  let stack_view = path_s
    |> RList.signal
    |> React.S.map ~eq:html_eq
      (fun path -> if List.length path > 0 then stack_view else txt "(empty)")
    |> RList.singleton_s
  in

  let controls_s = current_snapshot_s
    |> React.S.map ~eq:html_eq
      (fun Model.TraceSnapshot.{ snapshots; _ } -> view_controls str path_h snapshots)
  in

  let highlight_s = React.S.const SourceRanges.empty in
  let parser_view = current_snapshot_s
    |> React.S.map ~eq:html_eq
      (fun Model.TraceSnapshot.{ success; parser; _ } ->
        view_parser_ignore_selection ~highlight_s ~success parser)
  in

  let pre_loc_view = current_snapshot_s
    |> React.S.map ~eq:html_eq
      (fun Model.TraceSnapshot.{ pre_pos; _ } -> string_location ~str ~loc:pre_pos)
    |> RList.singleton_s
    |> r_inline_block
  in

  let status_view = current_snapshot_s
    |> React.S.map ~eq:html_eq
      (fun Model.TraceSnapshot.{ success; _ } -> Html.(span
        ~a:[a_class [if success then "success" else "error"]]
        [ if success then txt "succeeds" else txt "fails" ]))
    |> React.S.map ~eq:(List.equal html_eq) List.return
    |> RList.from_signal
    |> RHtml.span
  in

  let status_view = Html.(div
    [ inline_block (span [ txt "This parser" ])
    ; txt " "
    ; inline_block status_view
    ; txt " "
    ; inline_block (txt "and")
    ; txt " "
    ; current_snapshot_s
      |> React.S.map ~eq:html_eq (snapshot_advanced_view str)
      |> RList.singleton_s
      |> r_inline_block
    ; txt "."
    ])
  in

  let input_view = cols
    [ Html.(div [ span [ txt "The input to this parser is" ]])
    ; txt " "
    ; pre_loc_view
    ; txt "."
    ]
  in

  Html.(table ~a:[a_class ["w-full"]]
    [ tr [ td ~a:[a_class ["p-2 border-b-2"; "border-r-2"]] [txt "stack"]
         ; RHtml.td ~a:[Html.a_class ["p-2 border-b-2"]] stack_view
         ]
    ; tr [ td ~a:[a_class ["p-2 border-b-2"; "border-r-2"]] [txt "parser"]
         ; td ~a:[a_class ["p-2 border-b-2"]]
           [ parser_view |> RList.singleton_s |> RHtml.div ~a:[Html.a_class ["py-2"]]
           ; input_view
           ; status_view
           ]
         ]
    ; tr [ td ~a:[a_class ["p-2 border-r-2"]] [txt "subparsers"]
         ; td ~a:[a_class ["p-2"]] [mk_div controls_s]
         ]
    ])

module View = struct
  module Direct = P.Direct

  let view_parser_test
    ?term_ctx:(term_ctx=Lvca_util.String.Map.empty)
    ?parser_ctx:(parser_ctx=Lvca_util.String.Map.empty)
    ~highlight_s
    parser_or_err
    test_str = match parser_or_err with
      | Error msg ->
        let result = error_msg [txt msg] in
        let%html trace = "<div>not available: parser failed to parse</div>" in
        result, trace, React.S.const SourceRanges.empty
      | Ok parser ->
        let parser = P.map_loc ~f:(SourceRanges.of_opt_range ~buf:"parser") parser in
        let toplevel_result =
          Direct.parse_direct ~term_ctx ~parser_ctx parser test_str
        in
        let P.Direct.{ didnt_consume_msg; result; snapshot } = toplevel_result in

        let result, select_s = match result with
          | Error (msg, tm_opt) -> (match tm_opt with
            | None -> error_msg [txt msg], React.S.const SourceRanges.empty
            | Some tm ->
              let highlight_s = React.S.const SourceRanges.empty in
              let core, select_s = view_core ~highlight_s tm in
              error_msg [txt msg; core], select_s
          )
          | Ok tm -> match didnt_consume_msg with
            | Some msg -> error_msg [txt msg], React.S.const SourceRanges.empty
            | None -> view_term ~highlight_s tm
        in
        let trace = snapshot
          |> Model.TraceSnapshot.restrict_snapshot
          |> view_root_snapshot test_str
        in
        result, trace, select_s

  let mk_input_result
    ?parser_ctx:(parser_ctx=Lvca_util.String.Map.empty)
    ?parser_elem
    ~parser_str_s
    (test_s, update_test) =

    let show_trace_s, set_show_trace = React.S.create ~eq:Bool.(=) false in
    let trace_e, trace_button =
      toggle ~visible_text:"hide" ~hidden_text:"show" show_trace_s
    in
    let (_ : unit React.event) = trace_e |> React.E.map set_show_trace in

    let parser_s =
      let eq = Result.equal (P.equal OptRange.(=)) String.(=) in
      parser_str_s |> React.S.map ~eq parse_parser
    in

    let input_hl_s, set_input_hl =
      React.S.create ~eq:SourceRanges.(=) SourceRanges.empty
    in

    let test_s' =
      let eq (html11, html12, src1) (html21, html22, src2) =
        rhtml_eq html11 html21 && rhtml_eq html12 html22 &&
        React.S.equal ~eq:SourceRanges.(=) src1 src2
      in
      React.S.l2 ~eq
        (view_parser_test ~parser_ctx ~highlight_s:input_hl_s) parser_s test_s
    in

    let result = test_s'
      |> React.S.map ~eq:html_eq (fun (x, _, _) -> x)
      |> mk_div
    in
    let trace_s = test_s' |> React.S.map ~eq:html_eq (fun (_, x, _) -> x) in

    let tm_selection_s = test_s'
      |> React.S.map ~eq:(React.S.equal ~eq:SourceRanges.(=)) (fun (_, _, x) -> x)
      |> React.S.switch ~eq:SourceRanges.(=)
    in

    let input_hl_s = tm_selection_s
      |> React.S.map ~eq:OptRange.(=)
        (fun ranges -> match Map.find ranges "input" with
        | None -> None
        | Some ranges -> Range.list_range ranges
        )
      |> React.S.map ~eq:Ranges.(=) Ranges.of_opt_range
    in

    let parser_hl_s = tm_selection_s
      |> React.S.map ~eq:SourceRanges.(=) (SourceRanges.restrict ~buf:"parser")
    in

    let parser_elem = match parser_elem with
      | Some elem -> elem
      | None -> parser_s
        |> React.S.map ~eq:html_eq (function
          | Ok parser -> parser
            |> P.map_loc ~f:(SourceRanges.of_opt_range ~buf:"parser")
            |> view_parser ~highlight_s:parser_hl_s ~success:true
            |> fst
          | Error _ -> Html.(pre [code [RHtml.txt parser_str_s]])
        )
        |> mk_div
    in

    let test_input, test_evt = SingleLineInput.mk test_s ~highlights_s:input_hl_s in
    let (_ : unit React.event) = test_evt |> React.E.map
      (function
        | Common.InputUpdate str -> update_test str
        | InputSelect (start, finish)
        -> set_input_hl (SourceRanges.of_range ~buf:"input" Range.{ start; finish })
        | InputUnselect -> set_input_hl SourceRanges.empty)
    in

    let trace_cell = React.S.l2 ~eq:html_eq
      (fun trace show_trace -> if show_trace then trace else txt "")
      trace_s
      show_trace_s
      |> mk_div
    in

    [%html{|
      <div class="grid grid-cols-4">
        <table class="font-mono mb-6 col-span-4 table-fixed">
          <tr>
            <td class="border-2 p-2 w-1/6">Parser</td>
            <td class="border-2 p-2 w-5/6">|}[parser_elem]{|</td>
          </tr>
          <tr>
            <td class="border-2 p-2">Input</td>
            <td class="border-2 p-2">|}[test_input]{|</td>
          </tr>
          <tr>
            <td class="border-2 p-2">Result</td>
            <td class="border-2 p-2">|}[result]{|</td>
          </tr>
          <tr>
            <td class="border-2 p-2">Debugger |}[trace_button]{|</td>
            <td class="border-2 p-2">|}[trace_cell]{|</td>
          </tr>
        </table>
      </div>
      |}]

  let view model =
    let Model.
      { any_char_input
      ; char_input
      ; string_input
      ; satisfy1_input
      ; satisfy_is_alpha_input
      ; satisfy_is_digit_input
      ; star_input
      ; plus_input
      ; count_input = _
      ; choice1_input
      ; choice2_input
      ; choice3_input
      ; let_input
      ; fail_input
      ; sequence1_input
      ; sequence2_input
      ; fix_input
      ; playground_input
      } = model
    in

    let mk_input_result' ?parser_ctx str input =
      mk_input_result
        ?parser_ctx
        ~parser_str_s:(React.S.const str)
        input
    in

    let any_char_table = mk_input_result' Examples.any_char any_char_input in

    let char_table = mk_input_result' Examples.char char_input in
    let string_table = mk_input_result' Examples.string string_input in
    let satisfy1_table = mk_input_result' Examples.satisfy1 satisfy1_input in
    let satisfy_is_alpha_table =
      mk_input_result' Examples.satisfy_is_alpha satisfy_is_alpha_input
    in
    let satisfy_is_digit_table =
      mk_input_result' Examples.satisfy_is_digit satisfy_is_digit_input
    in
    let star_table = mk_input_result' Examples.many star_input in
    let plus_table = mk_input_result' Examples.plus plus_input in
    (* let count_table = mk_input_result' Examples.count count_input in *)

    let choice1_table = mk_input_result' Examples.two_choice choice1_input in
    let choice2_table = mk_input_result' Examples.multi_choice choice2_input in
    let choice3_table = mk_input_result' Examples.zero_choice choice3_input in

    let let_table = mk_input_result' Examples.let_ let_input in
    let fail_table = mk_input_result' Examples.fail fail_input in
    let sequence1_table = mk_input_result' Examples.sequence1 sequence1_input in
    let sequence2_table = mk_input_result' Examples.sequence2 sequence2_input in
    let fix_table = mk_input_result' ~parser_ctx:Prelude.ctx Examples.fix fix_input in

    let pg_parser_input, set_pg_parser_input =
      React.S.create ~eq:String.(=) Examples.fix
    in
    let pg_input_elem, pg_input_evt =
      MultilineInput.mk ~autofocus:false ~border:false pg_parser_input
    in
    let playground_table = mk_input_result
      ~parser_ctx:Prelude.ctx
      ~parser_elem:pg_input_elem
      ~parser_str_s:pg_parser_input
      playground_input
    in
    let (_ : unit React.event) = pg_input_evt |> React.E.map
      (fun evt -> match evt with
         | Common.InputUpdate str -> set_pg_parser_input str
         | _ -> ()
      )
    in

    [%html{|
      <div>
        <h3>Fixed character and string parsers</h3>

        <p>Let's start with the simplest class of parsers, which accept a single character
        or a fixed string.</p>

        <h4><code class="code-inline">.</code></h4>
        <p>The parser <code class="code-inline">.</code> accepts any single character.</p>
        |}[any_char_table]{|

        <h4><code class="code-inline">'c'</code></h4>
        <p>A single-quoted character accepts exactly that character. Note that this example, like many of the others is (intentionally) failing initially. Try changing the input so it's accepted.</p>
        |}[char_table]{|

        <h4><code class="code-inline">"str"</code></h4>
        <p>Similarly, a double-quoted string accepts exactly that string.</p>
        |}[string_table]{|

        <h4>Debugging</h4>
        <p>You've probably noticed the <em>debugger</em> rows below each parse result. By toggling this row you can see the steps the parser took to consume an input (or not). For the parsers we've seen so far, it's always exactly one step, but as soon as we get to <em>repetition</em> below, that will change. But this tool will really become useful when we get to <code class="code-inline">choice</code> and <code class="code-inline">fix</code>.</p>

        <h3>Satisfy</h3>

        <p>Fixed characters are awfully limiting. <code class="code-inline">satisfy</code> parses a single character that satisfies some predicate. The available predicates are <code class="code-inline">is_digit</code>, <code class="code-inline">is_lowercase</code>, <code class="code-inline">is_uppercase</code>, <code class="code-inline">is_alpha</code>, <code class="code-inline">is_alphanum</code>, and <code class="code-inline">is_whitespace</code>.</p>
        |}[satisfy1_table]{|
        |}[satisfy_is_alpha_table]{|
        |}[satisfy_is_digit_table]{|

        <p>For convenience, I'll leave the last two parsers in scope as <code class="code-inline">alpha</code> and <code class="code-inline">digit</code>, so we can use them later on.</p>

        <p>You might wonder, what's the syntax inside the <code class="code-inline">satisfy</code> expression? It's a language I'm calling <em>core</em>, which can be used for manipulating syntax trees. It's not what this post is about, but I'll have more to say about it in the future.</p>

        <h3>Repetition</h3>

        <p>The next class of operators accepts some number of repetitions of another
        parser.</p>

        <h4><code class="code-inline">*</code></h4>
        <p>The star operator can be used to accept any number of repetitions of
        the previous parser. For example <code class="code-inline">'c'*</code> accepts any number of <code class="code-inline">'c'</code>s, including 0.</p>
        |}[star_table]{|

        <h4><code class="code-inline">+</code></h4>
        <p>The plus operator can be used to accept one or more repetitions of
        the previous parser. For example <code class="code-inline">'c'*</code> accepts one or more <code class="code-inline">'c'</code>s.</p>
        |}[plus_table]{|

        <h3>Sequence</h3>

        <p>Concatenating a sequence of parsers accepts when they all parse successfully in sequence. A parser must return something, which goes to the right of the arrow. For example <code class="code-inline">|}[Html.txt Examples.sequence1]{|</code> parses a simple addition expression where the operands, <code>a</code> and <code>b</code>, are both one character (any character).</p>
        |}[sequence1_table]{|

        <p>Of course, it would be more useful to return something we parsed. That's why you can name the result of any parsers you'd like to use in the result.</p>
        |}[sequence2_table]{|

        <p>This is a good time to revisit the <em>Debugger</em> tool. If you look at the debugger for the sequence parser, you'll see that it calls five subparsers. You can click the <em>view</em> button to inspect the details of any subparser, then <em>return here</em> to return to a caller anywhere up the stack.</p>

        <h3>Choice</h3>

        <p>The <code class="code-inline">choice</code> construct can be used to accept one of several parsers. For
        example <code class="code-inline">choice ("c" | "foo")</code> accepts <code class="code-inline">"c"</code> or <code class="code-inline">"foo"</code>.</p>
        |}[choice1_table]{|

        <p><code class="code-inline">choice</code> can accept any number of choices, and you can start each line with <code class="code-inline">|</code>. Note that choice always chooses the first matching branch, so in this example, <code class="code-inline">"abcd"</code> will never match (<code class="code-inline">"abc"</code> will match, leaving <code class="code-inline">"d"</code> unconsumed).</p>

        |}[choice2_table]{|

        <p>An empty choice always fails.</p>

        |}[choice3_table]{|

        <h3>Language constructs</h3>

        <p>So far all of our parsers have looked a lot like regular expressions. Let's
        introduce a construct that will make this look more like a real language.
        Let-binding allows us to name parsers and use them later, for example
        <code class="code-inline">|}[Html.txt Examples.let_]{|</code>.</p>
        |}[let_table]{|

        <p>Parsers can also fail with a message, like <code class="code-inline">|}[Html.txt Examples.fail]{|</code>. This
        example as written is of course not very useful, but this can be quite
        useful as part of a larger parser.</p>
        |}[fail_table]{|

        <h3>Fix</h3>

        <p>Our parsers to this point have been limited: we can parse regular languages but not context-free languages. <code class="code-inline">fix</code> extends the language in the same way as <a class="prose-link" href="https://catonmat.net/recursive-regular-expressions">recursive regular expressions</a> to give it more power.
        </p>

        <p>Let's say you want to parse addition expressions, like "1 + 2", "1 + 2 + 3", "1 + 2 + 3 + 4", etc. We need a way to recursively use the parser we're defining. It's a little mind-bending, so let's look at an example.</p>

        <p>Note: For clarity I've pre-defined two parsers: <code class="code-inline">name = (chars=alpha+ -> {var(chars)})</code> and <code class="code-inline">literal = (chars=digit+ -> {literal(chars)})</code>.</p>

        |}[fix_table]{|

        <p><code class="code-inline">fix</code> computes the <a class="prose-link" href="https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node24.html#sec:proc-general-methods">fixed-point</a> of our parser. It takes a function which receives the parser being defined... and uses it to define itself.</p>

        <h3>Playground</h3>
        <p>Finally, here is a playground where you can write and test your own parsers.</p>

        |}[playground_table]{|

      </div>
    |}]
end

let stateless_view () = View.view Model.initial_model
;;
