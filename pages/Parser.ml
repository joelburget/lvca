open Base
open Lvca_core
open Lvca_syntax
open ReactiveData

module P = Lvca_languages.Parser
module ParseCore = Core.Parse (ParseUtil.CComment)
module ParseParser = P.Parse (ParseUtil.CComment)
module Html = Js_of_ocaml_tyxml.Tyxml_js.Html
module RHtml = Js_of_ocaml_tyxml.Tyxml_js.R.Html

open Components

module Model = struct
  type input_sig = string React.signal * (string -> unit)

  type t =
    { any_char_input: input_sig
    ; char_input: input_sig
    ; string_input: input_sig
    ; satisfy1_input: input_sig
    ; satisfy_is_alpha_input: input_sig
    ; satisfy_is_digit_input: input_sig
    ; star_input: input_sig
    ; plus_input: input_sig
    ; alt_input: input_sig
    ; count_input: input_sig
    ; let_input: input_sig
    ; fail_input: input_sig
    ; sequence_input: input_sig
    ; fix_input: input_sig
    }

  let mk str =
    let s, update = React.S.create str in
    s, (fun str -> update str)

  let initial_model =
    { any_char_input = mk "c"
    ; char_input = mk "c"
    ; string_input = mk "foo"
    ; satisfy1_input = mk "c"
    ; satisfy_is_alpha_input = mk "c"
    ; satisfy_is_digit_input = mk "c"
    ; star_input = mk "ccc"
    ; plus_input = mk "ccc"
    ; alt_input = mk "c"
    ; count_input = mk "cc"
    ; let_input = mk "foo"
    ; fail_input = mk "foo"
    ; sequence_input = mk "1 + 2"
    ; fix_input = mk "x + 90 + y"
    }
end

module Examples = struct
  let parse_or_fail parser_str =
      match ParseUtil.parse_string (ParseParser.t ParseCore.term) parser_str with
      | Error msg -> failwith msg
      | Ok parser -> parser

  let any_char = "."
  let char = "'c'"
  let string = {|"foo"|}
  let many = "'c'*"
  let plus = "'c'+"
  (* let count = "'c'{{2}}" *)
  let choice = {|choice('c' | "foo")|}
  let let_ = {|let p1 = "str" in let p2 = 'c'* in choice (p1 | p2)|}
  let fail = {|fail {"some reason for failing"}|}
  let satisfy1 = {|satisfy (c -> match c with {
  | 'c' -> {true()}
  | _ -> {false()}
})|}
  let satisfy_is_alpha = "satisfy(c -> {is_alpha(c)})"
  let satisfy_is_digit = "satisfy(c -> {is_digit(c)})"
  let sequence = "a=. ' '* '+' ' '* b=. -> {plus(a; b)}"
  (* TODO: add var / literal types *)
  let fix = {|let atom = choice(name | literal) in
fix (expr -> choice (
  | atom=atom ' '* '+' ' '* expr=expr -> {plus(atom; expr)}
  | atom=atom -> {atom}
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

let pp_view tm fmt =
  let selection_s = React.S.const None in
  let elt, formatter = RangeFormatter.mk selection_s in
  Caml.Format.pp_set_margin formatter 40 (* TODO: base on current window width! *);
  Fmt.pf formatter "%a" fmt tm;
  Fmt.flush formatter ();
  elt

let view_term tm = success_msg [pp_view tm (Nominal.pp_term_ranges Primitive.pp)]

let view_core core = success_msg [pp_view core Core.pp]

(* TODO: reactive version *)
let view_parser parser success =
  let elt = pp_view parser P.pp_plain in
  Html.(div ~a:[a_class [if success then "success" else "error"]] [elt])

let string_location ~str ~loc =
  if String.(str = "")
  then
    [%html{|<div class="font-mono mx-2 .bg-gray-100 underline">(empty string)</div>|}]
  else
    let before = String.subo str ~len:loc in
    let after = String.subo str ~pos:loc in
    [%html{|
    <div class="flex flex-row font-mono mx-2 .bg-gray-100 underline">
      <div class="inline-block">
        <span class="text-gray-500">|}[Html.txt before]{|</span>
      </div>
      <div style="width: 0" class="inline-block">
        <div
           style="width: 2px; margin-top: 0.125rem"
           class="h-5 relative bg-black"
        ></div>
      </div>
      <div class="inline-block">
        <span>|}[Html.txt after]{|</span>
      </div>
    </div>
    |}]

(* Produce a div saying "advances the input n chars to ...". Used in both
 *)
let snapshot_advanced_view str P.Direct.{ pre_pos; post_pos; _ } =
  let n = post_pos - pre_pos in
  let chars = match n with
    | 1 -> "1 character"
    | _ -> Printf.sprintf "%d characters" n
  in

  Html.(div
    [ inline_block (txt ("advances the input " ^ chars ^ " to"))
    ; txt " "
    ; inline_block (string_location ~str ~loc:post_pos)
    ])

let snapshot_controls str snapshots path_h =
  let header = [%html{|
    <tr>
      <td class="border-t-2 border-b-2 border-r-2">parser</td>
      <td class="border-t-2 border-b-2 border-r-2">action</td>
      <td class="border-t-2 border-b-2"></td>
    </tr>
  |}]
  in

  let body = snapshots
    |> List.mapi ~f:(fun i snapshot ->
      let P.Direct.{ success; parser; _ } = snapshot in
      let onclick _ = RList.snoc i path_h; false in
      let btn = button ~onclick "view" in

      Html.(tr
        [ td ~a:[a_class ["border-t-2 border-r-2"]] [view_parser parser success]
        ; td ~a:[a_class ["border-t-2 border-r-2"]] [snapshot_advanced_view str snapshot]
        ; td ~a:[a_class ["border-t-2"]] [btn]
        ]))
    |> RList.const
  in

  table ~classes:["col-span-3"] header body

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
  { bottom_snapshot: P.Direct.trace_snapshot
  ; stack: P.Direct.trace_snapshot list
  }

(* Traverse the path, returning the snapshot at the bottom and all the snapshots along the
 * way. *)
let traverse_path ~root ~path =
  let current_snapshot = ref root in
  let stack = path
    |> List.map ~f:(fun i ->
      let snapshot = !current_snapshot in
      let P.Direct.{ snapshots; _ } = snapshot in
      current_snapshot := List.nth_exn snapshots i;
      snapshot
    )
  in
  { bottom_snapshot = !current_snapshot; stack }

let view_stack root path_h path_s = path_s
  |> RList.signal
  |> React.S.map (fun path ->
    let stack_lst = (traverse_path ~root ~path).stack in
    let len = List.length stack_lst in
    stack_lst
    |> List.mapi ~f:(fun i snapshot ->
      let P.Direct.{ parser; success; _ } = snapshot in
      let elem = view_parser parser success in
      let onclick _ = RList.set path_h (List.take path i); false in
      let btn = button ~onclick "return here" in
      let classes = List.filter_map ~f:Fn.id
        [ if i > 0 then Some "border-t-2" else None
        ; if i < len - 1 then Some "border-b-2" else None
        ]
      in
      Html.(tr ~a:[a_class classes] [td [btn]; td [elem]]))
  )
  |> RList.from_signal
  |> RHtml.table

(* TODO: is there a cleaner way to do this? *)
let mk_div r_elem = r_elem |> RList.singleton_s |> RHtml.div

let view_root_snapshot str root =
  let path_s, path_h = RList.create [] in

  let current_snapshot_s = path_s
    |> RList.signal
    |> React.S.map (fun path -> (traverse_path ~root ~path).bottom_snapshot)
  in

  let stack_view = view_stack root path_h path_s in

  let stack_view = path_s
    |> RList.signal
    |> React.S.map
      (fun path -> if List.length path > 0 then stack_view else txt "(empty)")
    |> RList.singleton_s
  in

  let controls_s = current_snapshot_s
    |> React.S.map (fun P.Direct.{ snapshots; _ } -> view_controls str path_h snapshots)
  in

  let parser_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ success; parser; _ } -> view_parser parser success)
  in

  let pre_loc_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ pre_pos; _ } -> string_location ~str ~loc:pre_pos)
    |> RList.singleton_s
    |> r_inline_block
  in

  let status_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ success; _ } -> Html.(span
        ~a:[a_class [if success then "success" else "error"]]
        [ if success then txt "succeeds" else txt "fails" ]))
    |> React.S.map List.return
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
      |> React.S.map (snapshot_advanced_view str)
      |> RList.singleton_s
      |> r_inline_block (* TODO: only if success *)
    ])
  in

  let input_view = cols
    [ Html.(div
    [ span [ txt "The input to this parser is" ]])
    ; txt " "
    ; pre_loc_view
    ]
  in

  Html.(table ~a:[a_class []]
    [ tr [ td ~a:[a_class ["border-b-2"; "border-r-2"]] [txt "stack"]
         ; RHtml.(td ~a:[a_class (React.S.const ["border-b-2"])] stack_view)
         ]
    ; tr [ td ~a:[a_class ["border-b-2"; "border-r-2"]] [txt "parser"]
         ; td ~a:[a_class ["border-b-2"]] [mk_div parser_view; input_view; status_view]
         ]
    ; tr [ td ~a:[a_class ["border-r-2"]] [txt "subparsers"]
         ; td ~a:[a_class []] [mk_div controls_s]
         ]
    ])

module View = struct
  module Direct = P.Direct

  let view_parser_test
    ?term_ctx:(term_ctx=Lvca_util.String.Map.empty)
    ?parser_ctx:(parser_ctx=Lvca_util.String.Map.empty)
    parser
    test_str =
      let parser = P.map_loc ~f:(SourceRanges.of_opt_range ~buf:"TODO") parser in
      let toplevel_result = Direct.parse_direct ~term_ctx ~parser_ctx parser test_str in
      let P.Direct.{ didnt_consume_msg; result; snapshot } = toplevel_result in

      let result_title, result = match result with
        | Error (msg, tm_opt) ->
          let msg = match tm_opt with
            | None -> error_msg [txt msg]
            | Some tm -> error_msg [txt msg; view_core tm]
          in
          "failed", msg
        | Ok tm -> match didnt_consume_msg with
          | Some msg -> "failed", error_msg [txt msg]
          | None -> "parsed", view_term tm
      in

      let result = Html.div
        [ inline_block (txt result_title)
        ; txt " "
        ; inline_block result
        ]
      in
      let trace = view_root_snapshot test_str snapshot in

      result, trace

  let mk_input_result
    ?parser_ctx:(parser_ctx=Lvca_util.String.Map.empty)
    parser_str
    (test_s, update_test) =
    let test_input, test_evt = Common.mk_single_line_input test_s in
    let (_ : unit React.event) = test_evt |> React.E.map update_test in

    let show_trace_s, set_show_trace = React.S.create false in
    let trace_e, trace_button =
      toggle ~visible_text:"hide" ~hidden_text:"show" show_trace_s
    in
    let (_ : unit React.event) = trace_e |> React.E.map set_show_trace in

    let parser = Examples.parse_or_fail parser_str in

    let test_s' = test_s
      |> React.S.map (view_parser_test ~parser_ctx parser)
    in
    let result = test_s' |> React.S.Pair.fst |> mk_div in
    let trace_s = test_s' |> React.S.Pair.snd in

    let trace_cell = React.S.l2
      (fun trace show_trace -> if show_trace then trace else txt "")
      trace_s
      show_trace_s
      |> mk_div
    in

    let tab = [%html{|
      <div class="grid grid-cols-4">
        <table class="font-mono mb-6 col-span-4 table-fixed">
          <tr>
            <td class="border-2 w-1/6">Parser</td>
            <td class="border-2 w-5/6"><pre><code>|}[txt parser_str]{|</code></pre></td>
          </tr>
          <tr>
            <td class="border-2">Input</td><td class="border-2">|}[test_input]{|</td></tr>
          <tr><td class="border-2">Result</td><td class="border-2">|}[result]{|</td></tr>
          <tr><td class="border-2">Trace |}[trace_button]{|</td><td class="border-2">|}[trace_cell]{|</td></tr>
        </table>
      </div>
      |}]
    in

    let inline_code = [%html{|
      <code class="font-mono bg-gray-100 p-1 border-2 border-gray-400">
        |}[txt parser_str]{|
      </code>
      |}]
    in

    inline_code, tab

  let view Model.{any_char_input; char_input; string_input; satisfy1_input; satisfy_is_alpha_input; satisfy_is_digit_input; star_input; plus_input; count_input = _; alt_input; let_input; fail_input; sequence_input; fix_input} =

    let any_char_p, any_char_table = mk_input_result Examples.any_char any_char_input in
    let char_p, char_table = mk_input_result Examples.char char_input in
    let string_p, string_table = mk_input_result Examples.string string_input in
    let satisfy1_p, satisfy1_table = mk_input_result Examples.satisfy1 satisfy1_input in
    let _satisfy_is_alpha_p, satisfy_is_alpha_table =
      mk_input_result Examples.satisfy_is_alpha satisfy_is_alpha_input
    in
    let _satisfy_is_digit_p, satisfy_is_digit_table =
      mk_input_result Examples.satisfy_is_digit satisfy_is_digit_input
    in
    let star_p, star_table = mk_input_result Examples.many star_input in
    let plus_p, plus_table = mk_input_result Examples.plus plus_input in
    (* let count_p, count_table = mk_input_result Examples.count count_input in *)
    let choice_p, choice_table = mk_input_result Examples.choice alt_input in
    let let_p, let_table = mk_input_result Examples.let_ let_input in
    let fail_p, fail_table = mk_input_result Examples.fail fail_input in
    let sequence_p, sequence_table = mk_input_result Examples.sequence sequence_input in
    let _fix_p, fix_table = mk_input_result ~parser_ctx:Prelude.ctx Examples.fix fix_input in

    [%html{|
      <div>
        <h3>Fixed character and string parsers</h3>

        <p>Let's start with the simplest class of parsers, which accept a single character
        or a fixed string.</p>

        <h4>|}[any_char_p]{|</h4>
        <p>The parser |}[any_char_p]{| accepts any single character.</p>
        |}[any_char_table]{|

        <h4>|}[char_p]{|</h4>
        <p>A single-quoted character accepts exactly that character</p>
        |}[char_table]{|

        <h4>|}[string_p]{|</h4>
        <p>Similarly, a double-quoted string accepts exactly that string.</p>
        |}[string_table]{|

        <h3>Satisfy</h3>

        <h4>|}[satisfy1_p]{|</h4>
        <p>Fixed characters are awfully limiting. <code>satisfy</code> parses a single character that satisfies some predicate. The available predicates are <code>is_digit</code>, <code>is_lowercase</code>, <code>is_uppercase</code>, <code>is_alpha</code>, <code>is_alphanum</code>, and <code>is_whitespace</code>.</p>
        |}[satisfy1_table]{|
        |}[satisfy_is_alpha_table]{|
        |}[satisfy_is_digit_table]{|

        <p>For convenience, I'll leave the last two parsers in scope as <code>alpha</code> and <code>digit</code>.</p>

        <p>TODO: note about core</p>

        <h3>Repetition</h3>

        <p>The next class of operators accepts some number of repetitions of another
        parser.</p>

        <h4><code>*</code></h4>
        <p>The star operator can be used to accept any number of repetitions of
        the previous parser. For example |}[star_p]{| accepts any number of <code>'c'</code>s, including 0.</p>
        |}[star_table]{|

        <h4><code>+</code></h4>
        <p>The plus operator can be used to accept one or more repetitions of
        the previous parser. For example |}[plus_p]{| accepts one or more <code>'c'</code>s.</p>
        |}[plus_table]{|

        <h3>Sequence</h3>

        <p>Concatenating a sequence of parsers accepts when they all parse successfully in sequence. You're allowed to name the result of any parsers you'd like to use in the result For example |}[sequence_p]{| parses a simple addition expression (where the operands can be anything, as long as it's one character (just wait, we'll make better parsers in a moment)).</p>
        |}[sequence_table]{|

        <h3>Choice</h3>

        <p>The <code>choice</code> construct can be used to accept either of two parsers. For
        example |}[choice_p]{| accepts <code>"c"</code> or <code>"foo"</code> .</p>
        |}[choice_table]{|

        <h3>Language constructs</h3>

        <p>So far all of our parsers have looked a lot like regular expressions. Let's
        introduce a construct that will make this look a lot more like a real language.
        Let-binding allows us to name parsers and use them later, for example
        |}[let_p]{|. In this case it would have been simpler to write this
        parser as <code>"str" | 'c'*</code>, but it's often useful to name
        helpers in larger parsers</p>
        |}[let_table]{|

        <p>Parsers can also fail with a message, like |}[fail_p]{|. This
        example as written is of course not very useful, but this can be quite
        useful as part of a larger parser.</p>
        |}[fail_table]{|

        <h3>Fix</h3>

        <p>Let's say you want to parse arithmetic expressions like "x + 1". TODO</p>

        TODO: mention name and literal parsers.

        <p>Finally, fix:</p>
        |}[fix_table]{|

      </div>
    |}]
end

let stateless_view = View.view Model.initial_model
;;
