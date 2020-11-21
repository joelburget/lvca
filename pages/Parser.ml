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

type trace_snapshot = P.Direct.trace_snapshot

let parse_parser str = ParseUtil.parse_string (ParseParser.t ParseCore.term) str

module Model = struct
  type input_sig = string React.signal * (string -> unit)

  type t =
    { any_char_input: input_sig
    ; char_input: input_sig
    ; string_input: input_sig
    ; star_input: input_sig
    ; plus_input: input_sig
    ; alt_input: input_sig
    ; count_input: input_sig
    ; let_input: input_sig
    ; fail_input: input_sig
    (* ; satisfy_input: input_sig *)
    (* TODO: rest *)
    }

  let mk str =
    let s, update = React.S.create str in
    s, (fun str -> update str)

  let initial_model =
    { any_char_input = mk "c"
    ; char_input = mk "c"
    ; string_input = mk "foo"
    ; star_input = mk "ccc"
    ; plus_input = mk "ccc"
    ; alt_input = mk "c"
    ; count_input = mk "cc"
    ; let_input = mk "foo"
    ; fail_input = mk "foo"
    (* ; satisfy_input = mk "bar" *)
    }
end

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

(* TODO: reactive version *)
let view_parser parser success =
  let selection_s = React.S.const None in
  let elt, formatter = RangeFormatter.mk selection_s in
  Fmt.pf formatter "%a" P.pp_plain parser;
  Fmt.flush formatter ();
  Html.(div ~a:[a_class [if success then "success" else "error"]] [elt])

let view_term tm =
  let str = Fmt.str "parsed: %a" (Nominal.pp_term_ranges Primitive.pp) tm in
  Html.div [txt str]

let path_controls _path_s _path_h = rows
  []

let snapshot_advanced_view str P.Direct.{ pre_pos; post_pos; _ } =
  let n = post_pos - pre_pos in
  let chars = match n with
    | 1 -> "1 character"
    | _ -> Printf.sprintf "%d characters" n
  in

  Html.(div
    [ inline_block (txt (" advances the input " ^ chars ^ " to "))
    ; inline_block (string_location ~str ~loc:post_pos)
    ])

let snapshot_controls str snapshots path_h =

  let header = [%html{|
    <tr>
      <td class="border-2">parser</td>
      <td class="border-2">action</td>
      <td class="border-2"></td>
    </tr>
  |}]
  in

  let body = snapshots
    |> List.mapi ~f:(fun i snapshot ->
      let P.Direct.{ success; parser; _ } = snapshot in
      let onclick _ = RList.snoc i path_h; false in
      let btn = button ~onclick "view" in

      Html.(tr
        [ td ~a:[a_class ["border-2"]] [view_parser parser success]
        ; td ~a:[a_class ["border-2"]] [snapshot_advanced_view str snapshot]
        ; td ~a:[a_class ["border-2"]] [btn]
        ]))
    |> RList.const
  in

  table header body

let view_controls str path_s path_h snapshots =
  let n_snaps = List.length snapshots in
  let msg = match n_snaps with
    | 0 -> "this parser calls no subparsers"
    | 1 -> "this parser calls 1 subparser"
    | _ -> Caml.Printf.sprintf "this parser calls %n subparsers" n_snaps
  in

  let always_visible_rows =
    [ path_controls path_s path_h
    ; subheader msg
    ]
  in

  rows (if List.length snapshots > 0
    then Lvca_util.List.snoc always_visible_rows (snapshot_controls str snapshots path_h)
    else always_visible_rows)

type path_traversal =
  { bottom_snapshot: trace_snapshot
  ; stack: trace_snapshot list
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

let view_stack = fun root path_h path_s ->

    path_s
      |> RList.signal
      |> React.S.map (fun path -> (traverse_path ~root ~path).stack
        |> List.mapi ~f:(fun n snapshot ->
          let P.Direct.{ parser; success; _ } = snapshot in
          let elem = view_parser parser success in
          let onclick _ = RList.set path_h (List.take path n); false in
          (* let button = button ~onclick "return here" in *)
          let btn = Html.(div
            ~a:[a_onclick onclick; a_class ["button"]]
            [elem])
          in
          Html.(tr [td [btn]]))
      )
      |> RList.from_signal
      |> RHtml.table

let view_root_snapshot str root =
  let path_s, path_h = RList.create [] in

  let current_snapshot_s = path_s
    |> RList.signal
    |> React.S.map (fun path -> (traverse_path ~root ~path).bottom_snapshot)
  in

  let stack_view = view_stack root path_h path_s in

  let stack_section = path_s
    |> RList.signal
    |> React.S.map (fun path ->
      [ subheader "stack"
      ; if List.length path > 0 then stack_view else txt "(empty)"
      ]
    )
    |> RList.from_signal
  in

  let controls_s = current_snapshot_s
    |> React.S.map (fun P.Direct.{ snapshots; _ } ->
        view_controls str path_s path_h snapshots)
  in

  let parser_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ success; parser; _ } -> view_parser parser success)
  in

  let pre_loc_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ pre_pos; _ } -> string_location ~str ~loc:pre_pos)
    |> RList.singleton_s
    |> RHtml.(div ~a:[a_class (React.S.const ["inline-block"])])
  in

  let advanced_n_s = current_snapshot_s |> React.S.map (snapshot_advanced_view str) in

  (* TODO: is there a cleaner way to do this? *)
  let mk_div r_elem = RHtml.div (RList.singleton_s r_elem) in

  let status_view = current_snapshot_s
    |> React.S.map (fun P.Direct.{ success; _ } -> Html.(span
        ~a:[a_class [if success then "success" else "error"]]
        [ if success
          then txt "succeeds"
          else txt "fails"
        ]))
    |> React.S.map List.return
    |> RList.from_signal
    |> RHtml.span
  in

  rows
    [ RHtml.div stack_section
    ; subheader "parser"
    ; mk_div parser_view
    ; cols [ Html.(div [ span [ txt "The input to this parser is " ]]); pre_loc_view]
    ; Html.(div
      [ inline_block (span [ txt "This parser " ])
      ; inline_block status_view
      ; inline_block (txt " and ")
      ; RHtml.div
        ~a:[a_class ["inline-block"]]
        (RList.singleton_s advanced_n_s) (* TODO: only if success *)
      ])
    ; mk_div controls_s
    ]

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
  let count = "'c'{{2}}"
  let alt = {|'c' | "foo"|}
  let let_ = {|let p1 = "str" in let p2 = 'c'* in p1 | p2|}
  let fail = {|fail {"failed (reason)"}|}
end

module View = struct
  module Direct = P.Direct

  let view_parser_test parser test =
      let parser = P.map_loc ~f:(SourceRanges.of_opt_range ~buf:"TODO") parser in
      let P.Direct.{ snapshot; result } = Direct.parse_direct parser test in
      let result = match result with
        | Error (msg, tm_opt) ->
          let tm_str = match tm_opt with
            | None -> "(no tm) " ^ msg
            | Some tm -> Printf.sprintf "%s: %s" msg (Fmt.str "%a" Core.pp tm )

          in
          error_msg tm_str
        | Ok tm -> view_term tm
      in
      let trace = rows ~border:true
        [ subheader "trace this parser's execution"
        ; view_root_snapshot test snapshot
        ]
      in
      result, trace

  let mk_input_result parser_str (test_s, update_test) =
    let test_input, test_evt = Common.mk_single_line_input test_s in
    let parser = Examples.parse_or_fail parser_str in

    let (_ : unit React.event) = test_evt |> React.E.map update_test in

    let result = test_s
      |> React.S.map (view_parser_test parser)
      |> React.S.Pair.fst
      |> RList.singleton_s
      |> RHtml.div
    in

    let tab = [%html{|
      <table class="font-mono">
        <tr><td class="border-2">Parser</td><td class="border-2">|}[txt parser_str]{|</td></tr>
        <tr>
          <td class="border-2">Input</td><td class="border-2">|}[test_input]{|</td></tr>
        <tr><td class="border-2">Result</td><td class="border-2">|}[result]{|</td></tr>
      </table>
      |}]
    in

    let inline_code = [%html{|
      <code class="font-mono bg-gray-100 p-1 border-2 border-gray-400">
        |}[txt parser_str]{|
      </code>
      |}]
    in

    inline_code, tab

  let view model =
    let any_char_p, any_char_table =
      mk_input_result Examples.any_char model.Model.any_char_input
    in
    let char_p, char_table =
      mk_input_result Examples.char model.Model.char_input
    in
    let string_p, string_table =
      mk_input_result Examples.string model.Model.string_input
    in
    let star_p, star_table =
      mk_input_result Examples.many model.Model.star_input
    in
    let plus_p, plus_table =
      mk_input_result Examples.plus model.Model.plus_input
    in
    let count_p, count_table =
      mk_input_result Examples.count model.Model.count_input
    in
    let alt_p, alt_table =
      mk_input_result Examples.alt model.Model.alt_input
    in
    let let_p, let_table =
      mk_input_result Examples.let_ model.Model.alt_input
    in
    let fail_p, fail_table =
      mk_input_result Examples.fail model.Model.alt_input
    in

    [%html{|
      <div>
        <h3>Fixed character and string parsers</h3>

        <p>Let's start with the simplest class of parsers, which accept a single character
        or a fixed string.</p>

        <p>The parser |}[any_char_p]{| accepts any single character.</p>
        |}[any_char_table]{|

        <p>We can also parse a single character with |}[char_p]{|. It accepts the
        single character <code>'c'</code>.</p>
        |}[char_table]{|

        <p>We can parse a fixed string with |}[string_p]{|. It accepts the
        string <code>"foo"</code>.</p>
        |}[string_table]{|

        <h3>Repetition</h3>

        <p>The next class of operators accepts some number of repetitions of another
        parser.</p>

        <p>The operator <code>*</code> can be used to accept any number of repetitions of
        the previous parser. For example |}[star_p]{|.</p>
        |}[star_table]{|

        <p>The operator <code>+</code> can be used to accept one or more repetitions of
        the previous parser. For example |}[plus_p]{|.</p>
        |}[plus_table]{|

        <p>We can also require exactly <code>n</code> repetitions of some parser. For
        example |}[count_p]{|.</p>
        |}[count_table]{|

        <h3>Alternation</h3>

        <p>The operator <code>|</code> can be used to accept either of two parsers. For
        example |}[alt_p]{|.</p>
        |}[alt_table]{|

        <h3>Language constructs</h3>

        <p>So far all of our parsers have looked a lot like regular expressions Let's
        introduce a construct that will make this look a lot more like a real language.
        For example |}[let_p]{|. In this case it would have been simpler to write this
        parser as <code>"str" | 'c'*</code>, but it's often useful to name helpers in
  larger parsers</p>
        |}[let_table]{|

        <p>Parsers can also fail with a message. This example as written is of course not
        very useful, but this can be quite useful as part of a larger parser. For example
      |}[fail_p]{|.</p>
        |}[fail_table]{|

        <h3>Sequence and Fix</h3>

        <p>There are two more very important combinators before we hit the misc
        section. TODO: revamp this completely: motivate it by parsing arithmetic where we
        need both sequence and fix</p>

        <p>First of all, we can't yet parse a sequence of sub-parsers. For example...
        TODO</p>

        <p></p>

        <h3>Misc (TODO)</h3>

        <ul>
          <li>satisfy</li>
          <li>option</li>
          <li>return</li>
        </ul>

        <h3>Notes (TODO)</h3>
        <ul>
          <li>Call out ability to write intuitive syntax, in particular the intuitive
          syntax for sequences and binding.</li>
          <li>Call out parser debugger, provenance</li>
          <li>Error messages are good because parser parser written this way</li>
        </ul>

      </div>
    |}]
end

let stateless_view =
  (* let model_s, signal_update = React.S.create Model.initial_model in *)
  View.view Model.initial_model
;;
