open Base
open Brr
open Brr_note
open Lvca_core
open Lvca_syntax
open Note
open Prelude
module P = Lvca_languages.Parser
module ParseCore = Core.Parse (ParseUtil.CComment)
module ParseParser = P.Parse (ParseUtil.CComment)
module Tuple3 = Lvca_util.Tuple3
module Tuple2 = Lvca_util.Tuple2
open Components

let html_eq = Common.html_eq

let em, table, tr, td, span, div, h3, h4, p, pre, code =
  El.(em, table, tr, td, span, div, h3, h4, p, pre, code)
;;

module Model = struct
  type input_sig = string signal * (string -> unit)

  module TraceSnapshot = struct
    type t =
      { success : bool
      ; pre_pos : int
      ; post_pos : int
      ; parser : SourceRanges.t P.t
      ; snapshots : t list
      }

    let rec ( = ) t1 t2 =
      Bool.(t1.success = t2.success)
      && Int.(t1.pre_pos = t2.pre_pos)
      && Int.(t1.post_pos = t2.post_pos)
      && List.equal ( = ) t1.snapshots t2.snapshots
      && P.equal SourceRanges.( = ) t1.parser t2.parser
    ;;

    let rec restrict_snapshot : P.Direct.trace_snapshot -> t =
     fun { success; pre_pos; post_pos; parser; snapshots; _ } ->
      { success
      ; pre_pos
      ; post_pos
      ; parser
      ; snapshots = List.map snapshots ~f:restrict_snapshot
      }
   ;;
  end

  type t =
    { any_char_input : input_sig
    ; char_input : input_sig
    ; string_input : input_sig
    ; satisfy1_input : input_sig
    ; satisfy_is_alpha_input : input_sig
    ; satisfy_is_digit_input : input_sig
    ; star_input : input_sig
    ; plus_input : input_sig
    ; choice1_input : input_sig
    ; choice2_input : input_sig
    ; choice3_input : input_sig
    ; count_input : input_sig
    ; let_input : input_sig
    ; fail_input : input_sig
    ; sequence1_input : input_sig
    ; sequence2_input : input_sig
    ; fix_input : input_sig
    ; playground_input : input_sig
    }

  let mk str =
    let s, update = S.create ~eq:String.( = ) str in
    let handler str = update str in
    s, handler
  ;;

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
  ;;
end

module Action = struct
  type t =
    | ToggleTrace
    | SetSelection of SourceRanges.t
    | UpdateTest of string
    | SetInputHl of SourceRanges.t
end

module DebuggerAction = struct
  type t =
    | SubparserZoom of int (** Click on a subparser *)
    | ChopStack of int list (** Click on a stack member *)
end

let parse_parser = ParseUtil.parse_string (ParseParser.t ParseCore.term)

module Examples = struct
  let any_char = "."
  let char = "'c'"
  let string = {|"foo"|}
  let many = "'c'*"
  let plus = "'c'+"
  let count = "'c'2"
  let zero_choice = {|choice ()|}
  let two_choice = {|choice ('c' | "foo")|}

  let multi_choice = {|choice (
  | "abc"
  | "ab"
  | "a"
  | "abcd" // never matches
)|}

  let let_ = {|let p1 = "str" in let p2 = 'c'* in choice (p1 | p2)|}
  let fail = {|fail "some reason for failing"|}

  let satisfy1 =
    {|satisfy (c -> {match c with {
  | 'c' -> {true()}
  | _ -> {false()}
}})|}
  ;;

  let satisfy_is_alpha = "satisfy(c -> {is_alpha(c)})"
  let satisfy_is_digit = "satisfy(c -> {is_digit(c)})"
  let sequence1 = {|. ' '* '+' ' '* . -> {{"parsed an addition"}}|}
  let sequence2 = "a=. ' '* '+' ' '* b=. -> {{add(a; b)}}"

  let fix =
    {|let atom = choice (name | literal) in
fix (expr -> choice (
  | a=atom ' '* '+' ' '* e=expr -> {{add(a; e)}}
  | a=atom -> a
))|}
  ;;
end

module Prelude = struct
  let parse_parser_exn str =
    str |> ParseUtil.parse_string (ParseParser.t ParseCore.term) |> Result.ok_or_failwith
  ;;

  let alpha = parse_parser_exn Examples.satisfy_is_alpha
  let digit = parse_parser_exn Examples.satisfy_is_digit
  let name = parse_parser_exn {|chars=alpha+ -> {var (string_of_chars chars)}|}

  let literal =
    parse_parser_exn
      {|chars=digit+ -> {let str = string_of_chars chars in {literal(str)}}|}
  ;;

  let ctx : P.Direct.parser_ctx =
    Lvca_util.String.Map.of_alist_exn
      [ "alpha", alpha; "digit", digit; "name", name; "literal", literal ]
    |> Map.map ~f:(P.map_info ~f:(SourceRanges.of_opt_range ~buf:"prelude"))
  ;;
end

let pp_view ~highlight_s tm fmt =
  let clear, clear_formatter = E.create () in
  let RangeFormatter.{ elem; formatter; selection_e } =
    RangeFormatter.mk ~clear ~selection_s:highlight_s ()
  in
  (* TODO tie this to actual font size *)
  let font_size = 14. in
  let font_adjust = font_size *. 0.6 (* 0.6 is an estimate of font width to height *) in
  let initial_width = 40. *. font_adjust in
  let px_size_s, _set_px_size = S.create ~eq:Float.( = ) initial_width in
  (* let open Js_of_ocaml in *)
  (* TODO
  if (ResizeObserver.is_supported ()) then (
    let open Tyxml_js in
    let target = To_dom.of_code elt in
    let node = (target :> Dom.node Js.t) in

    let f entries _observer =
      match Js.array_get entries 0 |> Js.Optdef.to_option with
        | None -> ()
        | Some entry0 ->
          match entry0##.contentRect##.width |> Js.Optdef.to_option with
            | None -> ()
            | Some width -> set_px_size width
    in

    let _ : ResizeObserver.resizeObserver Js.t = ResizeObserver.observe ~node ~f () in
    ()
  ) else ();
  *)
  let char_size_s =
    px_size_s |> S.map ~eq:Int.( = ) (fun size -> Int.of_float (size /. font_adjust))
  in
  let _sink : Logr.t =
    S.log char_size_s (fun size ->
        clear_formatter ();
        Caml.Format.pp_set_margin formatter size;
        Fmt.pf formatter "%a" fmt tm;
        Fmt.flush formatter ())
  in
  (* TODO
  let px_user_data = px_size_s
    |> S.map ~eq:String.(=) Float.to_string
    |> RHtml.a_user_data "px-size"
  in
  let char_user_data = char_size_s
    |> S.map ~eq:String.(=) Int.to_string
    |> RHtml.a_user_data "char-size"
  in
  *)
  let elem = div ~at:[ class' "p-2" ] [ elem ] in
  elem, selection_e
;;

let view_term ~highlight_s tm =
  let pp_view, tm_selection_e =
    pp_view ~highlight_s tm (Nominal.pp_term_ranges Primitive.pp)
  in
  let tree_view, tree_selection_e = TreeView.view_tm tm in
  let view =
    div
      [ div ~at:[ class' "my-2" ] [ success_msg [ pp_view ] ]
      ; div ~at:[ class' "my-2" ] [ span [ txt "Tree viewer:" ] ]
      ; div ~at:[ class' "my-2" ] [ tree_view ]
      ]
  in
  let e = E.select [ tm_selection_e; tree_selection_e ] in
  view, e
;;

let view_core ~highlight_s core =
  let pp_view, tm_selection_e = pp_view ~highlight_s core Core.pp in
  success_msg [ pp_view ], tm_selection_e
;;

let view_parser ~highlight_s ~success parser =
  let elt, tm_selection_e = pp_view ~highlight_s parser P.pp_ranges in
  El.div ~at:[ class' (if success then "success" else "error") ] [ elt ], tm_selection_e
;;

let view_parser_ignore_selection ~highlight_s ~success parser =
  let elem, _ = view_parser parser ~highlight_s ~success in
  elem
;;

let string_location ~str ~loc =
  if String.(str = "")
  then
    El.div ~at:(classes "font-mono mx-2 bg-gray-100 underline") [ txt "(empty string)" ]
  else (
    let before = String.subo str ~len:loc in
    let after = String.subo str ~pos:loc in
    let before_elem = if String.(before = "") then El.wbr () else txt before in
    let after_elem = if String.(after = "") then El.wbr () else txt after in
    div
      ~at:(classes "flex flex-row font-mono mx-2 bg-gray-100 underline")
      [ div
          ~at:[ class' "inline-block" ]
          [ span ~at:[ class' "text-gray-500" ] [ before_elem ] ]
      ; div
          ~at:(classes "inline-block w-0")
          [ div ~at:(classes "h-5 relative bg-black") []
            (* TODO style="width: 2px; margin-top: 0.125rem" *)
          ]
      ; div ~at:[ class' "inline-block" ] [ span [ after_elem ] ]
      ])
;;

(* Produce a div saying "advances the input n chars to ...". Used in both... TODO
 *)
let snapshot_advanced_view str Model.TraceSnapshot.{ pre_pos; post_pos; _ } =
  let n = post_pos - pre_pos in
  let chars = match n with 1 -> "1 character" | _ -> Printf.sprintf "%d characters" n in
  div
    [ span [ txt ("advances the input " ^ chars ^ " to") ]
    ; inline_block (string_location ~str ~loc:post_pos)
    ]
;;

let snapshot_controls str snapshots =
  let header =
    tr
      [ td ~at:(classes "p-2 border-t-2 border-b-2 border-r-2 w-1/2") [ txt "parser" ]
      ; td ~at:(classes "p-2 border-t-2 border-b-2 border-r-2") [ txt "action" ]
      ; td ~at:(classes "p-2 border-t-2 border-b-2") []
      ]
  in
  let path_evts, body =
    snapshots
    |> List.mapi ~f:(fun i snapshot ->
           let Model.TraceSnapshot.{ success; parser; _ } = snapshot in
           let click_evt, btn = button "view" in
           let click_evt = click_evt |> E.map (fun _ -> DebuggerAction.SubparserZoom i) in
           let highlight_s = S.const SourceRanges.empty in
           ( click_evt
           , tr
               [ td
                   ~at:(classes "p-2 border-t-2 border-r-2")
                   [ view_parser_ignore_selection ~highlight_s ~success parser ]
               ; td
                   ~at:(classes "p-2 border-t-2 border-r-2")
                   [ snapshot_advanced_view str snapshot ]
               ; td ~at:(classes "p-2 border-t-2") [ btn ]
               ] ))
    |> List.unzip
  in
  let path_evt = E.select path_evts in
  path_evt, Components.table ~classes:[ "table-fixed" ] header (S.const body)
;;

let view_controls str snapshots =
  let n_snaps = List.length snapshots in
  let msg =
    match n_snaps with
    | 0 -> "this parser calls no subparsers"
    | 1 -> "this parser calls 1 subparser"
    | _ -> Printf.sprintf "this parser calls %n subparsers" n_snaps
  in
  let path_evt, snapshot_controls = snapshot_controls str snapshots in
  ( path_evt
  , rows
      ~classes:[]
      (if List.length snapshots > 0 then [ txt msg; snapshot_controls ] else [ txt msg ])
  )
;;

type path_traversal =
  { bottom_snapshot : Model.TraceSnapshot.t
  ; stack : Model.TraceSnapshot.t list
  }

(* Traverse the path, returning the snapshot at the bottom and all the snapshots along the
 * way. *)
let traverse_path ~root ~path =
  let current_snapshot = ref root in
  let stack =
    path
    |> List.map ~f:(fun i ->
           let snapshot = !current_snapshot in
           let Model.TraceSnapshot.{ snapshots; _ } = snapshot in
           current_snapshot := List.nth_exn snapshots i;
           snapshot)
  in
  { bottom_snapshot = !current_snapshot; stack }
;;

let view_stack root path_s =
  let eq = Tuple2.equal (List.equal phys_equal) Common.htmls_eq in
  let s =
    path_s
    |> S.map ~eq (fun path ->
           let stack_lst = (traverse_path ~root ~path).stack in
           let len = List.length stack_lst in
           let highlight_s = S.const SourceRanges.empty in
           stack_lst
           |> List.mapi ~f:(fun i snapshot ->
                  let Model.TraceSnapshot.{ parser; success; _ } = snapshot in
                  let p_view =
                    view_parser_ignore_selection ~highlight_s ~success parser
                  in
                  let click_evt, btn = button "return here" in
                  let click_evt =
                    click_evt
                    |> E.map (fun _ -> DebuggerAction.ChopStack (List.take path i))
                  in
                  let at =
                    [ (if i > 0 then Some "border-t-2" else None)
                    ; (if i < len - 1 then Some "border-b-2" else None)
                    ]
                    |> List.filter_map ~f:Fn.id
                    |> List.map ~f:class'
                  in
                  ( click_evt
                  , tr
                      ~at
                      [ td ~at:(classes "p-2 w-1/4") [ btn ]
                      ; td ~at:(classes "p-2 w-3/4") [ p_view ]
                      ] ))
           |> List.unzip)
  in
  let click_evt = s |> S.Pair.fst |> S.map E.select |> E.swap in
  let children = S.Pair.snd s in
  let table = table ~at:(classes "w-full table-fixed") [] in
  let () = Elr.def_children table children in
  click_evt, table
;;

let view_root_snapshot str root =
  let path_s, set_path = S.create [] in
  let current_snapshot_s =
    path_s
    |> S.map ~eq:Model.TraceSnapshot.( = ) (fun path ->
           (traverse_path ~root ~path).bottom_snapshot)
  in
  let stack_evt, stack_view = view_stack root path_s in
  let stack_view =
    path_s
    |> S.map ~eq:html_eq (fun path ->
           if List.length path > 0 then stack_view else txt "(empty)")
  in
  let s =
    current_snapshot_s
    |> S.map
         ~eq:(Tuple2.equal phys_equal html_eq)
         (fun Model.TraceSnapshot.{ snapshots; _ } -> view_controls str snapshots)
  in
  let controls_evt = s |> S.Pair.fst |> E.swap in
  let controls_s = S.Pair.snd s in
  let evt = E.select [ stack_evt; controls_evt ] in
  let _sink : Logr.t option =
    E.log evt (fun evt ->
        let path =
          match evt with
          | ChopStack path -> path
          | SubparserZoom i -> Lvca_util.List.snoc (S.value path_s) i
        in
        set_path path)
  in
  let highlight_s = S.const SourceRanges.empty in
  let parser_view =
    current_snapshot_s
    |> S.map ~eq:html_eq (fun Model.TraceSnapshot.{ success; parser; _ } ->
           view_parser_ignore_selection ~highlight_s ~success parser)
  in
  let pre_loc_view =
    current_snapshot_s
    |> S.map ~eq:html_eq (fun Model.TraceSnapshot.{ pre_pos; _ } ->
           string_location ~str ~loc:pre_pos)
    |> r_inline_block
  in
  let status_view =
    current_snapshot_s
    |> S.map ~eq:html_eq (fun Model.TraceSnapshot.{ success; _ } ->
           span
             ~at:[ class' (if success then "success" else "error") ]
             [ (if success then txt "succeeds" else txt "fails") ])
    |> S.map ~eq:(List.equal html_eq) List.return
    |> mk_reactive span
  in
  let status_view =
    El.div
      [ inline_block (span [ txt "This parser" ])
      ; txt " "
      ; inline_block status_view
      ; txt " "
      ; inline_block (txt "and")
      ; txt " "
      ; current_snapshot_s
        |> S.map ~eq:html_eq (snapshot_advanced_view str)
        |> r_inline_block
      ; txt "."
      ]
  in
  let input_view =
    cols
      [ div [ span [ txt "The input to this parser is" ] ]
      ; txt " "
      ; pre_loc_view
      ; txt "."
      ]
  in
  table
    ~at:[ class' "w-full" ]
    [ tr
        [ td ~at:(classes "p-2 border-b-2 border-r-2") [ txt "stack" ]
        ; mk_reactive' td ~at:(classes "p-2 border-b-2") stack_view
        ]
    ; tr
        [ td ~at:(classes "p-2 border-b-2 border-r-2") [ txt "parser" ]
        ; td
            ~at:(classes "p-2 border-b-2")
            [ parser_view |> mk_reactive' div ~at:[ class' "py-2" ]
            ; input_view
            ; status_view
            ]
        ]
    ; tr
        [ td ~at:(classes "p-2 border-r-2") [ txt "subparsers" ]
        ; td ~at:[ class' "p-2" ] [ mk_reactive' div controls_s ]
        ]
    ]
;;

module View = struct
  module Direct = P.Direct

  let view_parser_test
      ?(term_ctx = Lvca_util.String.Map.empty)
      ?(parser_ctx = Lvca_util.String.Map.empty)
      ~highlight_s
      parser_or_err
      test_str
    =
    let mk_err msg = error_msg [ El.span ~at:[ class' "p-2" ] [ txt msg ] ] in
    match parser_or_err with
    | Error msg ->
      let result = mk_err msg in
      let trace = El.div [ txt "not available: parser failed to parse" ] in
      result, trace, E.never
    | Ok parser ->
      let parser = P.map_info ~f:(SourceRanges.of_opt_range ~buf:"parser") parser in
      let toplevel_result = Direct.parse_direct ~term_ctx ~parser_ctx parser test_str in
      let P.Direct.{ didnt_consume_msg; result; snapshot } = toplevel_result in
      let result, select_e =
        match result with
        | Error (msg, tm_opt) ->
          (match tm_opt with
          | None -> mk_err msg, E.never
          | Some tm ->
            let highlight_s = S.const SourceRanges.empty in
            let core, select_e = view_core ~highlight_s tm in
            error_msg [ El.span ~at:[ class' "p-2" ] [ txt msg ]; core ], select_e)
        | Ok tm ->
          (match didnt_consume_msg with
          | Some msg -> mk_err msg, E.never
          | None -> view_term ~highlight_s tm)
      in
      let trace =
        snapshot |> Model.TraceSnapshot.restrict_snapshot |> view_root_snapshot test_str
      in
      result, trace, select_e
  ;;

  let mk_input_result
      ?(parser_ctx = Lvca_util.String.Map.empty)
      ?parser_elem
      ~parser_str_s
      (test_s, update_test)
    =
    let show_trace_s, set_show_trace = S.create ~eq:Bool.( = ) false in
    let trace_e, trace_button =
      button_toggle ~visible_text:"hide" ~hidden_text:"show" show_trace_s
    in
    let _sink : Logr.t option =
      E.log trace_e (fun _ -> show_trace_s |> S.value |> not |> set_show_trace)
    in
    let parser_s =
      let eq = Result.equal (P.equal OptRange.( = )) String.( = ) in
      parser_str_s |> S.map ~eq parse_parser
    in
    let input_hl_s, set_input_hl = S.create ~eq:SourceRanges.( = ) SourceRanges.empty in
    let test_s' =
      let eq = Tuple3.equal html_eq html_eq phys_equal in
      S.l2 ~eq (view_parser_test ~parser_ctx ~highlight_s:input_hl_s) parser_s test_s
    in
    let result = test_s' |> S.map ~eq:html_eq Tuple3.get1 |> mk_reactive' div in
    let trace_s = test_s' |> S.map ~eq:html_eq Tuple3.get2 in
    let tm_selection_s, set_selection =
      S.create ~eq:SourceRanges.( = ) SourceRanges.empty
    in
    let _sink : Logr.t =
      S.log test_s' (fun (_, _, e) ->
          let _sink : Logr.t option = E.log e set_selection in
          ())
    in
    let input_hl_s =
      tm_selection_s
      |> S.map ~eq:OptRange.( = ) (fun ranges ->
             Option.(Map.find ranges "input" >>= Range.list_range))
      |> S.map ~eq:Ranges.( = ) Ranges.of_opt_range
    in
    let parser_hl_s =
      tm_selection_s |> S.map ~eq:SourceRanges.( = ) (SourceRanges.restrict ~buf:"parser")
    in
    let parser_elem =
      match parser_elem with
      | Some elem -> elem
      | None ->
        parser_s
        |> S.map ~eq:html_eq (function
               | Ok parser ->
                 parser
                 |> P.map_info ~f:(SourceRanges.of_opt_range ~buf:"parser")
                 |> view_parser ~highlight_s:parser_hl_s ~success:true
                 |> fst
               | Error _ -> pre [ mk_reactive' code (parser_str_s |> S.map txt) ])
        |> mk_reactive' div
    in
    let test_input, test_evt = SingleLineInput.mk test_s ~highlights_s:input_hl_s in
    let _sink : Logr.t option =
      E.log test_evt (function
          | Common.InputUpdate str -> update_test str
          | InputSelect rng -> set_input_hl (SourceRanges.of_range ~buf:"input" rng)
          | InputUnselect -> set_input_hl SourceRanges.empty)
    in
    let trace_cell =
      S.l2
        ~eq:html_eq
        (fun trace show_trace -> if show_trace then trace else txt "")
        trace_s
        show_trace_s
      |> mk_reactive' div
    in
    let mk_row left right =
      tr [ td ~at:(classes "border-2 p-2") left; td ~at:(classes "border-2 p-2") right ]
    in
    div
      ~at:(classes "grid grid-cols-4")
      [ table
          ~at:(classes "font-mono mb-6 col-span-4 table-fixed")
          [ tr
              [ td ~at:(classes "border-2 p-2 w-1/6") [ txt "Parser" ]
              ; td ~at:(classes "border-2 p-2 w-5/6") [ parser_elem ]
              ]
          ; mk_row [ txt "Input" ] [ test_input ]
          ; mk_row [ txt "Result" ] [ result ]
          ; mk_row [ txt "Debugger"; trace_button ] [ trace_cell ]
          ]
      ]
  ;;

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
          ; count_input
          ; choice1_input
          ; choice2_input
          ; choice3_input
          ; let_input
          ; fail_input
          ; sequence1_input
          ; sequence2_input
          ; fix_input
          ; playground_input
          }
      =
      model
    in
    let mk_input_result' ?parser_ctx str input =
      mk_input_result ?parser_ctx ~parser_str_s:(S.const str) input
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
    let count_table = mk_input_result' Examples.count count_input in
    let choice1_table = mk_input_result' Examples.two_choice choice1_input in
    let choice2_table = mk_input_result' Examples.multi_choice choice2_input in
    let choice3_table = mk_input_result' Examples.zero_choice choice3_input in
    let let_table = mk_input_result' Examples.let_ let_input in
    let fail_table = mk_input_result' Examples.fail fail_input in
    let sequence1_table = mk_input_result' Examples.sequence1 sequence1_input in
    let sequence2_table = mk_input_result' Examples.sequence2 sequence2_input in
    let fix_table = mk_input_result' ~parser_ctx:Prelude.ctx Examples.fix fix_input in
    let pg_parser_input, set_pg_parser_input = S.create ~eq:String.( = ) Examples.fix in
    let pg_input_elem, pg_input_evt =
      MultilineInput.mk ~autofocus:false ~border:false pg_parser_input
    in
    let playground_table =
      mk_input_result
        ~parser_ctx:Prelude.ctx
        ~parser_elem:pg_input_elem
        ~parser_str_s:pg_parser_input
        playground_input
    in
    let _sink : Logr.t option =
      E.log pg_input_evt (fun evt ->
          match evt with Common.InputUpdate str -> set_pg_parser_input str | _ -> ())
    in
    let code_inline content = code ~at:[ class' "code-inline" ] content in
    let code_inline' str = code_inline [ txt str ] in
    div
      [ h3 [ txt "Fixed character and string parsers" ]
      ; p
          [ txt
              "Let's start with the simplest class of parsers, which accept a single \
               character or a fixed string."
          ]
      ; h4 [ code_inline' "." ]
      ; p
          [ txt "The parser "
          ; code ~at:[ class' "code-inline" ] [ txt "." ]
          ; txt " accepts any single character."
          ]
      ; any_char_table
      ; h4 [ code_inline' "'c'" ]
      ; p
          [ txt
              "A single-quoted character accepts exactly that character. Note that this \
               example, like many of the others is (intentionally) failing initially. \
               Try changing the input so it's accepted."
          ]
      ; char_table
      ; h4 [ code_inline' {|"str"|} ]
      ; p [ txt "Similarly, a double-quoted string accepts exactly that string." ]
      ; string_table
      ; h4 [ txt "Debugging" ]
      ; p
          [ txt "You've probably noticed the "
          ; em [ txt "debugger" ]
          ; txt
              " rows below each parse result. By toggling this row you can see the steps \
               the parser took to consume an input (or not). For the parsers we've seen \
               so far, it's always exactly one step, but as soon as we get to "
          ; em [ txt "repetition" ]
          ; txt
              " below, that will change. But this tool will really become useful when we \
               get to"
          ; code_inline' "choice"
          ; txt " and "
          ; code_inline' "fix"
          ; txt "."
          ]
      ; h3 [ txt "satisfy" ]
      ; p
          [ txt "Fixed characters are awfully limiting. "
          ; code_inline' "satisfy"
          ; txt
              " parses a single character that satisfies some predicate. The available \
               predicates are "
          ; code_inline' "is_digit"
          ; txt ", "
          ; code_inline' "is_lowercase"
          ; txt ", "
          ; code_inline' "is_uppercase"
          ; txt ", "
          ; code_inline' "is_alpha"
          ; txt ", "
          ; code_inline' "is_alphanum"
          ; txt ", and "
          ; code_inline' "is_whitespace"
          ; txt "."
          ]
      ; satisfy1_table
      ; satisfy_is_alpha_table
      ; satisfy_is_digit_table
      ; p
          [ txt "For convenience, I'll leave the last two parsers in scope as "
          ; code_inline' "alpha"
          ; txt " and "
          ; code_inline' "digit"
          ; txt ", so we can use them later on."
          ]
      ; p
          [ txt "You might wonder, what's the syntax inside the "
          ; code_inline' "satisfy"
          ; txt " expression? It's a language I'm calling "
          ; em [ txt "core" ]
          ; txt
              ", which can be used for manipulating syntax trees. It's not what this \
               post is about, but I'll have more to say about it in the future."
          ]
      ; h3 [ txt "Repetition" ]
      ; p
          [ txt
              "The next class of operators accepts some number of repetitions of another \
               parser."
          ]
      ; h4 [ code_inline' "*" ]
      ; p
          [ txt
              "The star operator can be used to accept any number of repetitions of the \
               previous parser. For example "
          ; code_inline' "'c'*"
          ; txt "accepts any number of "
          ; code_inline' "'c'"
          ; txt "s, including 0."
          ]
      ; star_table
      ; h4 [ code_inline' "+" ]
      ; p
          [ txt
              "The plus operator can be used to accept one or more repetitions of the \
               previous parser. For example "
          ; code_inline' "'c'*"
          ; txt " accepts one or more "
          ; code_inline' "'c'"
          ; txt "s."
          ]
      ; plus_table
      ; h4 [ txt "count" ]
      ; p
          [ txt
              "A parser followed by a number accepts a fixed number of repetitions of \
               that parser."
          ]
      ; count_table
      ; h3 [ txt "Sequence" ]
      ; p
          [ txt
              "Concatenating a sequence of parsers accepts when they all parse \
               successfully in sequence. A parser must return something, which goes to \
               the right of the arrow. For example "
          ; code_inline' Examples.sequence1
          ; txt " parses a simple addition expression where the operands, "
          ; code_inline' "a"
          ; txt " and "
          ; code_inline' "b"
          ; txt ", are both one character (any character)."
          ]
      ; sequence1_table
      ; p
          [ txt
              "Of course, it would be more useful to return something we parsed. That's \
               why you can name the result of any parsers you'd like to use in the \
               result."
          ]
      ; sequence2_table
      ; p
          [ txt "This is a good time to revisit the "
          ; em [ txt "Debugger" ]
          ; txt
              " tool. If you look at the debugger for the sequence parser, you'll see \
               that it calls five subparsers. You can click the "
          ; em [ txt "view" ]
          ; txt " button to inspect the details of any subparser, then "
          ; em [ txt "return here" ]
          ; txt " to return to a caller anywhere up the stack."
          ]
      ; h3 [ txt "Choice" ]
      ; p
          [ txt "The "
          ; code_inline' "choice"
          ; txt " construct can be used to accept one of several parsers. For example "
          ; code_inline' {|choice ("c" | "foo")|}
          ; txt " accepts "
          ; code_inline' {|"c"|}
          ; txt " or "
          ; code_inline' {|"foo"|}
          ; txt "."
          ]
      ; choice1_table
      ; p
          [ code_inline' "choice"
          ; txt " can accept any number of choices, and you can start each line with "
          ; code_inline' "|"
          ; txt
              ". Note that choice always chooses the first matching branch, so in this \
               example, "
          ; code_inline' {|"abcd"|}
          ; txt " will never match ("
          ; code_inline' "abc"
          ; txt " will match, leaving "
          ; code_inline' "d"
          ; txt " unconsumed)."
          ]
      ; choice2_table
      ; p [ txt "An empty choice always fails." ]
      ; choice3_table
      ; h3 [ txt "Language constructs" ]
      ; p
          [ txt
              "So far all of our parsers have looked a lot like regular expressions. \
               Let's introduce a construct that will make this look more like a real \
               language. Let-binding allows us to name parsers and use them later, for \
               example "
          ; code_inline' Examples.let_
          ; txt "."
          ]
      ; let_table
      ; p
          [ txt "Parsers can also fail with a message, like "
          ; code_inline' Examples.fail
          ; txt
              ". This example as written is of course not very useful, but this can be \
               quite useful as part of a larger parser."
          ]
      ; fail_table
      ; h3 [ txt "Fix" ]
      ; p
          [ txt
              "Our parsers to this point have been limited: we can parse regular \
               languages but not context-free languages. "
          ; code_inline' "fix"
          ; txt " extends the language in the same way as "
          ; El.a
              ~at:
                [ class' "prose-link"
                ; At.href (Jstr.v "https://catonmat.net/recursive-regular-expressions")
                ]
              [ txt " recursive regular expressions" ]
          ; txt " to give it more power."
          ]
      ; p
          [ txt
              {|Let's say you want to parse addition expressions like "1 + 2", "1 + 2 + 3", "1 + 2 + 3 + 4", etc. We need a way to recursively use the parser we're defining. It's a little mind-bending, so let's look at an example.|}
          ]
      ; p
          [ txt "Note: For clarity I've pre-defined two parsers: "
          ; code_inline' "name = (chars=alpha+ -> {var(string_of_chars chars)})"
          ; txt " and "
          ; code_inline' "literal = (chars=digit+ -> {literal(string_of_chars chars)})"
          ; txt "."
          ]
      ; fix_table
      ; p
          [ code_inline' "fix"
          ; txt " computes the "
          ; El.a
              ~at:
                [ class' "prose-link"
                ; At.href
                    (Jstr.v
                       "https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node24.html#sec:proc-general-methods")
                ]
              [ txt "fixed-point" ]
          ; txt
              " of our parser. It takes a function which receives the parser being \
               defined... and uses it to define itself."
          ]
      ; h3 [ txt "Playground" ]
      ; p
          [ txt
              "Finally, here is a playground where you can write and test your own \
               parsers."
          ]
      ; playground_table
      ]
  ;;
end

let stateless_view () = View.view Model.initial_model
