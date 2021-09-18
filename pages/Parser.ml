open Base
open Brr
open Brr_note
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Note
open Prelude
module P = Lvca_languages.Parser
open Components

let ( >> ) = Lvca_util.( >> )
let html_eq = Common.html_eq

let em, table, tr, td, span, div, h3, h4, p, pre, code, txt' =
  El.(em, table, tr, td, span, div, h3, h4, p, pre, code, txt')
;;

module Model = struct
  type input_sig = string signal * (string -> unit)

  module TraceSnapshot = struct
    type t =
      { success : bool
      ; pre_pos : int
      ; post_pos : int
      ; parser : Source_ranges.t P.Term.t
      ; snapshots : t list
      }

    let rec ( = ) t1 t2 =
      Bool.(t1.success = t2.success)
      && Int.(t1.pre_pos = t2.pre_pos)
      && Int.(t1.post_pos = t2.post_pos)
      && List.equal ( = ) t1.snapshots t2.snapshots
      && P.Term.equal ~info_eq:Source_ranges.( = ) t1.parser t2.parser
    ;;

    let rec restrict_snapshot : P.Evaluate.trace_snapshot -> t =
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
    | SetSelection of Source_ranges.t
    | UpdateTest of string
    | SetInputHl of Source_ranges.t
end

module DebuggerAction = struct
  type t =
    | SubparserZoom of int (** Click on a subparser *)
    | ChopStack of int list (** Click on a stack member *)
end

let parser =
  let c_comment, ( >>| ) = Lvca_parsing.(c_comment, ( >>| )) in
  P.Parse.t Lvca_core.Term.(parse ~comment:c_comment >>| map_info ~f:Commented.get_range)
;;

let parse_parser = Lvca_parsing.parse_string parser

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

  let satisfy_is_alpha = "satisfy (c -> {is_alpha c})"
  let satisfy_is_digit = "satisfy (c -> {is_digit c})"
  let sequence1 = {|. ' '* '+' ' '* . -> {{"parsed an addition"}}|}
  let sequence2 = "a=. ' '* '+' ' '* b=. -> {{add(a; b)}}"

  let fix =
    {|let atom = choice (name | literal) in
choice (
  | a=atom ' '* '+' ' '* e=expr -> {{add(a; e)}}
  | a=atom -> {a}
)
      |}
  ;;
  (*
    {|let atom = choice (name | literal) in
fix (expr -> choice (
  | a=atom ' '* '+' ' '* e=expr -> {{add(a; e)}}
  | a=atom -> {a}
))|}
       *)
end

module Prelude = struct
  let parse_parser_exn = P.parse_parser >> Result.ok_or_failwith
  let alpha = parse_parser_exn Examples.satisfy_is_alpha
  let digit = parse_parser_exn Examples.satisfy_is_digit
  let name = parse_parser_exn {|chars=alpha+ -> {var (string_of_chars chars)}|}

  let literal =
    parse_parser_exn
      {|chars=digit+ -> {let str = string_of_chars chars in {literal(str)}}|}
  ;;

  let ctx : P.Evaluate.parser_ctx =
    String.Map.of_alist_exn
      [ "alpha", alpha; "digit", digit; "name", name; "literal", literal ]
    |> Map.map ~f:(P.Term.map_info ~f:(Source_ranges.of_opt_range ~buf:"prelude"))
  ;;
end

let pp_view ~highlight_s tm fmt =
  let clear, clear_formatter = E.create () in
  let Range_formatter.{ elem; formatter; selection_e } =
    Range_formatter.mk ~clear ~selection_s:highlight_s ()
  in
  (* TODO tie this to actual font size *)
  let font_size = 14. in
  let font_adjust = font_size *. 0.6 (* 0.6 is an estimate of font width to height *) in
  let initial_width = 40. *. font_adjust in
  let px_size_s, _set_px_size = S.create ~eq:Float.( = ) initial_width in
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
  let pp_view, tm_selection_e = pp_view ~highlight_s tm Nominal.Term.pp_source_ranges in
  let tree_view, tree_selection_e = Tree_view.view_tm tm in
  let view =
    div
      [ div ~at:[ class' "my-2" ] [ success_msg [ pp_view ] ]
      ; div ~at:[ class' "my-2" ] [ span [ txt' "Tree viewer:" ] ]
      ; div ~at:[ class' "my-2" ] [ tree_view ]
      ]
  in
  let e = E.select [ tm_selection_e; tree_selection_e ] in
  view, e
;;

let view_core ~highlight_s core =
  let pp_view, tm_selection_e = pp_view ~highlight_s core Lvca_core.Term.pp in
  success_msg [ pp_view ], tm_selection_e
;;

let view_parser ~highlight_s ~success parser =
  let elt, tm_selection_e =
    pp_view ~highlight_s parser (fun ppf tm ->
        tm |> P.Term.to_nominal |> Nominal.Term.pp_source_ranges ppf)
  in
  El.div ~at:[ class' (if success then "success" else "error") ] [ elt ], tm_selection_e
;;

let view_parser_ignore_selection ~highlight_s ~success parser =
  let elem, _ = view_parser parser ~highlight_s ~success in
  elem
;;

let string_location ~str ~loc =
  if String.(str = "")
  then
    El.div ~at:(classes "font-mono mx-2 bg-gray-100 underline") [ txt' "(empty string)" ]
  else (
    let before = String.subo str ~len:loc in
    let after = String.subo str ~pos:loc in
    let before_elem = if String.(before = "") then El.wbr () else txt' before in
    let after_elem = if String.(after = "") then El.wbr () else txt' after in
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
    [ span [ txt' ("advances the input " ^ chars ^ " to") ]
    ; inline_block (string_location ~str ~loc:post_pos)
    ]
;;

let snapshot_controls str snapshots =
  let header =
    tr
      [ td ~at:(classes "p-2 border-t-2 border-b-2 border-r-2 w-1/2") [ txt' "parser" ]
      ; td ~at:(classes "p-2 border-t-2 border-b-2 border-r-2") [ txt' "action" ]
      ; td ~at:(classes "p-2 border-t-2 border-b-2") []
      ]
  in
  let path_evts, body =
    snapshots
    |> List.mapi ~f:(fun i snapshot ->
           let Model.TraceSnapshot.{ success; parser; _ } = snapshot in
           let click_evt, btn = button "view" in
           let click_evt = click_evt |> E.map (fun _ -> DebuggerAction.SubparserZoom i) in
           let highlight_s = S.const Source_ranges.empty in
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
      (if List.length snapshots > 0
      then [ txt' msg; snapshot_controls ]
      else [ txt' msg ]) )
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
           let highlight_s = S.const Source_ranges.empty in
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
           if List.length path > 0 then stack_view else txt' "(empty)")
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
          | SubparserZoom i -> List.snoc (S.value path_s) i
        in
        set_path path)
  in
  let highlight_s = S.const Source_ranges.empty in
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
             [ (if success then txt' "succeeds" else txt' "fails") ])
    |> S.map ~eq:(List.equal html_eq) List.return
    |> mk_reactive span
  in
  let status_view =
    El.div
      [ inline_block (span [ txt' "This parser" ])
      ; txt' " "
      ; inline_block status_view
      ; txt' " "
      ; inline_block (txt' "and")
      ; txt' " "
      ; current_snapshot_s
        |> S.map ~eq:html_eq (snapshot_advanced_view str)
        |> r_inline_block
      ; txt' "."
      ]
  in
  let input_view =
    cols
      [ div [ span [ txt' "The input to this parser is" ] ]
      ; txt' " "
      ; pre_loc_view
      ; txt' "."
      ]
  in
  table
    ~at:[ class' "w-full" ]
    [ tr
        [ td ~at:(classes "p-2 border-b-2 border-r-2") [ txt' "stack" ]
        ; mk_reactive' td ~at:(classes "p-2 border-b-2") stack_view
        ]
    ; tr
        [ td ~at:(classes "p-2 border-b-2 border-r-2") [ txt' "parser" ]
        ; td
            ~at:(classes "p-2 border-b-2")
            [ parser_view |> mk_reactive' div ~at:[ class' "py-2" ]
            ; input_view
            ; status_view
            ]
        ]
    ; tr
        [ td ~at:(classes "p-2 border-r-2") [ txt' "subparsers" ]
        ; td ~at:[ class' "p-2" ] [ mk_reactive' div controls_s ]
        ]
    ]
;;

module View = struct
  module Evaluate = P.Evaluate

  let view_parser_test
      ?(term_ctx = String.Map.empty)
      ?(parser_ctx = String.Map.empty)
      ~highlight_s
      parser_or_err
      test_str
    =
    let mk_err msg = error_msg [ El.span ~at:[ class' "p-2" ] [ txt' msg ] ] in
    match parser_or_err with
    | Error msg ->
      let result = mk_err msg in
      let trace = El.div [ txt' "not available: parser failed to parse" ] in
      result, trace, E.never
    | Ok parser ->
      let parser = P.Term.map_info ~f:(Source_ranges.of_opt_range ~buf:"parser") parser in
      let toplevel_result = Evaluate.parse_direct ~term_ctx ~parser_ctx parser test_str in
      let P.Evaluate.{ didnt_consume_msg; result; snapshot } = toplevel_result in
      let result, select_e =
        match result with
        | Error (msg, tm_opt) ->
          (match tm_opt with
          | None -> mk_err msg, E.never
          | Some tm ->
            let highlight_s = S.const Source_ranges.empty in
            let core, select_e = view_core ~highlight_s tm in
            error_msg [ El.span ~at:[ class' "p-2" ] [ txt' msg ]; core ], select_e)
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
      ?(parser_ctx = String.Map.empty)
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
      let eq = Result.equal (P.Term.equal ~info_eq:Opt_range.( = )) String.( = ) in
      parser_str_s |> S.map ~eq parse_parser
    in
    let input_hl_s, set_input_hl = S.create ~eq:Source_ranges.( = ) Source_ranges.empty in
    let test_s' =
      let eq = Tuple3.equal html_eq html_eq phys_equal in
      S.l2 ~eq (view_parser_test ~parser_ctx ~highlight_s:input_hl_s) parser_s test_s
    in
    let result = test_s' |> S.map ~eq:html_eq Tuple3.get1 |> mk_reactive' div in
    let trace_s = test_s' |> S.map ~eq:html_eq Tuple3.get2 in
    let tm_selection_s, set_selection =
      S.create ~eq:Source_ranges.( = ) Source_ranges.empty
    in
    let _sink : Logr.t =
      S.log test_s' (fun (_, _, e) ->
          let _sink : Logr.t option = E.log e set_selection in
          ())
    in
    let input_hl_s =
      tm_selection_s
      |> S.map ~eq:Opt_range.( = ) (fun ranges ->
             Option.(Map.find ranges "input" >>= Range.list_range))
      |> S.map ~eq:Ranges.( = ) Ranges.of_opt_range
    in
    let parser_hl_s =
      tm_selection_s
      |> S.map ~eq:Source_ranges.( = ) (Source_ranges.restrict ~buf:"parser")
    in
    let parser_elem =
      match parser_elem with
      | Some elem -> elem
      | None ->
        parser_s
        |> S.map ~eq:html_eq (function
               | Ok parser ->
                 parser
                 |> P.Term.map_info ~f:(Source_ranges.of_opt_range ~buf:"parser")
                 |> view_parser ~highlight_s:parser_hl_s ~success:true
                 |> fst
               | Error _ -> pre [ mk_reactive' code (parser_str_s |> S.map txt') ])
        |> mk_reactive' div
    in
    let test_input, test_evt = Single_line_input.mk test_s ~highlights_s:input_hl_s in
    let _sink : Logr.t option =
      E.log test_evt (function
          | Common.Evaluate_input str -> update_test str
          | Input_select rng -> set_input_hl (Source_ranges.of_range ~buf:"input" rng)
          | Input_unselect | Input_update _ -> set_input_hl Source_ranges.empty)
    in
    let trace_cell =
      S.l2
        ~eq:html_eq
        (fun trace show_trace -> if show_trace then trace else txt' "")
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
              [ td ~at:(classes "border-2 p-2 w-1/6") [ txt' "Parser" ]
              ; td ~at:(classes "border-2 p-2 w-5/6") [ parser_elem ]
              ]
          ; mk_row [ txt' "Input" ] [ test_input ]
          ; mk_row [ txt' "Result" ] [ result ]
          ; mk_row [ txt' "Debugger"; trace_button ] [ trace_cell ]
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
      Multiline_input.mk ~autofocus:false ~border:false pg_parser_input
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
          match evt with Common.Evaluate_input str -> set_pg_parser_input str | _ -> ())
    in
    let demo_env =
      String.Map.of_alist_exn
        [ "any-char", any_char_table
        ; "char", char_table
        ; "string", string_table
        ; "satisfy1", satisfy1_table
        ; "satisfy-is-alpha", satisfy_is_alpha_table
        ; "satisfy-is-digit", satisfy_is_digit_table
        ; "star", star_table
        ; "plus", plus_table
        ; "count", count_table
        ; "choice1", choice1_table
        ; "choice2", choice2_table
        ; "choice3", choice3_table
        ; "let", let_table
        ; "fail", fail_table
        ; "sequence1", sequence1_table
        ; "sequence2", sequence2_table
        ; "fix", fix_table
        ; "playground", playground_table
        ]
    in
    Md_viewer.of_string ~demo_env [%blob "md/parsing-language.md"]
  ;;
end

let stateless_view () = View.view Model.initial_model
