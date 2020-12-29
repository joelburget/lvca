open Base
open Lvca_syntax
open ReactiveData

module Ev = Js_of_ocaml_lwt.Lwt_js_events

let mk
  ?autofocus:(autofocus=false)
  ?highlights_s:(external_highlights_s=React.S.const [])
  input_s =
  let open Js_of_ocaml in
  let open Js_of_ocaml_tyxml.Tyxml_js in

  let dirty_input_s, update_dirty = React.S.create false in
  let input_event, signal_event = React.E.create () in

  let highlights_s = SafeReact.E.select
    [ SafeReact.S.diff (fun v _ -> v) external_highlights_s
    ; input_event |> SafeReact.E.map (fun _ -> [])
    ]
    |> SafeReact.S.hold ~eq:Ranges.(=) []
  in

  let input_value = React.S.value input_s in

  let a = List.filter_map ~f:Fn.id Html.
    [ Some (a_input_type `Text)
    (* ; inputmode |> Option.map ~f:a_inputmode *)
    ; Some (a_value input_value)
    ; Some (a_class
      [ "font-mono"
      (* ; "border-2" *)
      (* ; "border-indigo-900" *)
      (* ; "rounded" *)
      ; "p-1"
      (* ; "focus:ring" *)
      ; "bg-none"
      ; "border-none"
      ; "display-block"
      ; "w-full"
      ])
    ; if autofocus then Some (a_autofocus ()) else None
    ]
  in

  let input = Html.input ~a () in

  let highlighted_input_s = React.S.l2
    (fun input_str highlight_ranges -> Ranges.mark_string highlight_ranges input_str
       |> List.map ~f:(fun string_status ->
         let Range.{ start; finish } =
           match string_status with Covered rng | Uncovered rng -> rng
         in
         let str = String.sub input_str ~pos:start ~len:(finish - start) in
         Html.(match string_status with
           | Covered _ -> span ~a:[a_class ["bg-pink-200"; "rounded"]] [txt str]
           | Uncovered _ -> txt str)
       )
    )
    input_s
    highlights_s
    |> RList.from_signal
  in

  (* TODO: sync scroll position *)
  let input_shadow = R.Html.(div
    ~a:[ Html.a_class
         [ "absolute"
         ; "-z-1"
         ; "left-1"
         ; "top-1"
         ; "text-transparent"
         ; "font-mono"
         ; "whitespace-pre"
         ]
       ; Html.a_aria "hidden" ["true"]
       ]
    highlighted_input_s
  )
  in

  let updated_elem = dirty_input_s
    |> React.S.map (function
      | true -> "updated (press Enter to re-evaluate)"
      | false -> ""
    )
    |> R.Html.txt
  in

  let result = [%html{|
    <div>
      <div class="relative overflow-hidden border border-blue-800 dark:border-blue-200 rounded">
        |}[input; input_shadow]{|
      </div>
      |}[Html.(span ~a:[a_class ["ml-1"]] [updated_elem])]{|
    </div>
    |}]
  in

  let input_dom = To_dom.of_input input in

  Common.bind_event Ev.inputs input_dom (fun _evt ->
    update_dirty true;
    Lwt.return ()
  );

  Common.bind_event Ev.keydowns input_dom (fun evt ->
    let key_name = evt##.code
      |> Js.Optdef.to_option
      |> Option.value_exn
      |> Js.to_string
    in
    let _ : unit = match key_name with
      | "Enter" -> (
        Dom.preventDefault evt;
        update_dirty false;
        signal_event (Common.InputUpdate (Js.to_string input_dom##.value))
      )
      | _ -> ()
    in
    Lwt.return ()
  );

  Common.bind_event Ev.selects input_dom (fun evt ->
      let elem = evt##.target |> Js.Opt.to_option |> Option.value_exn in
      let input =
        elem |> Dom_html.CoerceTo.input |> Js.Opt.to_option |> Option.value_exn
      in
      let start = input##.selectionStart in
      let finish = input##.selectionEnd in
      signal_event (Common.InputSelect (start, finish));
      (* Used for debugging only -- can be removed: *)
      (* let str = input##.value##substring start finish in *)
      (* printf "Selected %u-%u '%s'\n" start finish (Js.to_string str); *)
      Lwt.return ());

  Common.bind_event Ev.clicks input_dom (fun _evt ->
      signal_event Common.InputUnselect;
      Lwt.return ());

  let (_: unit React.event) = input_s
    (* Create an event when the input has changed *)
    |> React.S.map ~eq:Caml.(=) Fn.id
    |> React.S.changes
    |> React.E.map (fun s -> input_dom##.value := Js.string s)
  in

  result, input_event
