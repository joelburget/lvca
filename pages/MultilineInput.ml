open Base
open ReactiveData

module Ev = Js_of_ocaml_lwt.Lwt_js_events
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js

let mk
  ?autofocus:(autofocus=true)
  ?border:(border=true)
  ?rows:(rows=None)
  ?cols:(cols=60)
  input_s =
  let open Js_of_ocaml in
  let open Tyxml_js in

  let input_dirty_s, update_input_dirty = React.S.create false in
  let input_event, signal_event = React.E.create () in

  let needed_rows = match rows with
    | Some n -> n
    | None ->
      String.count (React.S.value input_s) ~f:(fun c -> Char.(c = '\n')) + 1
  in

  let input_dirty_elem = input_dirty_s
    |> React.S.map (function
      | true -> (match WebUtil.platform_special_combo () with
        | Some info_elems -> Html.(span (List.concat
          [ [ txt "updated, press " ]
          ; info_elems
          ; [ txt " to re-evaluate)" ]
          ]))
        | None -> Html.txt "updated (press Enter to re-evaluate)"
      )
      | false -> Html.txt ""
    )
  in

  let classes = List.filter_map ~f:Fn.id
    [ Some "mt-4"
    ; Some "p-8"
    ; Some "font-mono"
    ; if border then Some "border-2" else None
    ; if border then Some "border-indigo-900" else None
    ]
  in

  let input = Html.(textarea
    ~a:([ a_rows needed_rows
        ; a_cols cols
        ; a_class classes
        ] @ (if autofocus then [a_autofocus ()] else []))
     (R.Html.txt input_s)
    )
  in
  let input_dom = To_dom.of_textarea input in

  Common.bind_event Ev.inputs input_dom (fun _evt ->
    update_input_dirty true;
    Lwt.return ()
  );

  Common.bind_event Ev.keydowns input_dom (fun evt ->
      Lwt.return
        (if WebUtil.is_special_enter evt
        then (
          Dom.preventDefault evt;
          update_input_dirty false;
          signal_event (Common.InputUpdate (Js.to_string input_dom##.value))
        )
        else ()));

  Common.bind_event Ev.selects input_dom (fun evt ->
      let elem = evt##.target |> Js.Opt.to_option |> Option.value_exn in
      let textarea =
        elem |> Dom_html.CoerceTo.textarea |> Js.Opt.to_option |> Option.value_exn
      in
      let start = textarea##.selectionStart in
      let finish = textarea##.selectionEnd in
      signal_event (Common.InputSelect (start, finish));
      (* Used for debugging only -- can be removed: *)
      (* let str = textarea##.value##substring start finish in *)
      (* Caml.Printf.printf "Selected %u-%u '%s'\n" start finish (Js.to_string str); *)
      Lwt.return ());

  Common.bind_event Ev.clicks input_dom (fun _evt ->
      signal_event Common.InputUnselect;
      Lwt.return ());

  (* XXX why doesn't the textarea automatically update? *)
  let (_ : unit React.event) =
    input_s
    (* Create an event when the input has changed *)
    |> React.S.map ~eq:Caml.(=) Fn.id
    |> React.S.changes
    |> React.E.map (fun input -> input_dom##.value := Js.string input)
  in

  let result = [%html{|
    <div class="flex flex-col">
      |}[input]{|
      |}[R.Html.span ~a:[Html.a_class ["my-2"]] (RList.singleton_s input_dirty_elem)]{|
    </div>
    |}]
  in

  result, input_event
