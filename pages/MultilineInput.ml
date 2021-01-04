open Base
open Brr
open Brr_note
open Note
open Prelude

let mk
  ?autofocus:(_autofocus=true)
  ?border:(border=true)
  ?rows:(rows=None)
  ?cols:(_cols=60)
  input_s =
  let input_dirty_s, _update_input_dirty = S.create false in
  let input_event, _signal_event = E.create () in

  let _needed_rows = match rows with
    | Some n -> n
    | None ->
      String.count (S.value input_s) ~f:(fun c -> Char.(c = '\n')) + 1
  in

  let txt str = El.txt (Jstr.v str) in

  let input_dirty_elem = input_dirty_s
    |> S.map (function
      | true -> (match WebUtil.platform_special_combo () with
        | Some info_elems -> List.concat
          [ [ txt "updated, press " ]
          ; info_elems
          ; [ txt " to re-evaluate)" ]
          ]
        | None -> [txt "updated (press Enter to re-evaluate)"]
      )
      | false -> [txt ""]
    )
  in

  let _classes = List.filter_map ~f:Fn.id
    [ Some "mt-4"
    ; Some "p-8"
    ; Some "font-mono"
    ; Some "dark:bg-transparent"
    ; if border then Some "border-2" else None
    ; if border then Some "border-indigo-900 dark:border-indigo-200" else None
    ]
  in

  let input = El.(textarea
    (* TODO
    ~a:([ a_rows needed_rows
        ; a_cols cols
        ; a_class classes
        ] @ (if autofocus then [a_autofocus ()] else []))
     *)
     []
    )
  in
  let () = Elr.def_children input (input_s |> S.map (fun str -> [txt str])) in

  (* TODO
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
      (* Stdio.printf "Selected %u-%u '%s'\n" start finish (Js.to_string str); *)
      Lwt.return ());

  Common.bind_event Ev.clicks input_dom (fun _evt ->
      signal_event Common.InputUnselect;
      Lwt.return ());

  (* XXX why doesn't the textarea automatically update? *)
  let (_ : unit event) =
    input_s
    (* Create an event when the input has changed *)
    |> S.map ~eq:Caml.(=) Fn.id
    |> S.changes
    |> E.map (fun input -> input_dom##.value := Js.string input)
  in
  *)

  let span = El.span ~at:[class' "my-2"] [] in
  let () = Elr.def_children span input_dirty_elem in

  let result = El.div
    ~at:[class' "flex"; class' "flex-col"]
    [ input; span ]
   (*[%html{|
    <div class="flex flex-col">
      |}[input]{|
      |}[R.Html.span ~a:[Html.a_class ["my-2"]] (RList.singleton_s input_dirty_elem)]{|
    </div>
    |}]*)
  in

  result, input_event
