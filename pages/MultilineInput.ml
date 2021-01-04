open Base
open Brr
open Brr_note
open Note
open Prelude

let mk
  ?autofocus:(autofocus=true)
  ?border:(border=true)
  ?rows:(rows=None)
  ?cols:(cols=60)
  input_s =
  let input_dirty_s, update_input_dirty = S.create false in
  let input_event, signal_event = E.create () in

  let needed_rows = match rows with
    | Some n -> n
    | None ->
      String.count (S.value input_s) ~f:(fun c -> Char.(c = '\n')) + 1
  in

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

  let classes =
    [ Some "mt-4"
    ; Some "p-8"
    ; Some "font-mono"
    ; Some "dark:bg-transparent"
    ; if border then Some "border-2" else None
    ; if border then Some "border-indigo-900" else None
    ; if border then Some "dark:border-indigo-200" else None
    ]
    |> List.filter_map ~f:Fn.id
    |> List.map ~f:class'
  in

  let input = El.textarea
    ~at:([ At.rows needed_rows
        ; At.cols cols
        ] @ classes @ (if autofocus then [At.autofocus] else []))
     []
  in
  let () = Elr.def_children input (input_s |> S.map (fun str -> [txt str])) in
  let _ : unit event = Evr.on_el Ev.input (fun _evt -> update_input_dirty true) input in
  let _ = Evr.on_el Ev.keydown (fun evt ->
    let evt = Ev.as_type evt in
    if WebUtil.is_special_enter evt
    then (
      (* TODO: Dom.preventDefault evt; *)
      update_input_dirty false;
      signal_event (Common.InputUpdate (El.prop El.Prop.value input |> Jstr.to_string))
    )
    else ())
    input
  in
  let _ = Evr.on_el Ev.select (fun _evt ->
    let start = El.prop selection_start input in
    let finish = El.prop selection_end input in
    signal_event (Common.InputSelect (start, finish));
    ) input
  in

  let _ = Evr.on_el Ev.click (fun _evt -> signal_event Common.InputUnselect) input in

  (* XXX why doesn't the textarea automatically update? *)
  let (_ : unit event) =
    input_s
    (* Create an event when the input has changed *)
    |> S.map ~eq:Caml.(=) Fn.id
    |> S.changes
    |> E.map (fun str -> El.set_prop El.Prop.value (Jstr.v str) input)
  in

  let span = El.span ~at:[class' "my-2"] [] in
  let () = Elr.def_children span input_dirty_elem in
  let result = El.div ~at:[class' "flex"; class' "flex-col"] [ input; span ] in
  result, input_event
