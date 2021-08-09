open Base
open Brr
open Brr_note
open Note
open Prelude

let txt' = El.txt'

let mk ?(autofocus = true) ?(border = true) ?(rows = None) ?(cols = 60) ?(at = []) input_s
  =
  let input_dirty_s, update_input_dirty = S.create false in
  let needed_rows =
    match rows with
    | Some n -> n
    | None -> String.count (S.value input_s) ~f:(fun c -> Char.(c = '\n')) + 1
  in
  let input_dirty_elem =
    input_dirty_s
    |> S.map (function
           | true ->
             (match Web_util.platform_special_combo () with
             | Some info_elems ->
               List.concat
                 [ [ txt' "updated, press " ]; info_elems; [ txt' " to re-evaluate)" ] ]
             | None -> [ txt' "updated (press Enter to re-evaluate)" ])
           | false -> [ txt' "" ])
  in
  let classes =
    [ Some "mt-4"
    ; Some "p-8"
    ; Some "font-mono"
    ; Some "dark:bg-transparent"
    ; (if border then Some "border-2" else None)
    ; (if border then Some "border-indigo-900" else None)
    ; (if border then Some "dark:border-indigo-200" else None)
    ]
    |> List.filter_map ~f:Fn.id
    |> List.map ~f:class'
  in
  let input =
    El.textarea
      ~at:
        ([ At.rows needed_rows; At.cols cols ]
        @ classes
        @ if autofocus then [ At.autofocus ] else [])
      [ input_s |> S.value |> txt' ]
  in
  let () = Elr.set_prop El.Prop.value input ~on:(input_s |> S.changes |> E.map Jstr.v) in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.input Fn.id input in
    E.log evt (fun _evt -> update_input_dirty true)
  in
  let keydown_evt =
    let handler evt =
      let keyboard_evt = Ev.as_type evt in
      if Web_util.is_special_enter keyboard_evt
      then (
        Ev.prevent_default evt;
        update_input_dirty false;
        Some (Common.EvaluateInput (El.prop El.Prop.value input |> Jstr.to_string)))
      else None
    in
    Evr.on_el Ev.keydown handler input |> E.filter_map Fn.id
  in
  let select_evt =
    let handler _evt =
      let start = El.prop selection_start input in
      let finish = El.prop selection_end input in
      Common.InputSelect Lvca_provenance.Range.{ start; finish }
    in
    Evr.on_el Ev.select handler input
  in
  let unselect_evt : Common.input_event event =
    Evr.on_el Ev.click (fun _evt -> Common.InputUnselect) input
  in
  let input_event = E.select [ keydown_evt; select_evt; unselect_evt ] in
  let result =
    El.div
      ~at:([ class' "flex"; class' "flex-col" ] @ at)
      [ input; mk_reactive El.span ~at:[ class' "my-2" ] input_dirty_elem ]
  in
  result, input_event
;;
