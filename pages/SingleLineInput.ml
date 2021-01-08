open Base
open Brr
open Brr_note
open Lvca_syntax
open Note
open Prelude

let mk
  ?autofocus:(autofocus=false)
  ?highlights_s:(external_highlights_s=S.const [])
  input_s =
  let dirty_input_s, update_dirty = S.create false in
  let input_event, signal_event = E.create () in

  let highlights_s = Note.E.select
    [ Note.S.changes external_highlights_s
    ; input_event |> Note.E.map (fun _ -> [])
    ]
    |> Note.S.hold ~eq:Ranges.(=) []
  in

  (* let input_value = Note.S.value input_s in *)

  let at =
    let classes =
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
      ]
      |> List.map ~f:class'
    in
    let at' = List.filter_map ~f:Fn.id
      [
      (* TODO [ Some (a_input_type `Text) *)
      (* ; inputmode |> Option.map ~f:a_inputmode *)
      (* TODO ; Some (a_value input_value) *)
        if autofocus then Some At.autofocus else None
      ]
    in
    at' @ classes
  in

  let input = El.input ~at () in

  let highlighted_input_s = S.l2
    (fun input_str highlight_ranges -> Ranges.mark_string highlight_ranges input_str
       |> List.map ~f:(fun string_status ->
         let Range.{ start; finish } =
           match string_status with Ranges.Covered rng | Uncovered rng -> rng
         in
         let str = String.sub input_str ~pos:start ~len:(finish - start) in
         match string_status with
           | Ranges.Covered _ -> El.span ~at:(classes "bg-pink-200 rounded") [txt str]
           | Uncovered _ -> txt str
       )
    )
    input_s
    highlights_s
  in

  (* TODO: sync scroll position *)
  let input_shadow = El.div
    ~at:(classes "absolute -z-1 left-1 top-1 text-transparent font-mono whitespace-pre")
       (* TODO ; Html.a_aria "hidden" ["true"] *)
    []
  in

  let () = Elr.def_children input_shadow highlighted_input_s in

  let updated_elem =  El.span ~at:[class' "ml-1"] [] in
  let () = Elr.def_children updated_elem (dirty_input_s
    |> S.map (function
      | true -> "updated (press Enter to re-evaluate)"
      | false -> ""
    )
    |> S.map (fun str -> [txt str])
  )
  in

  let result = El.div
    [ El.div
      ~at:(classes "relative overflow-hidden border border-blue-800 dark:border-blue-200 rounded")
      [input; input_shadow]
    ; updated_elem
    ]
  in

  let _ : unit event = Evr.on_el Ev.input (fun _evt -> update_dirty true) input in
  let _ : unit event = Evr.on_el Ev.keydown (fun evt ->
    let evt = Ev.as_type evt in
    if WebUtil.is_enter evt
    then
      (* TODO Dom.preventDefault evt; *)
      update_dirty false;
      signal_event (Common.InputUpdate (El.prop El.Prop.value input |> Jstr.to_string))
    ) input
  in

  let _ : unit event = Evr.on_el Ev.select (fun _evt ->
      let start = El.prop selection_start input in
      let finish = El.prop selection_end input in
      signal_event (Common.InputSelect (start, finish));
      (* Used for debugging only -- can be removed: *)
      (* let str = input##.value##substring start finish in *)
      (* printf "Selected %u-%u '%s'\n" start finish (Js.to_string str); *)
  )
  input
  in

  let _ : unit event =
    Evr.on_el Ev.click (fun _evt -> signal_event Common.InputUnselect) input
  in

  let _: unit event = input_s
    (* Create an event when the input has changed *)
    |> S.map ~eq:Caml.(=) Fn.id
    |> S.changes
    |> E.map (fun s -> El.set_prop El.Prop.value (Jstr.v s) input)
  in

  result, input_event
