open Base
open Brr
open Brr_note
open Lvca_provenance
open Note
open Prelude

let mk ?(autofocus = false) ?highlights_s:(external_highlights_s = S.const []) input_s =
  let dirty_input_s, update_dirty = S.create false in
  let highlights_s =
    external_highlights_s |> Note.S.changes |> Note.S.hold ~eq:Ranges.( = ) []
  in
  let at =
    let classes =
      [ "font-mono"
        (* ; "border-2" *)
        (* ; "border-indigo-900" *)
        (* ; "rounded" *)
      ; "p-1" (* ; "focus:ring" *)
      ; "bg-none"
      ; "border-none"
      ; "display-block"
      ; "w-full"
      ]
      |> List.map ~f:class'
    in
    let at' =
      List.filter_map
        ~f:Fn.id
        [ Some (At.type' (Jstr.v "text"))
        ; Some (input_s |> S.value |> Jstr.v |> At.value)
        ; (if autofocus then Some At.autofocus else None)
        ]
    in
    at' @ classes
  in
  let input = El.input ~at () in
  let () = Elr.set_prop El.Prop.value input ~on:(input_s |> S.changes |> E.map Jstr.v) in
  let highlighted_input_s =
    S.l2
      (fun input_str highlight_ranges ->
        Ranges.mark_string highlight_ranges input_str
        |> List.map ~f:(fun string_status ->
               let Range.{ start; finish } =
                 match string_status with Ranges.Covered rng | Uncovered rng -> rng
               in
               let str = String.sub input_str ~pos:start ~len:(finish - start) in
               match string_status with
               | Ranges.Covered _ ->
                 El.span ~at:(classes "bg-pink-200 rounded") [ txt str ]
               | Uncovered _ -> txt str))
      input_s
      highlights_s
  in
  (* TODO: sync scroll position *)
  let input_shadow =
    let at =
      At.true' (Jstr.v "aria-hidden")
      :: classes "absolute -z-1 left-1 top-1 text-transparent font-mono whitespace-pre"
    in
    El.div ~at []
  in
  let () = Elr.def_children input_shadow highlighted_input_s in
  let updated_elem = El.span ~at:[ class' "ml-1" ] [] in
  let () =
    Elr.def_children
      updated_elem
      (dirty_input_s
      |> S.map (function true -> "updated (press Enter to re-evaluate)" | false -> "")
      |> S.map (fun str -> [ txt str ]))
  in
  let result =
    El.div
      [ El.div
          ~at:
            (classes
               "relative overflow-hidden border border-blue-800 dark:border-blue-200 \
                rounded")
          [ input; input_shadow ]
      ; updated_elem
      ]
  in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.input (fun _evt -> update_dirty true) input in
    E.log evt Fn.id
  in
  let keydown_evt =
    let handler evt =
      let keyboard_evt = Ev.as_type evt in
      if Web_util.is_enter keyboard_evt
      then (
        Ev.prevent_default evt;
        update_dirty false;
        Some (Common.InputUpdate (El.prop El.Prop.value input |> Jstr.to_string)))
      else None
    in
    Evr.on_el Ev.keydown handler input |> E.filter_map Fn.id
  in
  let select_evt =
    let handler _evt =
      let start = El.prop selection_start input in
      let finish = El.prop selection_end input in
      Common.InputSelect Range.{ start; finish }
    in
    Evr.on_el Ev.select handler input
  in
  let unselect_evt : Common.input_event event =
    Evr.on_el Ev.click (fun _evt -> Common.InputUnselect) input
  in
  let input_event = E.select [ keydown_evt; select_evt; unselect_evt ] in
  result, input_event
;;
