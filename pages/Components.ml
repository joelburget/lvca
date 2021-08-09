open Base
open Brr
open Brr_note
open Note
open Prelude

let error_msg x = El.div ~at:[ class' "error" ] x
let success_msg x = El.div ~at:[ class' "success" ] x

(* TODO: remove *)
let mk_at ~border ~classes =
  let classes = if border then "border-2" :: classes else classes in
  List.map classes ~f:Prelude.class'
;;

let rows ?(border = false) ?(classes = [ "ml-2" ]) elems =
  El.div ~at:(mk_at ~border ~classes:("flex" :: "flex-col" :: classes)) elems
;;

let cols ?(border = false) ?(classes = []) elems =
  El.div ~at:(mk_at ~border ~classes:("flex" :: "flex-row" :: classes)) elems
;;

let button_classes =
  [ "px-2"
  ; "border-2"
  ; "border-blue-800"
  ; "dark:border-blue-200"
  ; "rounded"
  ; "bg-transparent"
  ; "hover:bg-blue-700"
  ; "dark:hover:bg-blue-900"
  ; "text-blue-800"
  ; "dark:text-blue-50"
  ; "hover:text-white" (* (no dark) *)
  ; "cursor-pointer"
  ]
;;

let button str =
  let button = El.button ~at:(button_classes |> List.map ~f:class') [ El.txt' str ] in
  let evt : unit event = Evr.on_el Ev.click (fun _ -> ()) button in
  evt, button
;;

let table ?(classes = []) thead tbody =
  let tbody = tbody |> S.map ~eq:Common.htmls_eq (fun tbody -> thead :: tbody) in
  mk_reactive El.table ~at:(classes |> List.map ~f:class') tbody
;;

let inline_block x = El.div ~at:[ class' "inline-block" ] [ x ]
let r_inline_block child_s = mk_reactive' El.div ~at:[ class' "inline-block" ] child_s

let button_toggle ~visible_text ~hidden_text visible_s =
  let text_s =
    visible_s |> S.map (function true -> visible_text | false -> hidden_text)
  in
  let button =
    mk_reactive' El.button ~at:(List.map button_classes ~f:class') (S.map El.txt' text_s)
  in
  let evt = Evr.on_el Ev.click Fn.id button in
  evt, button
;;

let chevron_toggle visible_s =
  let class_s =
    visible_s
    |> S.map (function true -> "gg-chevron-down" | false -> "gg-chevron-right")
    |> S.map (fun cls -> Some (Jstr.v cls))
  in
  let i = El.i [] in
  let () = Elr.def_at At.Name.class' class_s i in
  let elem =
    El.a
      ~at:[ class' "cursor-pointer"; class' "p-1" ]
      [ El.span ~at:[ class' "chevron-icon-wrap" ] [ i ] ]
  in
  let e = Evr.on_el Ev.click (fun _evt -> not (S.value visible_s)) elem in
  e, elem
;;
