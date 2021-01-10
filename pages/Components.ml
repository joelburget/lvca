open Base
open Brr
open Brr_note
open Note
open Prelude

let empty_elem = El.span []

let error_msg x = El.div ~at:[class' "error"] x
let success_msg x = El.div ~at:[class' "success"] x

(* TODO: remove *)
let mk_at ~border ~classes =
  let classes = if border
    then "border-2" :: classes
    else classes
  in
  let classes = classes |> List.map ~f:Prelude.class' in
  classes

let rows
  ?border:(border=false)
  ?classes:(classes=["ml-2"])
  elems =
  El.div ~at:(mk_at ~border ~classes:("flex" :: "flex-col" :: classes)) elems

let r_rows
  ?border:(_border=false)
  ?classes:(_classes=(S.const []))
  elems =
    (*
  let classes = classes
    |> S.map (List.append ["flex"; "flex-col"])
    |> S.map (fun classes -> if border then "border-2" :: classes else classes)
  in
  let at = classes |> List.map ~f:class' in
  *)
  El.div elems

let cols
  ?border:(border=false)
  ?classes:(classes=[])
  elems =
  El.div ~at:(mk_at ~border ~classes:("flex" :: "flex-row" :: classes)) elems

let ulist ?classes:(classes=[]) elems =
  El.ul ~at:(classes |> List.map ~f:class') elems

let r_ulist ?classes:(_classes=(S.const [])) elems =
  let result = El.ul (* TODO ~at:(classes |> List.map ~f:class') *) [] in
  let () = Elr.def_children result elems in
  result

let olist ?classes:(classes=[]) elems =
  El.ol ~at:(classes |> List.map ~f:class') elems

let r_olist ?classes:(_classes=(S.const [])) elems =
  let result = El.ol [] in
  (* TODO let result = El.ol ~at:(classes |> List.map ~f:class') elems in *)
  let () = Elr.def_children result elems in
  result

let dlist
  ?border:(border=false)
  ?classes:(classes=[])
  elems =
  let elems = elems
    |> List.map ~f:(fun (k, v) -> [El.dt [Prelude.txt k]; El.dd [v]])
    |> List.concat
  in
  El.dl ~at:(mk_at ~border ~classes) elems

(* let title = El.h1 *)
let header str = El.h2 [txt str]
let subheader str = El.h3 [txt str]
let r_subheader str_s =
  let result = El.h3 [] in
  let () = Elr.def_children result (str_s |> S.map (fun str -> [txt str])) in
  result

let button_classes =
  [ "px-2"
  ; "border-2"
  ; "border-blue-800"; "dark:border-blue-200"
  ; "rounded"
  ; "bg-transparent"
  ; "hover:bg-blue-700"; "dark:hover:bg-blue-900"
  ; "text-blue-800"; "dark:text-blue-50"
  ; "hover:text-white" (* (no dark) *)
  ; "cursor-pointer"
  ]

let button str =
  let button = El.button ~at:(button_classes |> List.map ~f:class') [txt str] in
  let evt : unit event = Evr.on_el Ev.click (fun _ -> ()) button in
  evt, button

let table
  ?classes:(classes=[])
  thead tbody =
  let tbody = tbody |> S.map ~eq:Common.htmls_eq (fun tbody -> thead :: tbody) in
  mk_reactive El.table ~at:(classes |> List.map ~f:class') tbody

let inline_block x = El.div ~at:[class' "inline-block"] [x]
let r_inline_block child_s = mk_reactive' El.div ~at:[class' "inline-block"] child_s

let button_toggle ~visible_text ~hidden_text visible_s =
  let text_s = visible_s
    |> S.map (function
      | true -> visible_text
      | false -> hidden_text)
  in
  let button = mk_reactive' El.button ~at:(List.map button_classes ~f:class')
    (text_s |> S.map txt)
  in
  let evt = Evr.on_el Ev.click Fn.id button in
  evt, button

let chevron_toggle visible_s =
  let _class_s = visible_s
    |> S.map (function
      | true -> ["gg-chevron-down"]
      | false -> ["gg-chevron-right"])
  in
  let i = El.i [] in
  (* TODO let () = Elr.def_at At.class' class_s i in *)
  let elem = El.a
    ~at:[ class' "cursor-pointer"; class' "p-1" ]
    [ El.span ~at:[class' "chevron-icon-wrap"] [ i ] ]
  in
  let e = Evr.on_el Ev.click (fun _evt -> not (S.value visible_s)) elem in
  e, elem
