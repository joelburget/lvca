open Base
open Brr
open Brr_note
open Note
open Prelude
(* open Js_of_ocaml_tyxml.Tyxml_js *)
(* open ReactiveData *)

let empty_elem = El.span []

let error_msg x = El.div ~at:[class' "error"] x
let success_msg x = El.div ~at:[class' "success"] x

(* TODO: remove *)
let mk_at ~border ~classes ~label:_todo =
  let classes = if border
    then "border-2" :: classes
    else classes
  in
  let classes = classes |> List.map ~f:Prelude.class' in
  classes
  (* TODO
  match label with
    | "" -> [classes]
    | _ -> [classes; Html.a_user_data label ""]
  *)

let rows
  ?border:(border=false)
  ?classes:(classes=["ml-2"])
  ?label:(label="")
  elems =
  El.div ~at:(mk_at ~border ~classes:("flex" :: "flex-col" :: classes) ~label) elems

let r_rows
  ?border:(_todo=false)
  ?classes:(_todo=(S.const []))
  ?label:(_todo="")
  elems =
    (* TODO
  let classes = classes
    |> S.map (List.append ["flex"; "flex-col"])
    |> S.map (fun classes -> if border then "border-2" :: classes else classes)
  in
  let at = classes |> List.map ~f:class' in
*)
  (* TODO
  let at = match label with
    | "" -> classes
    | _ ->
      [ Html.a_user_data label ""
      ; R.Html.a_class classes
      ]
  in
    *)
  El.div elems

let cols
  ?border:(border=false)
  ?classes:(classes=[])
  ?label:(label="")
  elems =
  El.div ~at:(mk_at ~border ~classes:("flex" :: "flex-row" :: classes) ~label) elems

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
  ?label:(label="")
  elems =
  let elems = elems
    |> List.map ~f:(fun (k, v) -> [El.dt [Prelude.txt k]; El.dd [v]])
    |> List.concat
  in
  El.dl ~at:(mk_at ~border ~classes ~label) elems

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

(* TODO: automatically return false? *)
let button ~onclick str =
  let button = El.button ~at:(button_classes |> List.map ~f:class') [txt str] in
  let _ : unit event = Evr.on_el Ev.click onclick button in
  button

let r_button ~onclick (str_s: string signal) =
  let button = El.button ~at:(button_classes |> List.map ~f:class') [] in
  let () = Elr.def_children button (str_s |> S.map (fun str -> [txt str])) in
  let _ : unit event = Evr.on_el Ev.click onclick button in
  button

let table
  ?classes:(classes=[])
  _todo1 _todo2 =
  let result = El.table ~at:(classes |> List.map ~f:class') [] in
  (* let () = Elr.def_children result ( *)
  (* RList.(concat (singleton header) body) *)
  result

let inline_block x = El.div ~at:[class' "inline-block"] [x]
let r_inline_block x = El.div ~at:[class' "inline-block"] x

let button_toggle ~visible_text ~hidden_text visible_s =
  let e, set_e = E.create () in
  let onclick _evt = set_e (not (S.value visible_s)) in
  let text_s = visible_s
    |> S.map (function
      | true -> visible_text
      | false -> hidden_text)
    (* |> S.map (fun str -> [txt str]) *)
  in
  (*
  let button = r_button [] in
  let () = Elr.def_children button text_s in
  let _ : unit event = Evr.on_el Ev.click onclick button in
  *)
  e, r_button ~onclick text_s

let chevron_toggle visible_s =
  let e, set_e = E.create () in
  let onclick _evt = set_e (not (S.value visible_s)); false in
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
  let _ : bool event = Evr.on_el Ev.click onclick elem in
  e, elem
