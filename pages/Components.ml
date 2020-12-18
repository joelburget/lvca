open Base
open Js_of_ocaml_tyxml.Tyxml_js
open ReactiveData

let empty_elem = Html.span []

let error_msg x = Html.(div ~a:[a_class ["error"]] x)
let success_msg x = Html.(div ~a:[a_class ["success"]] x)

let mk_a ~border ~classes ~label =
  let classes = if border
    then "border-2" :: classes
    else classes
  in
  let classes = Html.a_class classes in
  match label with
    | "" -> [classes]
    | _ -> [classes; Html.a_user_data label ""]

let rows
  ?border:(border=false)
  ?classes:(classes=["ml-2"])
  ?label:(label="")
  elems =
  Html.div ~a:(mk_a ~border ~classes:("flex" :: "flex-col" :: classes) ~label) elems

let r_rows
  ?border:(border=false)
  ?classes:(classes=(React.S.const []))
  ?label:(label="")
  elems =
  let classes = classes
    |> React.S.map (List.append ["flex"; "flex-col"])
    |> React.S.map (fun classes -> if border then "border-2" :: classes else classes)
  in
  let a = match label with
    | "" -> [R.Html.a_class classes]
    | _ ->
      [ Html.a_user_data label ""
      ; R.Html.a_class classes
      ]
  in
  R.Html.div ~a elems

let cols
  ?border:(border=false)
  ?classes:(classes=[])
  ?label:(label="")
  elems =
  Html.div ~a:(mk_a ~border ~classes:("flex" :: "flex-row" :: classes) ~label) elems

let ulist ?classes:(classes=[]) elems =
  Html.(ul ~a:[a_class classes] elems)

let r_ulist ?classes:(classes=(React.S.const [])) elems =
  R.Html.(ul ~a:[a_class classes] elems)

let olist ?classes:(classes=[]) elems =
  Html.(ol ~a:[a_class classes] elems)

let r_olist ?classes:(classes=(React.S.const [])) elems =
  R.Html.(ol ~a:[a_class classes] elems)

let dlist
  ?border:(border=false)
  ?classes:(classes=[])
  ?label:(label="")
  elems =
  let elems = elems
    |> List.map ~f:(fun (k, v) -> Html.[ dt [txt k]; dd [v] ])
    |> List.concat
  in
  Html.dl ~a:(mk_a ~border ~classes ~label) elems

(* let title = Html.h1 *)
let header str = Html.(h2 [txt str])
let subheader str = Html.(h3 [txt str])
let r_subheader str_s = Html.h3 [R.Html.txt str_s]

let txt = Html.txt

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
let button ~onclick str = Html.(button
  ~a:[a_onclick onclick; a_class button_classes]
  [txt str])

let r_button ~onclick (str_s: string React.signal) = R.Html.(button
  ~a:[a_onclick onclick; Html.a_class button_classes]
  (str_s |> React.S.map Html.txt |> RList.singleton_s))

let table
  ?classes:(classes=[])
  header body =
  R.Html.table ~a:[Html.a_class classes] RList.(concat (singleton header) body)

let inline_block x = Html.(div ~a:[a_class ["inline-block"]] [x])
let r_inline_block x = R.Html.div ~a:[Html.a_class ["inline-block"]] x

let toggle ~visible_text ~hidden_text visible_s =
  let e, set_e = React.E.create () in
  let onclick _evt = set_e (not (React.S.value visible_s)); false in
  let text_s = visible_s
    |> React.S.map (function
      | true -> visible_text
      | false -> hidden_text)
  in
  e, r_button ~onclick text_s
