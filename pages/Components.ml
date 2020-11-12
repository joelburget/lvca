open Base
open Js_of_ocaml_tyxml.Tyxml_js

let empty_elem = Html.span []

let error_msg msg = [%html{|<div class="error">|}[Html.txt msg]{|</div>|}]

let mk_a ~border ~classes ~label =
  let classes = if border
    then "border" :: classes
    else classes
  in
  let classes = Html.a_class classes in
  match label with
    | "" -> [classes]
    | _ -> [classes; Html.a_user_data label ""]

let rows
  ?border:(border=false)
  ?classes:(classes=[])
  ?label:(label="")
  elems =
  Html.div ~a:(mk_a ~border ~classes:("rows" :: classes) ~label) elems

let r_rows
  ?border:(border=false)
  ?classes:(classes=(React.S.const []))
  ?label:(label="")
  elems =
  let classes = classes
    |> React.S.map (List.cons "rows")
    |> React.S.map (fun classes -> if border then "border" :: classes else classes)
  in
  let a = match label with
    | "" -> [R.Html.a_class classes]
    | _ ->
      [ R.Html.a_user_data label (React.S.const "")
      ; R.Html.a_class classes
      ]
  in
  R.Html.div ~a elems

let cols
  ?border:(border=false)
  ?classes:(classes=[])
  ?label:(label="")
  elems =
  Html.div ~a:(mk_a ~border ~classes:("cols" :: classes) ~label) elems

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

(* TODO: automatically return false? *)
let button ~onclick str =
  Html.(button ~a:[a_onclick onclick] [txt str])
