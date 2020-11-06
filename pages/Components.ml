open Js_of_ocaml_tyxml.Tyxml_js

let empty_elem = Html.span []

let error_msg msg = [%html{|<div class="error">|}[Html.txt msg]{|</div>|}]

let rows ?classes:(classes=[]) elems =
  Html.(div ~a:[a_class ("rows" :: classes)] elems)

let cols ?classes:(classes=[]) elems =
  Html.(div ~a:[a_class ("cols" :: classes)] elems)

(* let title = Html.h1 *)
let header str = Html.(h2 [txt str])
let subheader str = Html.(h3 [txt str])

let txt = Html.txt

(* TODO: automatically return false? *)
let button ~onclick str =
  Html.(button ~a:[a_onclick onclick] [txt str])
