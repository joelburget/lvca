open Core_kernel
open Lvca
open Js_of_ocaml_tyxml.Tyxml_js

exception FailedRender of string

let str_of_tm tm = tm |> NonBinding.to_nominal |> Binding.Nominal.pp_term_str

open Html5

let render_inline_atom
    : NonBinding.term -> [< Html_types.div_content_fun > `Textarea ] To_dom.elt
  = function
  | NonBinding.Operator ("inlineAtom", [ _attrs; Primitive (PrimString str) ]) ->
    span [ txt str ]
  | tm -> raise @@ FailedRender ("inline atom: unexpected term: " ^ str_of_tm tm)
;;

let render_paragraph
    : NonBinding.term -> [< Html_types.div_content_fun > `Div ] To_dom.elt
  = function
  | NonBinding.Operator ("inline", inline_atoms) ->
    let inline_atoms' = List.map inline_atoms ~f:render_inline_atom in
    div inline_atoms'
  | tm -> raise @@ FailedRender ("render_paragraph: unexpected term: " ^ str_of_tm tm)
;;

let render_block
    (* : NonBinding.term -> [< Html_types.div_content_fun > `Div ] To_dom.elt *)
  = function
  | NonBinding.Operator ("header", [ level; Primitive (PrimString str) ]) ->
    (match level with
    | Operator ("h1", []) -> h1 [ txt str ]
    | Operator ("h2", []) -> h2 [ txt str ]
    | Operator ("h3", []) -> h3 [ txt str ]
    | tm -> raise @@ FailedRender ("render_block header: unexpected term: " ^ str_of_tm tm))
  | Operator ("paragraph", [ para ]) -> render_paragraph para
  | tm ->
    raise @@ FailedRender ("render_block paragraph: unexpected term: " ^ str_of_tm tm)
;;

let render_doc : NonBinding.term -> [< Html_types.div_content_fun > `Div ] To_dom.elt
  = function
  | Operator ("document", blocks) ->
    let blocks' = List.map ~f:render_block blocks in
    div blocks'
  | tm -> raise @@ FailedRender ("render_doc: unexpected term: " ^ str_of_tm tm)
;;

let render
    :  Binding.Nominal.term
    -> ([< Html_types.div_content_fun > `Div ] To_dom.elt, string) Result.t
  =
 fun tm ->
  match NonBinding.from_nominal tm with
  | None -> Error "Failed to convert term"
  | Some db_tm -> (try Ok (render_doc db_tm) with FailedRender msg -> Error msg)
;;
