open Bonsai_web
open Core_kernel
open Js_of_ocaml

let is_enter key =
  String.equal
    "Enter"
    (key##.code |> Js.Optdef.to_option |> Option.value_exn |> Js.to_string)
let is_meta key = Js.to_bool key##.metaKey
let is_shift key = Js.to_bool key##.shiftKey
let is_ctrl key = Js.to_bool key##.ctrlKey

(** Is this an enter keypress plus meta, shift, or ctrl *)
let is_special_enter key = is_enter key && (is_meta key || is_shift key || is_ctrl key)

let str_of_textarea_evt : Dom_html.keyboardEvent Js.t -> string option
  = fun evt ->
  let open Option.Let_syntax in
  let%bind target = Js.Opt.to_option evt##.target in
  let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.textarea target) in
  Js.to_string inp##.value

module TextareaAction = struct
  type t =
    | UpdateInput of string
    | Evaluate of string
  [@@deriving sexp]
end

let textarea
  : contents:string
  -> inject:(TextareaAction.t -> Vdom.Event.t)
  -> result:Vdom.Node.t option
  -> Vdom.Node.t
  = fun ~contents ~inject ~result ->
  let handle_keydown =
    Vdom.Attr.on_keydown (fun evt -> match str_of_textarea_evt evt with
      | Some str ->
         let update =
           if is_special_enter evt
           then (
             Dom.preventDefault evt;
             (* prevent inserting a newline *)
             TextareaAction.Evaluate str)
           else UpdateInput str
         in
         inject update
      | None -> Vdom.Event.Ignore)
  in
  Vdom.Node.(
    div
      []
      [ textarea
          Vdom.Attr.
            [ string_property "rows" "25"; string_property "cols" "90"; handle_keydown ]
          [ text contents ]
      ; div [] [ match result with
        | None -> text "(press (ctrl/shift/meta)-enter to evaluate)"
        | Some node -> node
      ]
      ])
