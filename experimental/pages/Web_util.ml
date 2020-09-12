open Core_kernel
open Js_of_ocaml

let str_of_textarea_evt : Dom_html.keyboardEvent Js.t -> string option =
 fun evt ->
  let open Option.Let_syntax in
  let%bind target = Js.Opt.to_option evt##.target in
  let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.textarea target) in
  Js.to_string inp##.value
;;

let str_of_input_evt : Dom_html.keyboardEvent Js.t -> string option =
 fun evt ->
  let open Option.Let_syntax in
  let%bind target = Js.Opt.to_option evt##.target in
  let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.input target) in
  Js.to_string inp##.value
;;

module TextareaAction = struct
  type t =
    | UpdateInput of string
    | Evaluate of string
  [@@deriving sexp]
end

(* type line_pos = int * int type span = line_pos * line_pos type color = | Red | Green *)

(* let textarea : contents:string -> inject:(TextareaAction.t -> Vdom.Event.t) ->
   result:Vdom.Node.t option -> Vdom.Node.t = fun ~contents ~inject ~result -> let div,
   text, textarea = Vdom.Node.(div, text, textarea) in let class_, string_property =
   Vdom.Attr.(class_, string_property) in let result_elem = match result with | Some node
   -> node (*(match highlight with | None -> [] | Some ((span_start, span_end), color) ->
   let color_name = match color with | Red -> "red" | Green -> "green" in [
   Vdom.Node.create "mark" [ style Css_gen.(concat [ margin_left (`Ch (float_of_int
   span_start)) ; background_color (`Name color_name) ; width (`Ch (float_of_int (span_end
   - span_start))) ]) ] [] ] ), node *) | None -> text "(press (ctrl/shift/meta)-enter to
   evaluate)" in

   let handle_keydown = Vdom.Attr.on_keydown (fun evt -> match str_of_textarea_evt evt
   with | Some str -> let update = if is_special_enter evt then ( Dom.preventDefault evt;
   (* prevent inserting a newline *) TextareaAction.Evaluate str) else UpdateInput str in
   inject update | None -> Vdom.Event.Ignore) in

   div [class_ "container"] (* [ div [class_ "backdrop"] [ div [class_ "highlights"]
   highlight_elems ] ; div [] *) [ textarea [ string_property "rows" "25"; string_property
   "cols" "90"; handle_keydown ] [ text contents ] ; div [] [ result_elem ] ] (* ] *)

   type span = { start : int ; length : int }

   let input : contents:string -> highlight:(span option) -> inject:(TextareaAction.t ->
   Vdom.Event.t) -> result:(Vdom.Node.t option) -> Vdom.Node.t = fun ~contents ~highlight
   ~inject ~result -> let div, input, text = Vdom.Node.(div, input, text) in let class_,
   on_input, on_keydown, style, value = Vdom.Attr.(class_, on_input, on_keydown, style,
   value) in

   let handle_input = on_input (fun _evt str -> inject (UpdateInput str)) in

   (* This would be a lot easier, but on_change apparently doesn't fire if the * contents
   haven't changed from the original value, but we want to * evaluate in that case. *)

   (* let handle_change = on_change (fun _evt str -> inject (Evaluate str)) in *) let
   handle_change = on_keydown (fun evt -> match str_of_input_evt evt with | Some str ->
   let update = if is_enter evt then TextareaAction.Evaluate str else UpdateInput str in
   inject update | None -> Vdom.Event.Ignore) in

   let input_style = style (Css_gen.(concat [ width (`Ch 80.0) ; font_family ["monospace"]
   ])) in

   let highlight_style = style (match highlight with | None -> Css_gen.empty | Some {
   start; length } -> Css_gen.(concat [ position `Absolute ; left (`Ch (float_of_int
   start)) ; width (`Ch (float_of_int length)) ; height (`Px 2) ; color (`Name "green") ;
   background_color (`Name "red") ])) in

   let result' = match result with | None -> text "(press enter to evaluate)" | Some node
   -> node in

   div [class_ "container"] [ input [ handle_input; handle_change; value contents;
   input_style ] [] ; div [ highlight_style ] [] ; div [ style (Css_gen.font_family
   ["monospace"]) ] [ result' ] ] *)
