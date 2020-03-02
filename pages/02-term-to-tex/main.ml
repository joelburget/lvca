open Bonsai_web
open Core_kernel
open Js_of_ocaml
open Lvca
open Vdom

let is_key_ret key =
  let is_enter =
    String.equal
      "Enter"
      (key##.code |> Js.Optdef.to_option |> Option.value_exn |> Js.to_string)
  in
  let is_meta = Js.to_bool key##.metaKey in
  let is_shift = Js.to_bool key##.shiftKey in
  let is_ctrl = Js.to_bool key##.ctrlKey in
  is_enter && (is_meta || is_shift || is_ctrl)
;;

(* module Katex_component = struct let name = "KaTeX" module Input = String module Result
   = Node module Model = Unit module Action = Unit

   let id = Type_equal.Id.create ~name:"katex-elem-id" Sexplib.Conv.sexp_of_opaque

   let apply_action ~inject:_ ~schedule_event:_ _input () () = ()

   let compute ~inject:_ input_str () = Node.widget ~update:(fun () dom_elem ->
   Katex.render dom_elem input_str, dom_elem) ~init:(fun () -> (), Dom_html.createDiv
   Dom_html.document) ~id () end

   let katex_component = Bonsai.of_module (module Katex_component) *)

let katex_elem_id = Type_equal.Id.create ~name:"katex-elem-id" Sexplib.Conv.sexp_of_opaque

let render_katex str =
  Node.widget
    ~update:(fun () dom_elem -> Katex.render dom_elem str, dom_elem)
    ~init:(fun () -> (), Dom_html.createDiv Dom_html.document)
    ~id:katex_elem_id
    ()
;;

module Term_render_component = struct
  let name = "Term to TeX"

  module Input = Unit
  module Result = Node
  module Model = String

  module Action = struct
    type t = Evaluate of string [@@deriving sexp]
  end

  module P_term = Parsing.Incremental (Parsing.Parseable_term)

  let apply_action ~inject:_ ~schedule_event:_ () _model (Action.Evaluate str) = str

  let compute : inject:(Action.t -> Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject () model ->
    let handle_keydown =
      Attr.on_keydown (fun evt ->
          (let open Option.Let_syntax in
          let%bind target = Js.Opt.to_option evt##.target in
          let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.textarea target) in
          let str = Js.to_string inp##.value in
          if is_key_ret evt
          then (
            Dom.preventDefault evt;
            (* prevent inserting a newline *)
            inject (Action.Evaluate str))
          else Event.Ignore)
          |> Option.value ~default:Event.Ignore)
    in
    let mk_textarea input_str =
      Node.(
        textarea
          Attr.
            [ string_property "rows" "25"; string_property "cols" "90"; handle_keydown ]
          [ text input_str ])
    in
    Node.div [] [ mk_textarea model; render_katex {|\overbrace{a+b+c}^{\text{note}}|} ]
 ;;
end

let component = Bonsai.of_module (module Term_render_component)

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:{|exp("a"; "2")|}
    ~bind_to_element_with_id:"app"
    component
;;
