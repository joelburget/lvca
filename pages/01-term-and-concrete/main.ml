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

module Term_render_component = struct
  let name = "Term and Concrete"

  type side =
    | Left
    | Right
  [@@deriving sexp]

  module Input = Unit
  module Result = Node

  module Model = struct
    type t =
      | NoInput
      | Produced of side * string * (Binding.Nominal.term, string) Core_kernel.Result.t
  end

  module Action = struct
    type t =
      | UpdateInput of side * string
      | Evaluate of side * string
    [@@deriving sexp]
  end

  module P_term = Parsing.Incremental (Parsing.Parseable_term)

  let apply_action ~inject:_ ~schedule_event:_ () _model action =
    match action with
    | Action.UpdateInput (side, str) -> Model.Produced (side, str, Error "...")
    | Evaluate (side, str) -> Produced (side, str, P_term.parse str)
  ;;

  let compute : inject:(Action.t -> Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject () model ->
    let handle_keydown side =
      Attr.on_keydown (fun evt ->
          (let open Option.Let_syntax in
          let%bind target = Js.Opt.to_option evt##.target in
          let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.textarea target) in
          let str = Js.to_string inp##.value in
          let update =
            if is_key_ret evt
            then (
              Dom.preventDefault evt;
              (* prevent inserting a newline *)
              Action.Evaluate (side, str))
            else UpdateInput (side, str)
          in
          inject update)
          |> Option.value ~default:Event.Ignore)
    in
    let mk_textarea side input_str =
      Node.(
        textarea
          Attr.
            [ string_property "rows" "25"
            ; string_property "cols" "90"
            ; handle_keydown side
            ]
          [ text input_str ])
    in
    let left_pane, right_pane =
      match model with
      | NoInput -> mk_textarea Left "type a term", mk_textarea Right "type a term"
      | Produced (side, input_str, result) ->
        let other_side =
          match result with
          | Error msg -> Node.(div [] [ text msg ])
          | Ok tm -> Node.(div [] [ text (Binding.Nominal.pp_term' tm) ])
        in
        (match side with
        | Left -> mk_textarea Left input_str, other_side
        | Right -> other_side, mk_textarea Right input_str)
    in
    Node.div [] [ left_pane; right_pane ]
 ;;
end

let component = Bonsai.of_module (module Term_render_component)

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:Term_render_component.Model.NoInput
    ~bind_to_element_with_id:"app"
    component
;;
