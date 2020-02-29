open Bonsai_web
open Core_kernel
open Js_of_ocaml
open Lvca

module Term_render_component = struct
  let name = "Term Render"
  module Input = struct
    type t = Binding.Nominal.term -> (Vdom.Node.t, string) Core_kernel.Result.t
  end
  module Result = Vdom.Node
  module Model = struct
    type t = string * (Binding.Nominal.term, string) Core_kernel.Result.t option
  end
  module Action = struct
    type t =
      | UpdateInput of string
      | Evaluate of string
      [@@deriving sexp]
  end

  module P_term = Parsing.Incremental(Parsing.Parseable_term)

  let apply_action ~inject:_ ~schedule_event:_ _input (input, tm_opt)
    = function
    | Action.UpdateInput str -> str, tm_opt
    | Action.Evaluate str    -> str, Some (P_term.parse input)

  let compute
    : inject:(Action.t -> Vdom.Event.t) -> Input.t -> Model.t -> Result.t
    = fun ~inject eval (input_str, tm_opt) ->

    let is_key_ret key =
      let is_enter = String.equal "Enter"
        (key##.code
         |> Js.Optdef.to_option
         |> Option.value_exn
         |> Js.to_string)
      in
      let is_meta = Js.to_bool key##.metaKey in
      let is_shift = Js.to_bool key##.shiftKey in
      let is_ctrl = Js.to_bool key##.ctrlKey in
      is_enter && (is_meta || is_shift || is_ctrl)
    in

    let handle_keydown = Vdom.Attr.on_keydown (fun evt ->
      (let open Option.Let_syntax in
       let%bind target = Js.Opt.to_option evt##.target in
       let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.textarea target) in
       let str = Js.to_string inp##.value in

       let update =
         if is_key_ret evt
         then (
           Dom.preventDefault evt;  (* prevent inserting a newline *)
           Action.Evaluate str
         )
         else UpdateInput str
       in

       inject update)
      |> Option.value ~default:Vdom.Event.Ignore
    )
    in

    Vdom.Node.(
      div
       []
       [ textarea (Vdom.Attr.(
           [ string_property "rows" "25";
             string_property "cols" "90";
             handle_keydown;
           ]))
           [ text input_str ];
         div []
           [ match tm_opt with
               | None -> text "(press (ctrl/shift/meta)-enter to evaluate)"
               | Some tm_result
               -> (match Core_kernel.Result.bind tm_result ~f:eval with
                 | Error msg -> text msg
                 | Ok node -> node)
           ];
       ])

end

let component = Bonsai.of_module (module Term_render_component)

let initial_input = {|document([
  header(h2(); "some document"),
  paragraph(inline([inlineAtom("attrs"; "body text")]))
])|}

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:Lvca_web.Render_doc_term.render
    ~initial_model:(initial_input, None)
    ~bind_to_element_with_id:"app"
    component
;;
