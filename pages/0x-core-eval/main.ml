open Base
open Js_of_ocaml
(* open Lvca_web *)
(* open Lvca *)

module Model = String

module Action = struct
  type action = Evaluate of string
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit
type react_pair = signal * update_fun

module Controller = struct
  let update (action : Action.action) (_ : react_pair) = match action with
    | Evaluate str -> str
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.(async @@ (fun () -> ev elem handler))

  let iput = Html5.(input ~a:[a_input_type `Text] ())

  let input_dom =
    To_dom.of_input iput

  let set_input v =
    input_dom##.value := Js.string v

  let task_entry ((r, f) : react_pair) =
    bind_event Ev.keypresses input_dom (fun evt ->
        Lwt.return @@
        if evt##.keyCode = 13 then (
          Controller.update Add (r, f) ;
          set_input ""
        )
      ) ;

  let view ((_r, _) : react_pair) = Html5.(div
    [ iput
    (* (R.Html5.txt (React.S.map Fn.id r)) *)
    ])
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app"))
      (fun () -> assert false)
  in
  (* let m = Model.initial_value in *)
  let m = {|(\(x : ty())) term()|} in
  let react_pair = React.S.create m in
  Dom.appendChild parent (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view react_pair)) ;
  Lwt.return ()

let (_ : unit Lwt.t) =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main

(*
module Term_render_component = struct
  let name = "Evaluate Core"

  module Input = Unit
  module Result = Node

  let apply_action ~inject:_ ~schedule_event:_ () _model (Action.Evaluate str) = str

  let compute : inject:(Action.t -> Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject () model ->
    let handle_keydown =
      Attr.on_keydown (fun evt ->
          (let open Option.Let_syntax in
          let%bind target = Js.Opt.to_option evt##.target in
          let%map inp = Js.Opt.to_option (Dom_html.CoerceTo.textarea target) in
          let str = Js.to_string inp##.value in
          if Web_util.is_special_enter evt
          then (
            Dom.preventDefault evt;
            (* prevent inserting a newline *)
            inject (Action.Evaluate str))
          else Event.Ignore)
          |> Option.value ~default:Event.Ignore)
    in

    let result = Node.text (match Parsing.CoreTerm.parse model with
      | Error err -> ParseError.to_string err
      | Ok tm -> (match Core.Types.eval tm with
        | Error (msg, _tm) -> msg
        | Ok tm' -> Binding.Nominal.pp_term_str tm'))
    in

    Node.div []
      [ mk_textarea handle_keydown model
      ; result
      ]
 ;;
end

let component = Bonsai.of_module (module Term_render_component)

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:{|(\(x : ty())) term()|}
    ~bind_to_element_with_id:"app"
    component
;;
*)
