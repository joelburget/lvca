open Base
open Js_of_ocaml
open Lvca

module Language = struct
  module PT = Parsing.NonBindingTerm

  let language = {|
  import {string} from "builtin";

  float :=
    | lit(string())
    | add(float(); float())
    | sub(float(); float())
    | mul(float(); float())
    | div(float(); float())

  p2 := p2(float(); float())
  size2 := size2(float(); float())
  box2 := box2(p2(); size2())

  path :=
    // subpaths and segments
    | empty()
    | sub(p2(); path())
    | line(p2(); path())
    | qcurve(p2(); p2(); path())
    | ccurve(p2(); p2(); p2(); path())
    | earc(size2(); p2(); path())
    | close(path())

    // derived paths
    | circle(p2(); float(); path())
    | ellipse(p2(); size2(); path())
    | rect(box2(); path())
    | rrect(box2(); size2(); path())

  image :=
    // primitive images
    | const(color())
    // TODO: axial, radial

    // cutting images
    | cut(path(); image()) // TODO: area

    // blending images
    | blend(image(); image())

    // transforming images
    | move(v2(); image())
    | rot(float(); image())
    | scale(v2(); image())
    // TODO: transform

  |}

  let parse : string -> PT.parse_result
    = PT.parse

  let render : Dom_html.canvasElement Js.t -> NonBinding.term -> unit
    = failwith "TODO"
end

module Model = struct
  type t =
    { input : string
    ; result : Parsing.NonBindingTerm.parse_result option
    }

  let initial_model : t =
    let input =
      (* TODO:
        modules: everything is crammed in one namespace
        float / int literals
        optionals / named arguments
        *)
      {|
        diagram(
          p_empty();
          p_circle(p2(lit("0.5"); lit("0.5")); lit("0.4"))
        );
      |}
    in { input; result = None }
end

let image_of_model : Model.t -> Vg.image
  = failwith "TODO"

module Action = struct
  type t =
    | UpdateInput of string
    | Evaluate of string
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit
type react_pair = signal * update_fun

module Controller = struct
  let update (action : Action.t) ((r, f) : react_pair) =
    let open Model in
    let { input; result } = React.S.value r in
    let new_model = match action with
      | Action.UpdateInput str -> { input = str; result }
      | Action.Evaluate str -> { input = str; result = Some (Parsing.NonBindingTerm.parse input) }
    in
    f new_model
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.(async @@ (fun () -> ev elem handler))

  let mk_input ((r, _f) as react_pair : react_pair) =
    let input = r
      |> React.S.map (fun m -> m.Model.input)
      |> fun (value : string React.signal) -> R.Html5.(textarea
        ~a:[
          a_rows (React.S.const 25);
          a_cols (React.S.const 90);
        ]
        (React.S.const (txt value)))
    in
    let input_dom = To_dom.of_textarea input in

    bind_event Ev.keydowns input_dom (fun evt ->
      let key = evt##.key |> Js.Optdef.to_option |> Option.map ~f:Js.to_string in
      Lwt.return (match key with
        (* TODO: require special key *)
        | Some "Enter"
        -> Controller.update (Evaluate (Js.to_string input_dom##.value)) react_pair
        | _ -> ())
      );

    input

  let mk_viewport signal =
    let open Gg in
    let open Vg in
    let canvas = Html5.canvas [] in
    let canvas_dom = To_dom.of_canvas canvas in
    let r = Vgr.create (Vgr_htmlc.target canvas_dom) `Other in
    let size = Size2.v 100. 100. in (* TODO *)
    let view = Box2.v P2.o (Size2.v 1. 1.) in (* TODO *)
    let _ : unit React.signal = signal
      |> React.S.map (fun img ->
        let _ : [ `Ok | `Partial ] = Vgr.render r (`Image (size, view, img)) in
        let _ : [ `Ok | `Partial ] = Vgr.render r `End in
        ())
    in
    canvas

  let info ((r, _f) : react_pair) = r
    |> React.S.map (fun m -> match m.Model.result with
      | None -> "(press (ctrl/shift/meta)-enter to evaluate)"
      | Some tm_result -> (match tm_result with
        | Ok _tm -> "TODO (render diagram)"
        | Error err -> ParseError.to_string err
      )
    )
    |> fun msg -> Html5.(div [ R.Html5.txt msg ])

  let view ((signal, _update_fun) as react_pair : react_pair) = Html5.(div
    [ mk_input react_pair
    ; mk_viewport (signal |> React.S.map image_of_model)
    ; info react_pair
    ])
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app"))
      (fun () -> assert false)
  in
  let m = Model.initial_model in
  let react_pair = React.S.create m in
  Dom.appendChild parent
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view react_pair));
  Lwt.return ()

let (_ : unit Lwt.t) =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main
