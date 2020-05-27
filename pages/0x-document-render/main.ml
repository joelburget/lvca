open Base
open Js_of_ocaml
open Lvca

module Model = struct
  type t =
    { input : string
    ; result : (Binding.Nominal.term, ParseError.t) Core_kernel.Result.t option
    }

  let initial_model : t =
    let input =
      {|document(
        header(h2(); "some document"),
        paragraph(inline(inlineAtom("attrs"; "body text")))
      )|}
    in { input; result = None }
end

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
      | Action.Evaluate str -> { input = str; result = Some (Parsing.Term.parse input) }
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

  let info ((r, _f) : react_pair) = r
    |> React.S.map (fun m -> match m.Model.result with
      | None -> "(press (ctrl/shift/meta)-enter to evaluate)"
      | Some tm_result -> (match tm_result with
        | Ok _tm -> "TODO (render document)"
        | Error err -> ParseError.to_string err
      )
    )
    |> fun msg -> Html5.(div [ R.Html5.txt msg ])

  let view (react_pair : react_pair) = Html5.(div
    [ mk_input react_pair
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
