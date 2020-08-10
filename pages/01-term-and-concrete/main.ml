open Base
open Js_of_ocaml
open Lvca_syntax

type term = Range.t Binding.Nominal.term
type side = Left | Right

module Model = struct
  type t =
    { left_input : string
    ; result : ((term, string) Result.t * side) option
    ; right_input : string
    }

  let initial_model : t =
    let left_input = {|\f -> \g -> \x -> f (g x)|} in
    let right_input = "" in
    { left_input; result = None; right_input }
end

module Action = struct
  type t =
    (* | Update of side * string *)
    | Evaluate of side * string
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit
type react_pair = signal * update_fun

module TermParse = Binding.Nominal.Parse(ParseUtil.Angstrom.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse(ParseUtil.Angstrom.NoComment)

module Controller = struct
  let update (action : Action.t) ((r, f) : react_pair) =
    let open Model in
    let term_pretty = Binding.Nominal.pp_term in
    let lambda_pretty = Lvca_languages.LambdaCalculus.pp in

    let { left_input; result = _; right_input } = React.S.value r in
    let new_model = match action with
      (*
      | Action.Update (side, str) ->
        begin
          match side with
            | Left -> { left_input = str; result; right_input }
            | Right -> { left_input; result; right_input = str }
        end
      *)
      | Action.Evaluate (side, str) ->
        begin
          let parser = match side with
            | Left -> LambdaParse.t
            | Right -> TermParse.t
          in
          let parsed = Angstrom.parse_string ~consume:All parser str in
          let result = Some (parsed, side) in
          let other_side_str = match parsed, side with
            | Ok tm, Left -> Fmt.str "%a" term_pretty tm
            | Ok tm, Right -> Fmt.str "%a" lambda_pretty tm
            | Error _, Left -> right_input
            | Error _, Right -> left_input
          in
          match side with
            | Left -> { left_input = str; result; right_input = other_side_str }
            | Right -> { left_input = other_side_str; result; right_input = str }
        end
    in
    f new_model
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.async @@ (fun () -> ev elem handler)

  let mk_input side ((r, _f) as react_pair : react_pair) =
    let input = r
      |> React.S.map (fun m -> match side with
        | Left -> m.Model.left_input
        | Right -> m.Model.right_input)
      |> fun (value : string React.signal) -> R.Html5.(textarea
        ~a:[
          a_rows (React.S.const 25);
          a_cols (React.S.const 80);
        ]
        (React.S.const (txt value)))
    in
    let input_dom = To_dom.of_textarea input in

    bind_event Ev.keydowns input_dom (fun evt ->
      let key = evt##.key |> Js.Optdef.to_option |> Option.map ~f:Js.to_string in
      Lwt.return (match key with
        (* TODO: require special key *)
        | Some "Enter"
        -> Controller.update (Evaluate (side, Js.to_string input_dom##.value)) react_pair
        | _ -> ())
      );

    input

  let info ((r, _f) : react_pair) = r
    |> React.S.map (fun m -> match m.Model.result with
      | None -> "(press (ctrl/shift/meta)-enter to evaluate)"
      | Some (tm_result, _side) -> (match tm_result with
        | Ok _tm -> "(parsed)"
        | Error err -> err
      )
    )
    |> fun msg -> Html5.div [ R.Html5.txt msg ]

  let view (react_pair : react_pair) = Html5.(div
    [ mk_input Left react_pair
    ; mk_input Right react_pair
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

let (_ : unit Lwt.t) = Lwt.Infix.(Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main)
