open Base
open Js_of_ocaml
open Lvca

type eval_result =
  { str_term : string
  ; result : (Binding.Nominal.term, string (* Core.Types.eval_error *)) Result.t
  }

let eval : string -> eval_result =
 fun str_term ->
  let result =
    match Parsing.CoreTerm.parse str_term with
    | Error err -> Error (ParseError.to_string err)
    | Ok tm -> Core.Types.eval tm |> Result.map_error ~f:(fun (str, _tm) -> str)
    (* | Error (msg, _tm) -> msg | Ok tm' -> Binding.Nominal.pp_term_str tm') *)
  in
  { str_term; result }
;;

module Model = struct
  type t =
    { before_history : eval_result list
    ; field : string
    ; after_history : eval_result list
    }

  let initial_model =
    { before_history = []; field = {|(\(x : ty())) term()|}; after_history = [] }
  ;;
end

module Action = struct
  type action =
    | UpdateField of string
    | Evaluate of string
    | Up
    | Down
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit
type react_pair = signal * update_fun

module Controller = struct
  let update (action : Action.action) ((r, f) : react_pair) =
    let open Model in
    let ({ before_history; field; after_history } as model) = React.S.value r in
    let m' =
      match action with
      | UpdateField field -> { model with field }
      | Evaluate "" -> model
      | Evaluate str ->
        (match after_history with
        | [] -> { before_history = eval str :: before_history; field = ""; after_history }
        | history_item :: after_history ->
          { before_history = eval str :: before_history
          ; field = history_item.str_term
          ; after_history
          })
      | Up ->
        (match before_history with
        | [] -> model
        | history_item :: before_history ->
          { before_history
          ; field = history_item.str_term
          ; after_history =
              (match field with "" -> after_history | _ -> eval field :: after_history)
          })
      | Down ->
        (match after_history, field with
        | [], "" -> model
        | [], _ ->
          { before_history = eval field :: before_history
          ; field = ""
          ; after_history = []
          }
        | history_item :: after_history, _ ->
          { before_history =
              (match field with
              | "" -> before_history
              | _ -> eval field :: before_history)
          ; field = history_item.str_term
          ; after_history
          })
    in
    f m'
  ;;
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.(async @@ fun () -> ev elem handler)
  ;;

  let mk_input ((r, f) as react_pair : react_pair) =
    let input =
      r
      |> React.S.map (fun m -> m.Model.field)
      |> fun value ->
      R.Html5.(input ~a:[ a_input_type (React.S.const `Text); a_value value ] ())
    in
    let input_dom = To_dom.of_input input in
    bind_event Ev.keydowns input_dom (fun evt ->
        let key = evt##.key |> Js.Optdef.to_option |> Option.map ~f:Js.to_string in
        Lwt.return
          (match key with
          | Some "Enter" ->
            Controller.update (Evaluate (Js.to_string input_dom##.value)) react_pair
          | Some "ArrowUp" -> Controller.update Up react_pair
          | Some "ArrowDown" -> Controller.update Down react_pair
          | _ -> ()));
    bind_event Ev.inputs input_dom (fun _ ->
        Lwt.return
          (Controller.update (UpdateField (Js.to_string input_dom##.value)) (r, f)));
    input
  ;;

  let history_item { str_term; result } =
    let msg =
      match result with Ok tm -> Binding.Nominal.pp_term_str tm | Error msg -> msg
    in
    Html5.(li [ div [ txt str_term ]; div [ txt msg ] ])
  ;;

  let before_history ((r, _f) : react_pair) =
    r
    |> React.S.map (fun m -> m.Model.before_history)
    |> ReactiveData.RList.from_signal
    |> ReactiveData.RList.map history_item
    |> ReactiveData.RList.rev
    |> R.Html5.ul
  ;;

  let after_history ((r, _f) : react_pair) =
    r
    |> React.S.map (fun m -> m.Model.after_history)
    |> ReactiveData.RList.from_signal
    |> ReactiveData.RList.map history_item
    |> R.Html5.ul
  ;;

  let view (react_pair : react_pair) =
    Html5.(
      div [ before_history react_pair; mk_input react_pair; after_history react_pair ])
  ;;
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app")) (fun () -> assert false)
  in
  let m = Model.initial_model in
  let react_pair = React.S.create m in
  Dom.appendChild parent (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view react_pair));
  Lwt.return ()
;;

let (_ : unit Lwt.t) =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main
;;
