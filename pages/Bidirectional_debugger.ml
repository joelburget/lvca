open Base
open Brr
open Brr_note
open Note
open Lvca_syntax
open Lvca_bidirectional
open Lvca_util

let ( a
    , button
    , div
    , span
    , h3
    , h4
    , input
    , li
    , tbody
    , table
    , textarea
    , th
    , thead
    , td
    , txt'
    , tr
    , ul )
  =
  El.(
    ( a
    , button
    , div
    , span
    , h3
    , h4
    , input
    , li
    , tbody
    , table
    , textarea
    , th
    , thead
    , td
    , txt'
    , tr
    , ul ))
;;

let reserved = String.Set.empty
let html_eq = Common.html_eq
let class', classes, mk_reactive', type' = Prelude.(class', classes, mk_reactive', type')
let string_of_term tm = Fmt.to_to_string Nominal.Term.pp tm

let elem_of_env Bidirectional.Env.{ var_types; _ } =
  let vars =
    var_types
    |> Map.to_alist
    |> List.map ~f:(fun (name, ty) -> li [ txt' name; txt' (string_of_term ty) ])
  in
  match vars with
  | [] -> h4 [ txt' "(empty environment)" ]
  | _ -> div [ h4 [ txt' "environment" ]; ul vars ]
;;

let string_of_typing (Statics.Typing.Typing (tm, ty)) =
  Fmt.str "%a: %a" Nominal.Term.pp tm Nominal.Term.pp ty
;;

let elem_of_trace_entry = function
  | Bidirectional.Trace_entry.Check_trace (env, typing) ->
    div [ h3 [ txt' "check" ]; elem_of_env env; txt' (string_of_typing typing) ]
  | Infer_trace (env, term) ->
    div [ h3 [ txt' "infer" ]; elem_of_env env; txt' (string_of_term term) ]
  | Check_success -> div [ h3 [ txt' "check success" ] ]
  | Check_failure _ -> div [ h3 [ txt' "check failure" ] ]
  | Inferred ty -> div [ h3 [ txt' "inferred" ]; txt' (string_of_term ty) ]
;;

let elem_of_current_stack current_stack =
  let last_row, stack =
    match current_stack with
    | Bidirectional.Trace_entry.Inferred ty :: entry :: current_stack' ->
      let row =
        tr
          [ td [ elem_of_trace_entry entry ]
          ; td ~at:[ class' "text-green" ] [ txt' ("inferred " ^ string_of_term ty) ]
          ]
      in
      [ row ], current_stack'
    | Check_success :: entry :: current_stack' ->
      let row =
        tr
          [ td [ elem_of_trace_entry entry ]
          ; td ~at:[ class' "text-green" ] [ txt' "success" ]
          ]
      in
      [ row ], current_stack'
    | Check_failure _msg :: entry :: current_stack' ->
      let row =
        tr
          [ td [ elem_of_trace_entry entry ]
          ; td ~at:[ class' "text-red" ] [ txt' "failure: TODO: message" ]
          ]
      in
      [ row ], current_stack'
    | _ -> [], current_stack
  in
  let other_rows =
    stack
    |> List.map ~f:(fun trace_entry ->
           tr [ td [ elem_of_trace_entry trace_entry ]; td [] ])
    |> List.rev
  in
  tbody (other_rows @ last_row)
;;

module Term_render_component = struct
  let name = "Term Render"

  module Input = struct
    type t = Nominal.Term.t -> (El.t, string) Result.t
  end

  module Model = struct
    module Debugger_state = struct
      type t =
        { parsed : Nominal.Term.t
        ; steps : Bidirectional.Trace_step.t list
        ; current_step : int
        }

      let ( = ) t1 t2 =
        Nominal.Term.(t1.parsed = t2.parsed)
        && List.equal Bidirectional.Trace_step.( = ) t1.steps t2.steps
        && Int.(t1.current_step = t2.current_step)
      ;;
    end

    type t =
      { abstract_expanded : bool
      ; abstract_input : string
      ; abstract_parsed : (Abstract_syntax.t, string) Result.t option
      ; statics_expanded : bool
      ; statics_input : string
      ; statics_parsed : (Statics.Rule.t list, string) Result.t option
      ; debugger_expanded : bool
      ; debugger_input : string
      ; debugger_state : (Debugger_state.t, string) Result.t option
      }

    let abstract_valid state =
      match state.abstract_parsed with Some (Ok _) -> true | _ -> false
    ;;

    let statics_valid state =
      match state.statics_parsed with Some (Ok _) -> true | _ -> false
    ;;

    let ( = ) t1 t2 =
      Bool.(t1.abstract_expanded = t2.abstract_expanded)
      && String.(t1.abstract_input = t2.abstract_input)
      && Option.equal
           (Result.equal Abstract_syntax.( = ) String.( = ))
           t1.abstract_parsed
           t2.abstract_parsed
      && Bool.(t1.statics_expanded = t2.statics_expanded)
      && String.(t1.statics_input = t2.statics_input)
      && Option.equal
           (Result.equal (List.equal Statics.Rule.( = )) String.( = ))
           t1.statics_parsed
           t2.statics_parsed
      && Bool.(t1.debugger_expanded = t2.debugger_expanded)
      && Option.equal
           (Result.equal Debugger_state.( = ) String.( = ))
           t1.debugger_state
           t2.debugger_state
    ;;

    let initial_model =
      { abstract_expanded = true
      ; abstract_input = "bool := True() | False()"
      ; abstract_parsed = None
      ; statics_expanded = false
      ; statics_input =
          {|
---
  ctx >> True() => bool()

---
  ctx >> False() => bool()
          |}
      ; statics_parsed = None
      ; debugger_expanded = false
      ; debugger_input = "True()"
      ; debugger_state = None
      }
    ;;
  end

  module Action = struct
    type t =
      | Abstract_update of string
      | Statics_update of string
      | Evaluate of string
      | Step_forward
      | Step_backward
      | Toggle_abstract
      | Toggle_statics
      | Toggle_debugger
  end

  module Controller = struct
    let update (action : Action.t) model =
      match action with
      | Action.Abstract_update abstract_input ->
        let abstract_result =
          Lvca_parsing.(parse_string (whitespace *> Abstract_syntax.parse) abstract_input)
        in
        Model.
          { model with
            abstract_parsed = Some abstract_result
          ; statics_expanded = Result.is_ok abstract_result
          }
      | Statics_update statics_input ->
        let statics_result =
          Lvca_parsing.(parse_string (whitespace *> Statics.parse) statics_input)
        in
        { model with
          statics_parsed = Some statics_result
        ; debugger_expanded = Result.is_ok statics_result
        }
      | (Step_forward | Step_backward) as step ->
        let debugger_state =
          match model.debugger_state with
          | None -> None
          | Some (Ok state) ->
            Some
              (Ok
                 (match step with
                 | Step_forward -> { state with current_step = state.current_step + 1 }
                 | _ -> { state with current_step = state.current_step - 1 }))
          | Some (Error _) -> model.debugger_state
        in
        { model with debugger_state }
      | Toggle_abstract -> { model with abstract_expanded = not model.abstract_expanded }
      | Toggle_statics ->
        { model with
          statics_expanded = Model.abstract_valid model && not model.statics_expanded
        }
      | Toggle_debugger ->
        { model with
          debugger_expanded =
            Model.abstract_valid model
            && Model.statics_valid model
            && not model.debugger_expanded
        }
      | Evaluate input ->
        Fmt.pr "evaluating `%s`\n" input;
        (match model.statics_parsed with
        | Some (Ok rules) ->
          let steps = Queue.create () in
          let handle_trace = Queue.enqueue steps in
          let env = Bidirectional.Env.{ rules; var_types = String.Map.empty } in
          let debugger_state =
            let parsed =
              Lvca_parsing.(parse_string (Nominal.Term.parse' reserved) input)
            in
            match parsed with
            | Error msg ->
              Fmt.pr "failed to parse: %s\n" msg;
              Some (Error msg)
            | Ok tm ->
              Fmt.pr "parsed: %a\n" Nominal.Term.pp tm;
              let (_ : (Nominal.Term.t, Bidirectional.Check_error.t) Result.t) =
                Bidirectional.infer_trace handle_trace env tm
              in
              Some
                (Ok
                   Model.Debugger_state.
                     { parsed = tm; steps = Queue.to_list steps; current_step = 0 })
          in
          { model with debugger_input = input; debugger_state }
        | _ -> model)
    ;;
  end

  module View = struct
    open Action

    let mk_section name enabled expanded toggle_evt contents =
      let at =
        List.map
          ~f:class'
          ("section-box"
          :: (if enabled then [] else [ "text-gray-500"; "cursor-not-allowed" ]))
      in
      let link = button ~at [ txt' (if expanded then "- " ^ name else "+ " ^ name) ] in
      let evt = Evr.on_el Ev.click (fun _ -> toggle_evt) link in
      let header = h3 [ link ] in
      let children = if expanded then [ header; contents ] else [ header ] in
      evt, div children
    ;;

    let view model_s =
      let eq (evt1, el1) (evt2, el2) = phys_equal evt1 evt2 && html_eq el1 el2 in
      let abstract, abstract_input_event =
        let editor, input_evt =
          Multiline_input.mk
            ~autofocus:true
            (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.abstract_input))
        in
        let feedback =
          model_s
          |> S.map ~eq:html_eq (fun model ->
                 match model.Model.abstract_parsed with
                 | None -> span []
                 | Some (Ok _) -> span [ txt' "okay" ]
                 | Some (Error msg) -> span [ txt' msg ])
          |> mk_reactive' div
        in
        let input_evt =
          input_evt
          |> E.filter_map (function
                 | Common.Evaluate_input str -> Some (Abstract_update str)
                 | _ -> None)
        in
        let elem =
          model_s
          |> S.map ~eq (fun state ->
                 mk_section
                   "Abstract"
                   true
                   state.Model.abstract_expanded
                   Toggle_abstract
                   (div [ editor; feedback ]))
        in
        elem, input_evt
      in
      let statics, statics_input_event =
        let editor, input_evt =
          Multiline_input.mk
            (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.statics_input))
        in
        let feedback =
          model_s
          |> S.map ~eq:html_eq (fun model ->
                 match model.Model.statics_parsed with
                 | None -> span []
                 | Some (Ok _) -> span [ txt' "okay" ]
                 | Some (Error msg) -> span [ txt' msg ])
          |> mk_reactive' div
        in
        let input_evt =
          input_evt
          |> E.filter_map (function
                 | Common.Evaluate_input str -> Some (Statics_update str)
                 | _ -> None)
        in
        let elem =
          model_s
          |> S.map ~eq (fun state ->
                 mk_section
                   "Statics"
                   (Model.abstract_valid state)
                   state.Model.statics_expanded
                   Toggle_statics
                   (div [ editor; feedback ]))
        in
        elem, input_evt
      in
      let debugger =
        let editor, input_evt =
          Multiline_input.mk
            (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.debugger_input))
        in
        let input_evt =
          input_evt
          |> E.filter_map (function
                 | Common.Evaluate_input str -> Some (Evaluate str)
                 | _ -> None)
        in
        model_s
        |> S.map ~eq (fun state ->
               let debugger_evts, debugger_results =
                 match state.Model.debugger_state with
                 | None -> E.never, span []
                 | Some (Error msg) -> E.never, span [ txt' msg ]
                 | Some (Ok Model.Debugger_state.{ steps; current_step; parsed = _ }) ->
                   let step_count = List.length steps in
                   let trace_step = List.nth_exn steps current_step in
                   let elem = elem_of_current_stack trace_step in
                   let back_evt, back_button =
                     if Int.(current_step = 0)
                     then
                       ( E.never
                       , button
                           ~at:(classes "text-gray-500 cursor-not-allowed")
                           [ txt' "previous step" ] )
                     else (
                       let elem = button [ txt' "previous step" ] in
                       let evt = Evr.on_el Ev.click (fun _evt -> Step_backward) elem in
                       evt, elem)
                   in
                   let forward_evt, forward_button =
                     if Int.(current_step = step_count - 1)
                     then
                       ( E.never
                       , button
                           ~at:(classes "text-gray-500 cursor-not-allowed")
                           [ txt' "next step" ] )
                     else (
                       let elem = button [ txt' "next step" ] in
                       let evt = Evr.on_el Ev.click (fun _evt -> Step_forward) elem in
                       evt, elem)
                   in
                   let elem =
                     div
                       [ div [ txt' ("Inferring type of " ^ state.Model.debugger_input) ]
                       ; div [ txt' (Fmt.str "%n / %n" (current_step + 1) step_count) ]
                       ; back_button
                       ; forward_button
                       ; table
                           [ thead [ tr [ th [ txt' "action" ]; th [ txt' "result" ] ] ]
                           ; elem
                           ]
                       ]
                   in
                   E.select [ back_evt; forward_evt ], elem
               in
               let contents = div [ span [ txt' "input:" ]; editor; debugger_results ] in
               let evt, section =
                 mk_section
                   "Debugger"
                   (Model.abstract_valid state && Model.statics_valid state)
                   state.Model.debugger_expanded
                   Toggle_debugger
                   contents
               in
               E.select [ evt; input_evt; debugger_evts ], section)
      in
      let abstract_elem = Note.S.Pair.snd ~eq:html_eq abstract in
      let statics_elem = Note.S.Pair.snd ~eq:html_eq statics in
      let debugger_elem = Note.S.Pair.snd ~eq:html_eq debugger in
      ( E.select
          [ abstract_input_event
          ; statics_input_event
          ; abstract |> Note.S.Pair.fst ~eq:phys_equal |> Note.E.swap
          ; statics |> Note.S.Pair.fst ~eq:phys_equal |> Note.E.swap
          ; debugger |> Note.S.Pair.fst ~eq:phys_equal |> Note.E.swap
          ]
      , div
          ~at:[ class' "lvca-viewer" ]
          [ mk_reactive' div abstract_elem
          ; mk_reactive' div statics_elem
          ; mk_reactive' div debugger_elem
          ] )
    ;;
  end

  module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
end
