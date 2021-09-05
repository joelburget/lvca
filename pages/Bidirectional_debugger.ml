open Base
open Brr
open Brr_note
open Note
open Lvca_provenance
open Lvca_syntax
open Lvca_bidirectional
open Lvca_util

let a, button, div, h3, h4, input, li, tbody, table, textarea, th, thead, td, txt', tr, ul
  =
  El.(
    a, button, div, h3, h4, input, li, tbody, table, textarea, th, thead, td, txt', tr, ul)
;;

let class', classes, mk_reactive', type' = Prelude.(class', classes, mk_reactive', type')

(* TODO: this is hacky -- don't use it *)
let parse_tm : string -> Opt_range.t Nominal.Term.t =
 fun str ->
  match Lvca_parsing.(parse_string (Nominal.Term.parse' ~comment:no_comment) str) with
  | Ok tm -> Nominal.Term.map_info ~f:Commented.get_range tm
  | Error err -> failwith err
;;

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
    type t = Opt_range.t Nominal.Term.t -> (El.t, string) Result.t
  end

  module Model = struct
    module Debugger_state = struct
      type t =
        { input : string
        ; steps : Opt_range.t Bidirectional.Trace_step.t list
        ; current_step : int
        }

      let ( = ) t1 t2 =
        String.(t1.input = t2.input)
        && List.equal
             (Bidirectional.Trace_step.equal ~info_eq:Opt_range.( = ))
             t1.steps
             t2.steps
        && Int.(t1.current_step = t2.current_step)
      ;;
    end

    type t =
      { abstract_expanded : bool
      ; abstract_input : string
      ; abstract_parsed : (Opt_range.t Abstract_syntax.t, string) Result.t option
      ; statics_expanded : bool
      ; statics_input : string
      ; statics_parsed : (Opt_range.t Statics.Rule.t list, string) Result.t option
      ; debugger_expanded : bool
      ; debugger : Debugger_state.t option
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
           (Result.equal (Abstract_syntax.equal Opt_range.( = )) String.( = ))
           t1.abstract_parsed
           t2.abstract_parsed
      && Bool.(t1.statics_expanded = t2.statics_expanded)
      && String.(t1.statics_input = t2.statics_input)
      && Option.equal
           (Result.equal
              (List.equal (Statics.Rule.equal ~info_eq:Opt_range.( = )))
              String.( = ))
           t1.statics_parsed
           t2.statics_parsed
      && Bool.(t1.debugger_expanded = t2.debugger_expanded)
      && Option.equal Debugger_state.( = ) t1.debugger t2.debugger
    ;;

    let initial_model =
      { abstract_expanded = true
      ; abstract_input = "bool := True() | False()"
      ; abstract_parsed = None
      ; statics_expanded = false
      ; statics_input = "TODO: statics"
      ; statics_parsed = None
      ; debugger_expanded = false
      ; debugger = None
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
          Lvca_parsing.(
            parse_string
              (whitespace *> Abstract_syntax.parse ~comment:c_comment)
              abstract_input)
        in
        let abstract_result =
          Result.map ~f:(Abstract_syntax.map_info ~f:Commented.get_range) abstract_result
        in
        Model.{ model with abstract_parsed = Some abstract_result }
      | Statics_update statics_input ->
        let statics_result =
          Lvca_parsing.(parse_string (whitespace *> Statics.parse) statics_input)
        in
        { model with statics_parsed = Some statics_result }
      | (Step_forward | Step_backward) as step ->
        let debugger =
          match model.debugger with
          | None -> None
          | Some state ->
            (match step with
            | Step_forward -> Some { state with current_step = state.current_step + 1 }
            | _ -> Some { state with current_step = state.current_step - 1 })
        in
        { model with debugger }
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
        (match model.statics_parsed with
        | Some (Ok rules) ->
          let steps = Queue.create () in
          let handle_trace = Queue.enqueue steps in
          let env = Bidirectional.Env.{ rules; var_types = String.Map.empty } in
          let debugger =
            match parse_tm input with
            | exception _ -> None
            | tm ->
              (match Bidirectional.infer_trace handle_trace env tm with
              | exception _ -> None
              | _ty ->
                Some
                  Model.Debugger_state.
                    { input; steps = Queue.to_list steps; current_step = 0 })
          in
          { model with debugger }
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
      let abstract_editor, abstract_input_event =
        Multiline_input.mk
          (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.abstract_input))
      in
      let abstract_input_event =
        abstract_input_event
        |> E.filter_map (function
               | Common.Evaluate_input str -> Some (Abstract_update str)
               | _ -> None)
      in
      let eq (evt1, el1) (evt2, el2) = phys_equal evt1 evt2 && Common.html_eq el1 el2 in
      let abstract =
        model_s
        |> S.map ~eq (fun state ->
               mk_section
                 "Abstract"
                 true
                 state.Model.abstract_expanded
                 Toggle_abstract
                 abstract_editor)
      in
      let statics_editor, statics_input_event =
        Multiline_input.mk
          (model_s |> S.map ~eq:String.( = ) (fun model -> model.Model.statics_input))
      in
      let statics_input_event =
        statics_input_event
        |> E.filter_map (function
               | Common.Evaluate_input str -> Some (Statics_update str)
               | _ -> None)
      in
      let statics =
        model_s
        |> S.map ~eq (fun state ->
               mk_section
                 "Statics"
                 (Model.abstract_valid state)
                 state.Model.statics_expanded
                 Toggle_statics
                 statics_editor)
      in
      let debugger_input = input ~at:[ type' "text" ] () in
      let debugger_input_evt =
        let f evt =
          let keyboard_evt = Ev.as_type evt in
          if Web_util.is_enter keyboard_evt
          then
            El.at At.Name.value debugger_input
            |> Option.map ~f:(fun jstr -> Evaluate (Jstr.to_string jstr))
          else None
        in
        Evr.on_el Ev.keyup f debugger_input |> E.filter_map Fn.id
      in
      let debugger =
        model_s
        |> S.map ~eq (fun state ->
               let debugger_evts, debugger_results =
                 match state.Model.debugger with
                 | None -> E.never, El.span []
                 | Some { input; steps; current_step } ->
                   let step_count = List.length steps in
                   let trace_step = List.nth_exn steps current_step in
                   let elem = elem_of_current_stack trace_step in
                   let back_evt, back_button =
                     if Int.(current_step = 0)
                     then
                       ( E.never
                       , button
                           ~at:(classes "text-gray cursor-not-allowed")
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
                           ~at:(classes "text-gray cursor-not-allowed")
                           [ txt' "next step" ] )
                     else (
                       let elem = button [ txt' "next step" ] in
                       let evt = Evr.on_el Ev.click (fun _evt -> Step_forward) elem in
                       evt, elem)
                   in
                   let elem =
                     div
                       [ div [ txt' ("Inferring type of " ^ input) ]
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
               let contents = div [ debugger_input; debugger_results ] in
               let evt, section =
                 mk_section
                   "Debugger"
                   (Model.abstract_valid state && Model.statics_valid state)
                   state.Model.debugger_expanded
                   Toggle_debugger
                   contents
               in
               E.select [ evt; debugger_input_evt; debugger_evts ], section)
      in
      let eq = phys_equal in
      let abstract_evt = abstract |> Note.S.Pair.fst ~eq |> Note.E.swap in
      let statics_evt = statics |> Note.S.Pair.fst ~eq |> Note.E.swap in
      let debugger_evt = debugger |> Note.S.Pair.fst ~eq |> Note.E.swap in
      let eq = Common.html_eq in
      let abstract_elem = Note.S.Pair.snd ~eq abstract in
      let statics_elem = Note.S.Pair.snd ~eq statics in
      let debugger_elem = Note.S.Pair.snd ~eq debugger in
      ( E.select
          [ abstract_input_event
          ; statics_input_event
          ; abstract_evt
          ; statics_evt
          ; debugger_evt
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
