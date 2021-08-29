open Bonsai_web
open Core_kernel
open Lvca
open Lvca_web
open Bidirectional
open Statics

module AbstractEditor = ContainedEditor.ContainedEditorComponent (struct
  type t = AbstractSyntax.t

  let parse = Parsing.AbstractSyntax.parse
end)

let abstract_editor_component = Bonsai.of_module (module AbstractEditor)

module ConcreteEditor = ContainedEditor.ContainedEditorComponent (struct
  type t =
    ConcreteSyntaxDescription.pre_terminal_rule list
    * ConcreteSyntaxDescription.nonterminal_rule list

  let parse = Parsing.ConcreteSyntax.parse
end)

let concrete_editor_component = Bonsai.of_module (module AbstractEditor)

(* TODO: this is hacky -- don't use it *)
let parse_cvt : string -> term =
 fun str ->
  let tm =
    match Parsing.Term.parse str with
    | Ok tm -> tm
    | Error err -> failwith (ParseError.to_string err)
  in
  let tm' =
    match Binding.DeBruijn.from_nominal tm with Ok tm -> tm | Error msg -> failwith msg
  in
  Statics.of_de_bruijn tm'
;;

let div, h3, h4, li, tbody, td, text, tr, ul =
  Vdom.Node.(div, h3, h4, li, tbody, td, text, tr, ul)
;;

let class_ = Vdom.Attr.class_

let elem_of_env { var_types; _ } =
  let vars =
    var_types
    |> Map.to_alist
    |> List.map ~f:(fun (name, ty) -> li [] [ text name; text (string_of_term ty) ])
  in
  match vars with
  | [] -> h4 [] [ text "(empty environment)" ]
  | _ -> div [] [ h4 [] [ text "environment" ]; ul [] vars ]
;;

let string_of_typing (Typing (tm, ty)) = string_of_term tm ^ ": " ^ string_of_term ty

let elem_of_trace_entry = function
  | CheckTrace (env, typing) ->
    div [] [ h3 [] [ text "check" ]; elem_of_env env; text (string_of_typing typing) ]
  | InferTrace (env, term) ->
    div [] [ h3 [] [ text "infer" ]; elem_of_env env; text (string_of_term term) ]
  | CheckSuccess -> div [] [ h3 [] [ text "check success" ] ]
  | CheckFailure _ -> div [] [ h3 [] [ text "check failure" ] ]
  | Inferred ty -> div [] [ h3 [] [ text "inferred" ]; text (string_of_term ty) ]
;;

let elem_of_current_stack current_stack =
  let last_row, stack =
    match current_stack with
    | Inferred ty :: entry :: current_stack' ->
      let row =
        tr
          []
          [ td [] [ elem_of_trace_entry entry ]
          ; td [ class_ "result-good" ] [ text ("inferred " ^ string_of_term ty) ]
          ]
      in
      [ row ], current_stack'
    | CheckSuccess :: entry :: current_stack' ->
      let row =
        tr
          []
          [ td [] [ elem_of_trace_entry entry ]
          ; td [ class_ "result-good" ] [ text "success" ]
          ]
      in
      [ row ], current_stack'
    | CheckFailure msg :: entry :: current_stack' ->
      let row =
        tr
          []
          [ td [] [ elem_of_trace_entry entry ]
          ; td [ class_ "result-bad" ] [ text ("failure: " ^ msg) ]
          ]
      in
      [ row ], current_stack'
    | _ -> [], current_stack
  in
  let other_rows =
    stack
    |> List.map ~f:(fun trace_entry ->
           tr [] [ td [] [ elem_of_trace_entry trace_entry ]; td [] [] ])
    |> List.rev
  in
  tbody [] (other_rows @ last_row)
;;

module Term_render_component = struct
  let name = "Term Render"

  module Input = struct
    type t = Binding.Nominal.term -> (Vdom.Node.t, string) Core_kernel.Result.t
  end

  module Result = Vdom.Node

  module Model = struct
    type debugger_state =
      { input : string
      ; steps : trace_step list
      ; current_step : int
      }

    type t =
      { abstract_expanded : bool
      ; abstract : (AbstractSyntax.t, ParseError.t) Core_kernel.Result.t option
      ; statics_expanded : bool
      ; statics : (Statics.rule list, ParseError.t) Core_kernel.Result.t option
      ; debugger_expanded : bool
      ; debugger : debugger_state option
      }
  end

  module Action = struct
    type t =
      | AbstractUpdate of (AbstractSyntax.t, ParseError.t) Core_kernel.Result.t
      | StaticsUpdate of (Statics.rule list, ParseError.t) Core_kernel.Result.t
      | Evaluate of string
      | StepForward
      | StepBackward
      | ToggleAbstract
      | ToggleStatics
      | ToggleDebugger
    [@@deriving sexp]
  end

  let apply_action ~inject:_ ~schedule_event:_ _input model = function
    | Action.AbstractUpdate abstract_result ->
      Model.{ model with abstract = Some abstract_result }
    | StaticsUpdate statics_result -> { model with statics = Some statics_result }
    | (StepForward | StepBackward) as step ->
      let debugger =
        match model.debugger with
        | None -> None
        | Some state ->
          (match step with
          | StepForward -> Some { state with current_step = state.current_step + 1 }
          | _ -> Some { state with current_step = state.current_step - 1 })
      in
      { model with debugger }
    | ToggleAbstract -> { model with abstract_expanded = not model.abstract_expanded }
    | ToggleStatics -> { model with statics_expanded = not model.statics_expanded }
    | ToggleDebugger -> { model with debugger_expanded = not model.debugger_expanded }
    | Evaluate input ->
      (match model.statics with
      | Some (Ok rules) ->
        let steps = Queue.create () in
        let handle_trace = Queue.enqueue steps in
        let env = { rules; var_types = String.Map.empty } in
        let debugger =
          match parse_cvt input with
          | exception _ -> None
          | tm ->
            (match infer_trace handle_trace env tm with
            | exception _ -> None
            | _ty -> Some Model.{ input; steps = Queue.to_list steps; current_step = 0 })
        in
        { model with debugger }
      | _ -> model)
  ;;

  let compute : inject:(Action.t -> Vdom.Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject _input state ->
    let a, button, input, none, table, text, textarea, th, thead =
      Vdom.Node.(a, button, input, none, table, text, textarea, th, thead)
    in
    let class_, classes, href, on_change, on_click, on_keyup, type_ =
      Vdom.Attr.(class_, classes, href, on_change, on_click, on_keyup, type_)
    in
    let abstract_valid = match state.abstract with Some (Ok _) -> true | _ -> false in
    let statics_valid = match state.statics with Some (Ok _) -> true | _ -> false in
    let mk_section name enabled expanded toggle_evt contents =
      div
        [ classes ("section-box" :: (if enabled then [] else [ "disabled" ])) ]
        [ h3
            []
            [ a
                [ href "#"; on_click (fun _evt -> inject toggle_evt) ]
                [ text (if expanded then "- " ^ name else "+ " ^ name) ]
            ]
        ; (if expanded then contents else none)
        ]
    in
    (* (* XXX don't parse here *) let handle_abstract_change = fun _ str -> inject
       (AbstractUpdate (Parsing.AbstractSyntax.parse str)) in

       let abstract = mk_section "Abstract" true state.abstract_expanded ToggleAbstract
       (textarea [ on_change handle_abstract_change ] []) (* Codemirror.codemirror *) in *)
    let abstract =
      mk_section
        "Abstract"
        true
        state.abstract_expanded
        ToggleAbstract
        abstract_editor_component
    in
    (* XXX don't parse here *)
    let handle_statics_change _ str =
      inject (StaticsUpdate (Parsing.Statics.parse str))
    in
    let statics =
      mk_section
        "Statics"
        abstract_valid
        state.statics_expanded
        ToggleStatics
        (textarea [ on_change handle_statics_change ] [])
    in
    let debugger =
      mk_section
        "Debugger"
        (abstract_valid && statics_valid)
        state.debugger_expanded
        ToggleDebugger
        (let debugger_input =
           let handle_keyup evt =
             if Web_util.is_enter evt
             then (
               match Web_util.str_of_input_evt evt with
               | Some str -> inject (Evaluate str)
               | None -> Vdom.Event.Ignore)
             else Vdom.Event.Ignore
           in
           input [ type_ "text"; on_keyup handle_keyup ] []
         in
         let debugger_results =
           match state.debugger with
           | None -> none
           | Some { input; steps; current_step } ->
             let step_count = List.length steps in
             let trace_step = List.nth_exn steps current_step in
             let elem = elem_of_current_stack trace_step in
             let back_button =
               button
                 [ (if current_step = 0
                   then class_ "disabled"
                   else on_click (fun _evt -> inject StepBackward))
                 ]
                 [ text "previous step" ]
             in
             let forward_button =
               button
                 [ (if current_step = step_count - 1
                   then class_ "disabled"
                   else on_click (fun _evt -> inject StepForward))
                 ]
                 [ text "next step" ]
             in
             div
               []
               [ div [] [ text ("Inferring type of " ^ input) ]
               ; div [] [ text (Printf.sprintf "%n / %n" (current_step + 1) step_count) ]
               ; back_button
               ; forward_button
               ; table
                   []
                   [ thead
                       []
                       [ tr [] [ th [] [ text "action" ]; th [] [ text "result" ] ] ]
                   ; elem
                   ]
               ]
         in
         div [] [ debugger_input; debugger_results ])
    in
    div [ class_ "lvca-viewer" ] [ abstract; statics; debugger ]
 ;;
end

let component = Bonsai.of_module (module Term_render_component)

let initial_model =
  Term_render_component.Model.
    { abstract_expanded = true
    ; abstract = None
    ; statics_expanded = false
    ; statics = None
    ; debugger_expanded = false
    ; debugger = None
    }
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:Lvca_web.Render_doc_term.render
    ~initial_model
    ~bind_to_element_with_id:"app"
    component
;;
