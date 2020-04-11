open Bonsai_web
open Core_kernel

open Lvca
open Lvca_web

module Parse_component = struct
  let name = "Parse Hutton's Razor"

  module Input = Unit
  module Result = Vdom.Node
  module Model = struct
    type t = string * (ConcreteSyntax.formatted_tree, string) Core_kernel.Result.t option
  end
  module Action = Web_util.TextareaAction

  let apply_action ~inject:_ ~schedule_event:_ _input (input, parsed_opt) = function
    | Action.UpdateInput str -> str, parsed_opt
    | Action.Evaluate str -> str, Some (Expr_concrete.parse_concrete input)
  ;;

  let compute : inject:(Action.t -> Vdom.Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject _ (input_str, parsed_opt) ->
     let result = Option.map parsed_opt ~f:(fun tree_result ->
       let text = Vdom.Node.text in
       match Core_kernel.Result.bind tree_result ~f:Expr_concrete.to_ast with
       | Error msg -> text msg
       | Ok tm -> text (Binding.Nominal.pp_term' tm))
     in
     Web_util.textarea ~contents:input_str ~inject ~result
 ;;
end

module Pretty_component = struct
  let name = "Pretty-print Hutton's Razor"

  module Input = Unit
  module Result = Vdom.Node
  module Model = struct
    type t = string * (Binding.Nominal.term, string) Core_kernel.Result.t option
  end
  module Action = Web_util.TextareaAction

  module Parse_term = Parsing.Incremental (Parsing.Parseable_term)

  let apply_action ~inject:_ ~schedule_event:_ _input (input, parsed_opt) = function
    | Action.UpdateInput str -> str, parsed_opt
    | Action.Evaluate str -> str, Some (Parse_term.parse input)
  ;;

  let compute : inject:(Action.t -> Vdom.Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject _ (input_str, parsed_opt) ->
     let result = parsed_opt
       |> Option.map ~f:(function
         | Error msg -> msg
         | Ok ast ->
             Printf.printf "have ast: %s\n" (Binding.Nominal.pp_term' ast);
             ConcreteSyntax.to_string (Expr_concrete.of_ast ast))
       |> Option.map ~f:Vdom.Node.text
     in
     Web_util.textarea ~contents:input_str ~inject ~result
 ;;
end

let parse_component = Bonsai.of_module (module Parse_component)
let pretty_component = Bonsai.of_module (module Pretty_component)

module Model = struct
  type t =
    { parse_model : Parse_component.Model.t
    ; pretty_model : Pretty_component.Model.t
    } [@@deriving fields]
end

let component : (unit, Model.t, Vdom.Node.t) Bonsai.t
  = let open Bonsai.Infix in
    let parse_component' : (unit, Model.t, Vdom.Node.t) Bonsai.t
      = parse_component |> Bonsai.Project.Model.field Model.Fields.parse_model
    in
    let pretty_component' : (unit, Model.t, Vdom.Node.t) Bonsai.t
      = pretty_component |> Bonsai.Project.Model.field Model.Fields.pretty_model
    in
    let composed = Bonsai.Let_syntax.Let_syntax.both parse_component' pretty_component' in
    let layout_both (c1, c2) = Vdom.Node.div [] [c1; c2] in
    composed >>| layout_both

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:Model.({ parse_model = "1 + (2 + 3)", None; pretty_model = "lit(1)", None })
    ~bind_to_element_with_id:"app"
    component
;;
