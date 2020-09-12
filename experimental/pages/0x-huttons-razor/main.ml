open Bonsai_web
open Core_kernel
open Lvca
open Lvca_web

let no_highlight str = Some str, None

module Parse_component = struct
  let name = "Hutton's Razor: Parse"

  module Input = Unit
  module Result = Vdom.Node

  module Model = struct
    type t =
      string
      * ( ConcreteSyntax.formatted_tree
        , (LexerUtil.lex_error, LrParsing.parse_error) Either.t )
        Core_kernel.Result.t
        option
  end

  module Action = Web_util.TextareaAction

  let apply_action ~inject:_ ~schedule_event:_ _input (input, parsed_opt) = function
    | Action.UpdateInput str -> str, parsed_opt
    | Action.Evaluate str -> str, Some (LanguageHuttonsRazor.parse_concrete input)
  ;;

  let compute : inject:(Action.t -> Vdom.Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject _ (input_str, parsed_opt) ->
    let result_str, highlight =
      match parsed_opt with
      | None -> None, None
      | Some parsed ->
        (match parsed with
        | Ok tree ->
          (match LanguageHuttonsRazor.to_ast tree with
          | Error msg -> no_highlight msg
          | Ok tm -> tm |> Binding.Nominal.pp_term' |> no_highlight)
        | Error (First lex_error) ->
          ( Some ("Lexical error: " ^ lex_error.message)
          , Some Web_util.{ start = lex_error.position.pos_cnum + 1; length = 1 } )
        | Error (Second (err_char, msg)) ->
          Some msg, Some Web_util.{ start = err_char; length = 1 })
    in
    let result = Option.map result_str ~f:Vdom.Node.text in
    Web_util.input ~contents:input_str ~highlight ~inject ~result
 ;;
end

module Pretty_component = struct
  let name = "Hutton's Razor: Pretty-print"

  module Input = Unit
  module Result = Vdom.Node

  module Model = struct
    type t = string * (Binding.Nominal.term, ParseError.t) Core_kernel.Result.t option
  end

  module Action = Web_util.TextareaAction

  let apply_action ~inject:_ ~schedule_event:_ _input (input, parsed_opt) = function
    | Action.UpdateInput str -> str, parsed_opt
    | Action.Evaluate str -> str, Some (Parsing.Term.parse input)
  ;;

  let compute : inject:(Action.t -> Vdom.Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject _ (input_str, parsed_opt) ->
    let result_str, highlight =
      match parsed_opt with
      | None -> None, None
      | Some parsed ->
        (match parsed with
        | Ok ast ->
          Some (ast |> LanguageHuttonsRazor.of_ast |> ConcreteSyntax.to_string), None
        | Error parse_error ->
          Printf.printf "%s\n" (ParseError.to_string parse_error);
          ( Some parse_error.message
          , Some
              Web_util.
                { start = parse_error.start_pos.pos_cnum
                ; length = ParseError.length parse_error + 1
                } ))
    in
    let result = Option.map result_str ~f:Vdom.Node.text in
    Web_util.input ~contents:input_str ~highlight ~inject ~result
 ;;
end

module Typecheck_component = struct
  let name = "Hutton's Razor: Typecheck"

  module Input = Unit
  module Result = Vdom.Node

  module Model = struct
    type t =
      string
      * ( ConcreteSyntax.formatted_tree
        , (LexerUtil.lex_error, LrParsing.parse_error) Either.t )
        Core_kernel.Result.t
        option
  end

  module Action = Web_util.TextareaAction

  let apply_action ~inject:_ ~schedule_event:_ _input (input, parsed_opt) = function
    | Action.UpdateInput str -> str, parsed_opt
    | Action.Evaluate str -> str, Some (LanguageHuttonsRazor.parse_concrete input)
  ;;

  let compute : inject:(Action.t -> Vdom.Event.t) -> Input.t -> Model.t -> Result.t =
   fun ~inject _ (input_str, parsed_opt) ->
    let result_str, highlight =
      match parsed_opt with
      | None -> None, None
      | Some parsed ->
        (match parsed with
        | Ok tree ->
          (match LanguageHuttonsRazor.to_ast tree with
          | Error msg -> no_highlight msg
          | Ok tm ->
            (match LanguageHuttonsRazor.infer tm with
            | exception Statics.FreeVar name -> no_highlight name
            | exception LanguageHuttonsRazor.InferenceError ->
              no_highlight "inference error"
            | ty -> ty |> Binding.Nominal.pp_term' |> no_highlight))
        | Error (First lex_error) ->
          ( Some ("Lexical error: " ^ lex_error.message)
          , Some Web_util.{ start = lex_error.position.pos_cnum + 1; length = 1 } )
        | Error (Second (err_char, msg)) ->
          Some msg, Some Web_util.{ start = err_char; length = 1 })
    in
    let result = Option.map result_str ~f:Vdom.Node.text in
    Web_util.input ~contents:input_str ~highlight ~inject ~result
 ;;
end

let parse_component = Bonsai.of_module (module Parse_component)
let pretty_component = Bonsai.of_module (module Pretty_component)
let typecheck_component = Bonsai.of_module (module Typecheck_component)

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:("1 + (2 + 3) + 4", None)
    ~bind_to_element_with_id:"parse-app"
    parse_component
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:("add(lit(1); add(add(lit(2); lit(3)); lit(4)))", None)
    ~bind_to_element_with_id:"pretty-app"
    pretty_component
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:()
    ~initial_model:("1 + 2", None)
    ~bind_to_element_with_id:"typecheck-app"
    typecheck_component
;;
