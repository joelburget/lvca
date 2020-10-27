open Base
open Lvca_core
open Lvca_syntax

module ParseCore = Core.Parse (ParseUtil.CComment)
module ParseParser = Lvca_languages.Parser.Parse (ParseUtil.CComment)

let parse_parser str = ParseUtil.parse_string (ParseParser.t ParseCore.term) str

module Model = struct
  type parser_defn_signal_pool = string SignalPool.t
  let parser_defn_signal_pool = SignalPool.create ()

  type parser_context =
    { parser_str_key: SignalPool.key
    ; parser: (Lvca_languages.Parser.t, string) Result.t option
    ; evaluations: string list
    }

  let new_context =
    { parser_str_key = SignalPool.add parser_defn_signal_pool (React.S.create "")
    ; parser = None
    ; evaluations = []
    }

  type t = parser_context list

  let parser_str = {|"foo" | "bar"|}
  let initial_model =
    [
      { parser_str_key = SignalPool.add parser_defn_signal_pool (React.S.create parser_str)
      ; parser = Some (parse_parser parser_str)
      ; evaluations = ["foo"]
      }
    ]
end

module Action = struct
  type t =
    | AddParser
    | UpdateParser of int * string
    | UpdateTest of int * int * string
end

module Controller = struct
  let update (action : Action.t) (model_s : Model.t React.S.t) signal_update =
    let contexts = React.S.value model_s in
    let parser_defn_signal_pool = Model.parser_defn_signal_pool in

    let model = match action with
      | AddParser
      -> Model.new_context :: contexts
      | UpdateParser (i, parser_str) ->
        let Model.{ parser_str_key; _ } = List.nth_exn contexts i in
        let _parser_str_s, parser_str_update =
          SignalPool.find_exn parser_defn_signal_pool parser_str_key
        in
        parser_str_update parser_str;
        contexts
      | UpdateTest (i, j, str) ->
        Lvca_util.List.update_nth contexts ~i ~f:(fun context ->
          let evaluations = Lvca_util.List.set_nth context.evaluations ~i:j ~data:str
          in { context with evaluations }
        )
    in
    signal_update model
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  let view model_s signal_update =
    let update evt = Controller.update evt model_s signal_update in
    (*

    let test_input, test_input_event = Common.mk_single_line_input
      (model_s |> React.S.map (fun model -> model.Model.test))
    in

    let (_ : unit React.event) = parser_defn_input_event
      |> React.E.map (function
        | Common.InputUpdate str
        -> update (UpdateParser str)
        | InputSelect _ -> ()
        | InputUnselect -> ()
        )
    in

    let (_ : unit React.event) = test_input_event
      |> React.E.map (fun str -> update (UpdateTest str))
    in
    *)

    let mk_error msg = [%html{|<div class="error">|}[Html.txt msg]{|</div>|}] in

    let mk_context Model.{ parser_str_key; parser; evaluations } =
      let parser_str_s, parser_str_update =
        SignalPool.find_exn Model.parser_defn_signal_pool parser_str_key
      in
      let parser_defn_input, parser_defn_input_event = Common.mk_multiline_input
        parser_str_s
      in

      let (_ : unit React.event) = parser_defn_input_event
        |> React.E.map (function
          | Common.InputUpdate str -> parser_str_update str
          | InputSelect _ -> ()
          | InputUnselect -> ()
          )
      in

      let elems = match parser with
        | None ->
          [ parser_defn_input
          ]
        | Some (Error msg) ->
          [ parser_defn_input
          ; mk_error msg
          (* ; test_input *)
          ]
        | Some (Ok parser) ->
          let module Direct = Lvca_languages.Parser.Direct in
          let translate_direct, parse_direct = Direct.(translate_direct, parse_direct) in
          let parser_d = translate_direct parser in
          let eval_elems = evaluations
            |> List.map ~f:(fun test -> match parse_direct parser_d test with
              | Error (msg, tm_opt) ->
                let tm_str = match tm_opt with
                  | None -> "(no tm) " ^ msg
                  | Some tm -> (Printf.sprintf "%s: %s" msg (Fmt.str "%a" Core.pp tm ))

                in
                mk_error tm_str
              | Ok tm ->
                let str = Fmt.str "%a" (Nominal.pp_term_ranges Primitive.pp) tm in
                [%html{|<div>|}[Html.txt str]{|</div>|}]
            )
          in
          [ parser_defn_input
          ; Html.div eval_elems
          (* ; test_input *)
          (* ; output *)
          ]
        in
        Html.div elems
    in

    let context_elems_s = model_s
      |> React.S.map (fun ctx_list -> ctx_list
        |> List.map ~f:mk_context
      )
    in

    let new_parser_handler _evt = update AddParser; false in

    Html.div
      [ Html.(button ~a:[a_onclick new_parser_handler] [txt "create new parser"])
      ; R.Html.div (ReactiveData.RList.from_signal context_elems_s)
      ]
end

let stateless_view =
  let model_s, signal_update = React.S.create Model.initial_model in
  View.view model_s signal_update
;;
