open Base
open Js_of_ocaml
open Lvca_syntax
open ReactiveData
open Result.Let_syntax
open Common

let eval = Lvca_languages.LambdaCalculus.eval

module Model = struct
  type t =
    { input : string
    ; result : (term, string) Result.t
    ; selected : OptRange.t
    }

  let print { input; result; selected } =
    Fmt.pr
      "{ input = %s; result = %a; selected = %a }\n"
      input
      (fun ppf tm_result ->
        match tm_result with
        | Error msg -> Fmt.pf ppf "%s" msg
        | Ok tm -> Nominal.pp_term Primitive.pp ppf tm)
      result
      OptRange.pp
      selected
  ;;
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit

module Controller = struct
  let update (action : Action.t) model_s signal_update =
    let open Model in
    let { input; result; selected } = React.S.value model_s in
    let new_model =
      match action with
      | Evaluate str ->
        let result =
          let%bind parsed = ParseUtil.parse_string LambdaParse.t str in
          eval parsed
        in
        { input; result; selected }
      | Unselect -> { input; result; selected = None }
      | Select (start, finish) ->
        { input; result; selected = Some Range.{ start; finish } }
      | SwitchInputLang ->
        let input', result' =
          match result with
          | Error _ -> "", result
          | Ok tm ->
            (* TODO: clean up / explain *)
            let result'_str = Fmt.str "%a" lambda_pretty tm in
            Fmt.pr "result'_str: %s\n" result'_str;
            result'_str, ParseUtil.parse_string LambdaParse.t result'_str
        in
        (* TODO: update not with result but input *)
        { input = input'; selected = None; result = result' }
    in
    signal_update new_model
  ;;
end

let demo_template handler input_elem output_elem =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  [%html{|
    <div>
      <h2>Eval with Provenance</h2>
      <div class="container">
        <div class="side">
          <h3>input</h3>
          |}[ input_elem ]{|
        </div>
        <div class="switch-languages">
          <button onclick=|}handler{|>
            switch input languages
          </button>
        </div>
        <div class="side">
          <h3>output</h3>
          |}[ output_elem ]{|
        </div>
      </div>
    </div>
  |}] [@@@ocamlformat "disable"]

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let mk_input model_s signal_update =
    let input_val = model_s
      |> React.S.map (fun m -> m.Model.input)
      |> R.Html.txt
    in
    let input =
      [%html{|
        <textarea rows=2 cols=60 autofocus class="input">|}input_val{|</textarea>
      |}]
    in
    let input_dom = To_dom.of_textarea input in
    bind_event Ev.keydowns input_dom (fun evt ->
        Lwt.return
          (if WebUtil.is_special_enter evt
          then (
            Dom.preventDefault evt;
            Controller.update
              (Evaluate (Js.to_string input_dom##.value))
              model_s
              signal_update)
          else ()));
    bind_event Ev.selects input_dom (fun evt ->
        let elem = evt##.target |> Js.Opt.to_option |> Option.value_exn in
        let textarea =
          elem |> Dom_html.CoerceTo.textarea |> Js.Opt.to_option |> Option.value_exn
        in
        let start = textarea##.selectionStart in
        let finish = textarea##.selectionEnd in
        Controller.update (Select (start, finish)) model_s signal_update;
        (* Used for debugging only -- can be removed: *)
        (* let str = textarea##.value##substring start finish in *)
        (* Caml.Printf.printf "Selected %u-%u '%s'\n" start finish (Js.to_string str); *)
        Lwt.return ());
    bind_event Ev.clicks input_dom (fun _evt ->
        Controller.update Unselect model_s signal_update;
        Lwt.return ());
    (* XXX why doesn't the textarea automatically update? *)
    let (_ : unit React.event) =
      model_s
      (* Create an event when the input has changed *)
      |> React.S.map ~eq:(fun m1 m2 -> Caml.(m1.Model.input = m2.Model.input)) Fn.id
      |> React.S.changes
      |> React.E.map (fun Model.{ input; _ } ->
             (* Caml.Printf.printf "Updating input: %s\n" input; *)
             input_dom##.value := Js.string input)
    in
    input
  ;;

  let mk_output model_s =
    let range_s : OptRange.t React.signal =
      model_s |> React.S.map (fun Model.{ selected; _ } -> selected)
    in
    let formatted_s : [> `Code ] Html.elt React.signal =
      model_s
      |> React.S.map (fun Model.{ result; _ } ->
             let elt, formatter = RangeFormatter.mk range_s in
             (match result with
             | Ok tm -> Fmt.pf formatter "%a" lambda_pretty tm
             | Error msg -> Fmt.pf formatter "%s" msg);
             Fmt.flush formatter ();
             elt)
    in
    R.Html.div
      ~a:[ R.Html.a_class (React.S.const [ "output" ]) ]
      (RList.singleton_s formatted_s)
  ;;

  let view model_s signal_update =
    let handler _evt =
      Controller.update SwitchInputLang model_s signal_update;
      false
    in
    demo_template handler (mk_input model_s signal_update) (mk_output model_s )
  ;;
end

let stateless_view =
  let initial_model : Model.t =
    let input = {|(\x -> \y -> x) z w|} in
    let result =
      let%bind parsed = ParseUtil.parse_string LambdaParse.t input in
      eval parsed
    in
    { input; result; selected = None }
  in
  let model_s, signal_update = React.S.create initial_model in
  View.view model_s signal_update
;;
