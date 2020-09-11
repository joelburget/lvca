open Base
open Js_of_ocaml
open Lvca_syntax
open ReactiveData

type term = OptRange.t Nominal.term
type lang = Lambda | Term

module TermParse = Nominal.Parse(ParseUtil.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse(ParseUtil.NoComment)
let term_pretty = Nominal.pp_term_range (* XXX why used twice? *)
let lambda_pretty = Lvca_languages.LambdaCalculus.pp (* XXX why used twice? *)

module Model = struct
  type t =
    { input : string
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; selected : OptRange.t
    }

  let initial_model : t =
    let input = {|\f -> \g -> \x -> f (g x)|} in
    let result = ParseUtil.parse_string LambdaParse.t input in
    { input; result; input_lang = Lambda; selected = None }

  let print { input; input_lang; result; selected } =
    let input_lang_str = match input_lang with
      | Lambda -> "Lambda"
      | Term -> "Term"
    in
    Fmt.pr "{ input = %s; input_lang = %s; result = %a; selected = %a }\n"
      input
      input_lang_str
      (fun ppf tm_result -> match tm_result with
        | Error msg -> Fmt.pf ppf "%s" msg
        | Ok tm -> Nominal.pp_term ppf tm)
      result
      (fun ppf rng_opt -> OptRange.pp ppf rng_opt)
      selected
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit

module Action = struct
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

let parser_of = function
  | Lambda -> LambdaParse.t
  | Term -> TermParse.t

module Controller = struct
  let update (action : Action.t) model_s signal_update =
    let open Model in

    let { input; result; input_lang; selected } = React.S.value model_s in
    let new_model = match action with
      | Evaluate str ->
        let result = ParseUtil.parse_string (parser_of input_lang) str in
        { input; input_lang; result; selected }
      | Unselect -> { input; result; input_lang; selected = None }
      | Select (start, finish) ->
        { input; result; input_lang; selected = Some Range.{ start; finish } }
      | SwitchInputLang ->
        let input_lang', formatter = match input_lang with
          | Lambda -> Term, term_pretty
          | Term -> Lambda, lambda_pretty
        in
        let input', result' = match result with
          | Error _ -> "", result
          | Ok tm ->
            (* TODO: clean up / explain *)
            let result'_str = Fmt.str "%a" formatter tm in
            Fmt.pr "result'_str: %s\n" result'_str;
            result'_str, ParseUtil.parse_string (parser_of input_lang') result'_str
        in
        { input = input'; input_lang = input_lang'; selected = None; result = result' }
    in
    signal_update new_model
end

(** The incoming signal holds the currently selected range. We return both a
    Dom element (<code>) to be inserted in the Dom and the formatter which is used to
    write formatted text to this element. Note that the returned Dom element is empty
    until the formatter is used and flushed. *)
let mk_range_formatter
  : OptRange.t React.signal
  -> [> `Code ] Js_of_ocaml_tyxml.Tyxml_js.Html5.elt * Caml.Format.formatter
  = fun rng_signal ->
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let br, span, txt = Html.(br, span, txt) in

  let top_level_elems, top_level_handle = RList.create [] in

  (* - Push a new range and queue on the stack every time we enter a semantic tag
     - Pop every time we exit a tag
     - The queue is used to add elements (text and nested children) under this element
     - The range is the extent of this element, used to update the style when text is
       selected
   *)
  let stack : (OptRange.t * [> `Span ] Html5.elt Queue.t) Stack.t
    = Stack.create ()
  in

  let add_at_current_level elem = match Stack.top stack with
    | None -> RList.snoc elem top_level_handle
    | Some (_, q) -> Queue.enqueue q elem
  in

  let get_attrs () = match Stack.top stack with
    | None -> []
    | Some (rng, _) ->
        let style = rng_signal
          |> React.S.map (fun selected_rng ->
            (* Highlight if this is a subset of the selected range *)
            if OptRange.(rng < selected_rng)
            then "background-color: rgba(33 150 243 / 50%);"
            else ""
          )
        in
        (* The data-range attribute is currently used just for debugging, it can be
           removed. *)
        [R.Html5.a_style style; Html5.a_user_data "range" (OptRange.to_string rng)]
  in

  let add_text str = add_at_current_level (span ~a:(get_attrs ()) [txt str]) in

  let out_fns : Caml.Format.formatter_out_functions =
    { out_string = (fun str _start _char_count -> add_text str)
    (* No need to do anything -- we update the element immediately on receiving
       characters. *)
    ; out_flush = Fn.id
    ; out_newline = (fun () -> add_at_current_level (br ()))
    ; out_spaces = (fun spaces -> add_text (String.make spaces ' '))
    ; out_indent = Caml.Printf.printf "out_indent %n\n"
    }
  in

  let fmt = Caml.Format.formatter_of_out_functions out_fns in
  Caml.Format.pp_set_tags fmt true;

  Caml.Format.pp_set_formatter_stag_functions fmt
    (* We open a new span for every range tag we encounter. All children until we
       encounter the matching close tag will be nested under it (by enqueuing). *)
    { mark_open_stag = (function
      | Range.Stag rng -> Stack.push stack (Some rng, Queue.create ()); ""
      | _ -> ""
    )
    (* Closing a range; create the span holding all of the enqueued children. *)
    ; mark_close_stag = (fun _ ->
      begin
      match Stack.pop stack with
        | None -> ()
        | Some (_, q) -> q
          |> Queue.to_list
          |> span
          |> add_at_current_level
      end;
      ""
    )
    ; print_open_stag = Fn.const ()
    ; print_close_stag = Fn.const ()
    };

  R.Html5.code top_level_elems, fmt

module WebUtil = struct
  let is_enter key_evt =
    String.equal
      "Enter"
      (key_evt##.code |> Js.Optdef.to_option |> Option.value_exn |> Js.to_string)
  let is_meta key_evt = Js.to_bool key_evt##.metaKey
  let is_shift key_evt = Js.to_bool key_evt##.shiftKey
  let is_ctrl key_evt = Js.to_bool key_evt##.ctrlKey

  (** Is this an enter keypress plus meta, shift, or ctrl *)
  let is_special_enter key_evt =
    is_enter key_evt &&
    (is_meta key_evt || is_shift key_evt || is_ctrl key_evt)
end;;

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.async @@ (fun () -> ev elem handler)

  let mk_input model_s signal_update =
    let input = Html5.(textarea
      ~a:[
        a_rows 2;
        a_cols 60;
        a_autofocus ();
        a_class ["input"];
      ])
      (model_s
        |> React.S.map (fun m ->
          (* Caml.Printf.printf "Updating input (209): %s\n" m.Model.input; *)
          m.Model.input)
        |> R.Html5.txt)
    in
    let input_dom = To_dom.of_textarea input in

    bind_event Ev.keydowns input_dom (fun evt ->
      Lwt.return (
        if WebUtil.is_special_enter evt
        then
          begin
            Dom.preventDefault evt;
            Controller.update (Evaluate (Js.to_string input_dom##.value))
              model_s signal_update
          end
        else ())
      );

    bind_event Ev.selects input_dom (fun evt ->
      let elem = evt##.target |> Js.Opt.to_option |> Option.value_exn in
      let textarea = elem
        |> Dom_html.CoerceTo.textarea
        |> Js.Opt.to_option
        |> Option.value_exn
      in

      let start = textarea##.selectionStart in
      let finish = textarea##.selectionEnd in
      Controller.update (Select (start, finish)) model_s signal_update;
      (* Used for debugging only -- can be removed: *)
      (* let str = textarea##.value##substring start finish in *)
      (* Caml.Printf.printf "Selected %u-%u '%s'\n" start finish (Js.to_string str); *)
      Lwt.return ()
      );

    bind_event Ev.clicks input_dom (fun _evt ->
      Controller.update Unselect model_s signal_update;
      Lwt.return ()
      );

    (* XXX why doesn't the textarea automatically update? *)
    let _ : unit React.event = model_s
      (* Create an event when the input has changed *)
      |> React.S.map ~eq:(fun m1 m2 -> Caml.(m1.Model.input = m2.Model.input)) Fn.id
      |> React.S.changes
      |> React.E.map (fun Model.{ input; _ } ->
        (* Caml.Printf.printf "Updating input (254): %s\n" input; *)
        input_dom##.value := Js.string input
      )
    in

    input

  let mk_output model_s =
    let range_s : OptRange.t React.signal = model_s
      |> React.S.map (fun Model.{ selected; _ } -> selected)
    in

    let formatted_s : [> `Code ] Html5.elt React.signal = model_s
      |> React.S.map (fun Model.{ result; input_lang; _ } ->
        let elt, formatter = mk_range_formatter range_s in
        begin
          match result, input_lang with
            | Ok tm, Lambda -> Fmt.pf formatter "%a" term_pretty tm
            | Ok tm, Term -> Fmt.pf formatter "%a" lambda_pretty tm
            | Error msg, Lambda
            | Error msg, Term -> Fmt.pf formatter "%s" msg
        end;
        Fmt.flush formatter ();
        elt
        )
    in

    R.Html5.div
      ~a:[ R.Html5.a_class (React.S.const ["output"]) ]
      (RList.singleton_s formatted_s)

  let make_descriptions model_s = model_s
    |> React.S.map (fun Model.{ input_lang; _ } -> match input_lang with
      | Lambda -> "input (concrete)", "output (abstract)"
      | Term -> "input (abstract)", "output (concrete)")

  let view model_s signal_update =
    let descriptions_s = make_descriptions model_s in
    let input_desc, output_desc = React.S.Pair.(fst descriptions_s, snd descriptions_s) in
    Html5.(div
    [ h2 [ txt "Concrete / Abstract" ]
    ; div ~a:[ a_class ["container"] ]
      [ div ~a:[ a_class ["side"] ]
        [ h3 [ R.Html5.txt input_desc ]
        ; mk_input model_s signal_update
        ]
      ; div ~a:[ a_class ["switch-languages"] ]
        [ button
          ~a:[
            a_onclick (fun _evt ->
              Controller.update SwitchInputLang model_s signal_update;
              false)
          ]
          [ txt "switch input languages" ]
        ]
      ; div ~a:[ a_class ["side"] ]
        [ h3 [ R.Html5.txt output_desc ]
        ; mk_output model_s
        ]
      ]
    ])
end

let insert_demo elem =
  let model_s, signal_update = React.S.create Model.initial_model in
  Dom.appendChild elem
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view model_s signal_update));
  Lwt.return ()

let (_ : unit) = Js.export "TermAndConcrete"
  (object%js
     method run = insert_demo
   end)

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app"))
      (fun () -> assert false)
  in
  insert_demo parent

let (_ : unit Lwt.t) = Lwt.Infix.(Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main)
