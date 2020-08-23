open Base
open Js_of_ocaml
open Lvca_syntax
open ReactiveData

type term = Range.t Binding.Nominal.term
type lang = Lambda | Term

module TermParse = Binding.Nominal.Parse(ParseUtil.Angstrom.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse(ParseUtil.Angstrom.NoComment)
let parse = Angstrom.parse_string ~consume:All
let term_pretty = Binding.Nominal.pp_term_range (* XXX why used twice? *)
let lambda_pretty = Lvca_languages.LambdaCalculus.pp (* XXX why used twice? *)

module Model = struct
  type t =
    { input : string
    ; input_lang : lang
    ; result : (term, string) Result.t
    ; selected : Range.t option
    }

  let initial_model : t =
    let input = {|\f -> \g -> \x -> f (g x)|} in
    let result = parse LambdaParse.t input in
    { input; result; input_lang = Lambda; selected = None }
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit

module Action = struct
  type t =
    | Evaluate of string
    | Select of int * int
    | SwitchInputLang
end

module Controller = struct
  let update (action : Action.t) model_s signal_update =
    let open Model in

    let { input; result; input_lang; selected = _ } = React.S.value model_s in
    let new_model = match action with
      | Action.Evaluate str ->
        begin
          let parser = match input_lang with
            | Lambda -> LambdaParse.t
            | Term -> TermParse.t
          in
          let result = parse parser str in
          let selected = None in
          { input; result; input_lang; selected }
        end
      | Select (start, finish) ->
        let selected = Some Range.{ start; finish } in
        { input; result; input_lang; selected }
      | SwitchInputLang ->
        let input_lang' = match input_lang with
          | Lambda -> Term
          | Term -> Lambda
        in
        let input' = match result with
          | Error _ -> ""
          | Ok tm -> match input_lang with
            | Lambda -> Fmt.str "%a" term_pretty tm
            | Term -> Fmt.str "%a" lambda_pretty tm
        in
        { input = input'; input_lang = input_lang'; selected = None; result }
    in
    signal_update new_model
end

let mk_range_formatter
  : Range.t React.signal
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
  let stack : (Range.t * [> `Span ] Html5.elt Queue.t) Stack.t
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
            if Range.(rng < selected_rng)
            then "background-color: rgba(33 150 243 / 50%);"
            else ""
          )
        in
        (* The data-range attribute is currently used just for debugging, it can be
           removed. *)
        [R.Html5.a_style style; Html5.a_user_data "range" (Range.to_string rng)]
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
    { mark_open_stag = (fun stag ->
      begin
        match stag with
          | Range.Stag rng ->
            (* Fmt.pr "Opening stag %a\n" Range.pp rng; *)
            Stack.push stack (rng, Queue.create ())
          | Caml.Format.String_tag _str -> (* Fmt.pr "Opening stag %s\n" str; *) ()
          | _ -> ()
      end;
      ""
    )
    ; mark_close_stag = (fun stag ->
      begin
      match Stack.pop stack with
        | None ->
            begin
              match stag with
                | Caml.Format.String_tag _str -> (* Fmt.pr "Closing stag %s\n" str; *) ()
                | _ -> ()
            end;
        | Some (_, q) ->
          let elem = span (Queue.to_list q) in
          add_at_current_level elem
      end;
      (* clearer?
      stack
        |> Stack.pop_exn
        |> snd
        |> Queue.to_list
        |> span
        |> add_at_current_level;
      *)
      ""
    )
    ; print_open_stag = Fn.const ()
    ; print_close_stag = Fn.const ()
    };

  R.Html5.code top_level_elems, fmt

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.async @@ (fun () -> ev elem handler)

  let mk_input model_s signal_update =
    let input = model_s
      |> React.S.map (fun m -> m.Model.input)
      |> fun (value : string React.signal) -> R.Html5.(textarea
        ~a:[
          a_rows (React.S.const 25);
          a_cols (React.S.const 60);
          a_autofocus ();
          a_class (React.S.const ["input"]);
        ]
        (React.S.const (txt value)))
    in
    let input_dom = To_dom.of_textarea input in

    bind_event Ev.keydowns input_dom (fun evt ->
      let key = evt##.key |> Js.Optdef.to_option |> Option.map ~f:Js.to_string in
      Lwt.return (match key with
        (* TODO: require special key *)
        | Some "Enter"
        -> Controller.update (Evaluate (Js.to_string input_dom##.value))
          model_s signal_update
        | _ -> ())
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
      let str = textarea##.value##substring start finish in
      Controller.update (Select (start, finish)) model_s signal_update;
      (* Used for debugging only -- can be removed: *)
      Caml.Printf.printf "Selected %u-%u '%s'\n" start finish (Js.to_string str);
      Lwt.return ()
      );

    input

  let mk_output model_s =
    let range_s : Range.t React.signal = model_s
      |> React.S.map (fun Model.{ selected; _ } -> match selected with
        | Some r -> r
        | None -> Range.mk 0 0)
    in

    let code_s : [> `Code ] Html5.elt React.signal = model_s
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
      (RList.singleton_s code_s)

  let info model_s =
    let msg_s = model_s
      |> React.S.map (function
        | Ok _tm -> "(parsed)"
        | Error err -> err
      )
    in
    Html5.div [ R.Html5.txt msg_s ]

  let view model_s signal_update = Html5.(div
    [ h2 [ txt "Term / concrete" ]
    ; div ~a:[ a_class ["container"] ]
      [ div ~a:[a_class ["side"]]
        [ h3 [ txt "input" ]
        ; button
          ~a:[a_onclick (fun _evt -> Controller.update SwitchInputLang model_s signal_update; false)]
          [ txt "switch input languages" ]
        ; mk_input model_s signal_update
        ]
      ; div ~a:[a_class ["side"]]
        [ h3 [ txt "output" ]
        ; mk_output model_s
        ]
      ]
    ; info (model_s |> React.S.map (fun m -> m.Model.result))
    ])
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app"))
      (fun () -> assert false)
  in
  let m = Model.initial_model in
  let model_s, signal_update = React.S.create m in
  Dom.appendChild parent
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view model_s signal_update));
  Lwt.return ()

let (_ : unit Lwt.t) = Lwt.Infix.(Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main)
