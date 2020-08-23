open Base
open Js_of_ocaml
open Lvca_syntax
open ReactiveData

type term = Range.t Binding.Nominal.term
type side = Left | Right

module TermParse = Binding.Nominal.Parse(ParseUtil.Angstrom.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse(ParseUtil.Angstrom.NoComment)
let parse = Angstrom.parse_string ~consume:All
let term_pretty = Binding.Nominal.pp_term_range (* XXX why used twice? *)
let lambda_pretty = Lvca_languages.LambdaCalculus.pp (* XXX why used twice? *)

module Model = struct
  type t =
    { left_input : string
    ; right_input : string (* XXX: do we want to maintain two inputs? easier to just
    maintain one. *)
    ; result : (term, string) Result.t
    ; side : side
    ; selected : Range.t option
    }

  let initial_model : t =
    let left_input = {|\f -> \g -> \x -> f (g x)|} in
    let result = parse LambdaParse.t left_input in
    let right_input = match result with
      | Error _ -> ""
      | Ok tm -> Fmt.str "%a" term_pretty tm
    in
    { left_input; right_input; result; side = Left; selected = None }
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit
type react_pair = signal * update_fun

module Action = struct
  type t =
    (* | Update of side * string *)
    | Evaluate of string
    | Select of int * int
    | SwitchSides
end

module Controller = struct
  let update (action : Action.t) ((r, f) : react_pair) =
    let open Model in

    let { left_input; right_input; result; side; selected = _ } = React.S.value r in
    let new_model = match action with
      (*
      | Action.Update (side, str) ->
        begin
          match side with
            | Left -> { left_input = str; result; right_input }
            | Right -> { left_input; result; right_input = str }
        end
      *)
      | Action.Evaluate str ->
        begin
          let parser = match side with
            | Left -> LambdaParse.t
            | Right -> TermParse.t
          in
          let result = parse parser str in
          let other_side_str = match result, side with
            | Ok tm, Left -> Fmt.str "%a" term_pretty tm
            | Ok tm, Right -> Fmt.str "%a" lambda_pretty tm
            | Error _, Left -> right_input
            | Error _, Right -> left_input
          in
          let selected = None in
          match side with
            | Left -> { left_input = str; right_input = other_side_str; result; side; selected }
            | Right -> { left_input = other_side_str; right_input = str; result; side; selected }
        end
      | Select (start, finish) ->
        let selected = Some Range.{ start; finish } in
        { left_input; right_input; result; side; selected }
      | SwitchSides ->
        let side', result' = match side with
          | Left -> Right, parse TermParse.t right_input
          | Right -> Left, parse LambdaParse.t left_input
        in
        { left_input; right_input; side = side'; selected = None; result = result' }
    in
    f new_model
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

  let mk_input side ((r, _f) as react_pair : react_pair) =
    let input = r
      |> React.S.map (fun m -> match side with
        | Left -> m.Model.left_input
        | Right -> m.Model.right_input)
      |> fun (value : string React.signal) -> R.Html5.(textarea
        ~a:[
          a_rows (React.S.const 25);
          a_cols (React.S.const 60);
          a_autofocus ();
          a_style (React.S.const "display: block; border: 2px solid; margin: 20px; padding: 2em;");
        ]
        (React.S.const (txt value)))
    in
    let input_dom = To_dom.of_textarea input in

    bind_event Ev.keydowns input_dom (fun evt ->
      let key = evt##.key |> Js.Optdef.to_option |> Option.map ~f:Js.to_string in
      Lwt.return (match key with
        (* TODO: require special key *)
        | Some "Enter"
        -> Controller.update (Evaluate (Js.to_string input_dom##.value)) react_pair
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
      Controller.update (Select (start, finish)) react_pair;
      (* Used for debugging only -- can be removed: *)
      Caml.Printf.printf "Selected %u-%u '%s'\n" start finish (Js.to_string str);
      Lwt.return ()
      );

    input

  let mk_output _side ((r, _f) : react_pair) =
    let range_signal : Range.t React.signal = r
      |> React.S.map (fun Model.{ selected; _ } -> match selected with
        | Some r -> r
        | None -> Range.mk 0 0)
    in
    let code : [> `Code ] Html5.elt React.signal = r
    |> React.S.map (fun m ->
        let elt, formatter = mk_range_formatter range_signal in
        begin
          match m.Model.result, m.Model.side with
            | Ok tm, Left -> Fmt.pf formatter "%a" term_pretty tm
            | Ok tm, Right -> Fmt.pf formatter "%a" lambda_pretty tm
            | Error msg, Left
            | Error msg, Right -> Fmt.pf formatter "%s" msg
        end;
        Fmt.flush formatter ();
        elt
        )
    in

    R.Html5.div
      ~a:[
        R.Html5.a_style (React.S.const "margin: 20px; padding: 1em; background-color: hsl(0 0% 95% / 1);");
      ]
      (RList.singleton_s code)

  let info ((r, _f) : react_pair) = r
    |> React.S.map (fun m -> match m.Model.result with
      | Ok _tm -> "(parsed)"
      | Error err -> err
    )
    |> fun (msg : string React.signal) -> Html5.div [ R.Html5.txt msg ]

  let mk_side side (react_pair : react_pair) =
    (* TODO: remove duplication *)
    let h3_label_s = react_pair
      |> fst
      |> React.S.map
        (fun m -> if Caml.(side = m.Model.side) then "input" else "output")
    in
    let contents_s = react_pair
      |> fst
      |> React.S.map (fun m ->
        if Caml.(side = m.Model.side)
        then mk_input side react_pair
        else mk_output side react_pair)
    in
    Html5.(div
      ~a:[a_style "width: 50%"]
      [ h3 ~a:[a_style "margin: 20px"] [ R.Html5.txt h3_label_s ]
      ; react_pair
        |> fst
        |> React.S.map (fun m ->
          if Caml.(side = m.Model.side)
          then txt ""
          else button
            ~a:[a_onclick (fun _evt -> Controller.update SwitchSides react_pair; false)]
            [ txt "switch to this side" ])
        |> RList.singleton_s
        |> R.Html5.div
      ; R.Html5.div (RList.singleton_s contents_s)
      ])

  (* let mk_header side *)

  let view (react_pair : react_pair) = Html5.(div
    [ h2 [ txt "Term / concrete" ]
    ; div ~a:[a_style "display: flex; flex-direction: row; max-width: 1200px;"]
      [ mk_side Left react_pair
      ; mk_side Right react_pair
      ]
    ; info react_pair
    ])
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app"))
      (fun () -> assert false)
  in
  let m = Model.initial_model in
  let react_pair = React.S.create m in
  Dom.appendChild parent
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view react_pair));
  Lwt.return ()

let (_ : unit Lwt.t) = Lwt.Infix.(Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main)
