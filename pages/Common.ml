open Base
open Lvca_syntax
open ReactiveData
module Ev = Js_of_ocaml_lwt.Lwt_js_events
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js

type term = (OptRange.t, Primitive.t) Nominal.term

module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

module Action = struct
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

let lambda_pretty = Lvca_languages.LambdaCalculus.pp (* XXX why used twice? *)

let bind_event ev elem handler =
  let handler evt _ = handler evt in
  Ev.async @@ fun () -> ev elem handler
;;

let demo_template handler input_desc input_elem output_desc output_elem =
  let open Tyxml_js in
  [%html{|
    <div>
      <h2>Eval with Provenance</h2>
      <div class="container">
        <div class="side">
          <h3>|}[ input_desc ]{|</h3>
          |}[ input_elem ]{|
        </div>
        <div class="switch-languages">
          <button onclick=|}handler{|>
            switch input languages
          </button>
        </div>
        <div class="side">
          <h3>|}[ output_desc ]{|</h3>
          |}[ output_elem ]{|
        </div>
      </div>
    </div>
  |}] [@@@ocamlformat "disable"]

type input_event =
  | InputUpdate of string
  | InputSelect of int * int
  | InputUnselect

let mk_multiline_input
  ?autofocus:(autofocus=true)
  ?rows:(rows=None)
  ?cols:(cols=60)
  input_s =
  let open Js_of_ocaml in
  let open Tyxml_js in

  let input_event, signal_event = React.E.create () in

  let needed_rows = match rows with
    | Some n -> n
    | None ->
      String.count (React.S.value input_s) ~f:(fun c -> Char.(c = '\n')) + 1
  in

  let input = Html.(textarea
    ~a:([ a_rows needed_rows
        ; a_cols cols
        ; a_class ["input"]
        ] @ (if autofocus then [a_autofocus ()] else []))
     (R.Html.txt input_s)
    )
  in
  let input_dom = To_dom.of_textarea input in
  bind_event Ev.keydowns input_dom (fun evt ->
      Lwt.return
        (if WebUtil.is_special_enter evt
        then (
          Dom.preventDefault evt;
          signal_event (InputUpdate (Js.to_string input_dom##.value))
        )
        else ()));
  bind_event Ev.selects input_dom (fun evt ->
      let elem = evt##.target |> Js.Opt.to_option |> Option.value_exn in
      let textarea =
        elem |> Dom_html.CoerceTo.textarea |> Js.Opt.to_option |> Option.value_exn
      in
      let start = textarea##.selectionStart in
      let finish = textarea##.selectionEnd in
      signal_event (InputSelect (start, finish));
      (* Used for debugging only -- can be removed: *)
      (* let str = textarea##.value##substring start finish in *)
      (* Caml.Printf.printf "Selected %u-%u '%s'\n" start finish (Js.to_string str); *)
      Lwt.return ());
  bind_event Ev.clicks input_dom (fun _evt ->
      signal_event InputUnselect;
      Lwt.return ());
  (* XXX why doesn't the textarea automatically update? *)
  let (_ : unit React.event) =
    input_s
    (* Create an event when the input has changed *)
    |> React.S.map ~eq:Caml.(=) Fn.id
    |> React.S.changes
    |> React.E.map (fun input ->
           (* Caml.Printf.printf "Updating input: %s\n" input; *)
           input_dom##.value := Js.string input)
  in
  input, input_event

let mk_single_line_input ?autofocus:(autofocus=true) input_s =
  let open Js_of_ocaml in
  let open Tyxml_js in

    let input_event, signal_input_event = React.E.create () in

    let input_value = React.S.value input_s in

    let input = Html.(input
      ~a:(
        [a_input_type `Text ; a_value input_value] @
        (if autofocus then [a_autofocus ()] else []))
      ()
    )
    in
    let input_dom = To_dom.of_input input in

    bind_event Ev.keydowns input_dom (fun evt ->
      let key_name = evt##.code
        |> Js.Optdef.to_option
        |> Option.value_exn
        |> Js.to_string
      in
      let result = match key_name with
        | "Enter" -> (
          Dom.preventDefault evt;
          signal_input_event (Js.to_string input_dom##.value)
        )
        | _ -> ()
      in
      Lwt.return result
    );

    let (_: unit React.event) = input_s
      (* Create an event when the input has changed *)
      |> React.S.map ~eq:Caml.(=) Fn.id
      |> React.S.changes
      |> React.E.map (fun s -> input_dom##.value := Js.string s)
    in
    input, input_event

let mk_output
  (elt_s : [ `Code ] Tyxml_js.To_dom.elt React.signal) =
  let open Tyxml_js in
  R.Html.div
    ~a:[ R.Html.a_class (React.S.const [ "output" ]) ]
    (RList.singleton_s elt_s)

type digits_update =
  | SetDigits of int
  | IncrDigits
  | DecrDigits

let mk_digits_entry digits_s =
    let open Js_of_ocaml in
    let open Tyxml_js in

    let digits_event, signal_digits_event = React.E.create () in
    let input_value = Int.to_string (React.S.value digits_s) in
    let input = [%html{|<input type="text" value=|}input_value{|>|}] in
    let input_dom = To_dom.of_input input in

    bind_event Ev.keydowns input_dom (fun evt ->
      let key_name = evt##.code
        |> Js.Optdef.to_option
        |> Option.value_exn
        |> Js.to_string
      in
      let result = match key_name with
        | "Enter" -> (
         Dom.preventDefault evt;
         try
           signal_digits_event
             (SetDigits (Int.of_string (Js.to_string input_dom##.value)))
         with
           _ -> ()
        )
        | "ArrowUp" | "ArrowRight" ->
          Dom.preventDefault evt;
          signal_digits_event IncrDigits
        | "ArrowDown" | "ArrowLeft" ->
          Dom.preventDefault evt;
          signal_digits_event DecrDigits
        | _ -> ()
      in
      Lwt.return result
    );

    let (_: unit React.event) = digits_s
      (* Create an event when the input has changed *)
      |> React.S.map ~eq:Caml.(=) Fn.id
      |> React.S.changes
      |> React.E.map (fun i -> input_dom##.value := Js.string (Int.to_string i))
    in
    input, digits_event
