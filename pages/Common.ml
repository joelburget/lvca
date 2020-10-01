open Base
open Lvca_syntax
open ReactiveData

type term = (OptRange.t, Primitive.t) Nominal.term

type lang =
  | Lambda
  | Term

module PrimitiveParse = Primitive.Parse (ParseUtil.NoComment)
module TermParse = Nominal.Parse (ParseUtil.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

module Action = struct
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

let parser_of = function Lambda -> LambdaParse.t | Term -> TermParse.t PrimitiveParse.t
let term_pretty = Nominal.pp_term_range Primitive.pp (* XXX why used twice? *)

let lambda_pretty = Lvca_languages.LambdaCalculus.pp (* XXX why used twice? *)

let bind_event ev elem handler =
  let handler evt _ = handler evt in
  Js_of_ocaml_lwt.Lwt_js_events.async @@ fun () -> ev elem handler
;;

let demo_template handler input_desc input_elem output_desc output_elem =
  let open Js_of_ocaml_tyxml.Tyxml_js in
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

let mk_input input_s =
  let open Js_of_ocaml in
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let module Ev = Js_of_ocaml_lwt.Lwt_js_events in

  let input_event, signal_event = React.E.create () in

  let input_val = R.Html.txt input_s in
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

let mk_output
  (elt_s : [ `Code ] Js_of_ocaml_tyxml.Tyxml_js.To_dom.elt React.signal) =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  R.Html.div
    ~a:[ R.Html.a_class (React.S.const [ "output" ]) ]
    (RList.singleton_s elt_s)
