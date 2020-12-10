open Base
open Lvca_syntax
open ReactiveData
module Ev = Js_of_ocaml_lwt.Lwt_js_events
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js

type term = (OptRange.t, Primitive.t) Nominal.term

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
      <h2>Demo</h2>
      <div class="container">
        <div class="py-4">
          <h3>|}[ input_desc ]{|</h3>
          |}[ input_elem ]{|
        </div>
        <div class="switch-languages">
          <button
            class="p-2 border-2 border-indigo-900 rounded"
            onclick=|}handler{|>
            switch input languages
          </button>
        </div>
        <div class="py-4">
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

let mk_output elt_s =
  let open Tyxml_js in
  R.Html.div
    ~a:[ Html.a_class [ "bg-gray-100"; "p-1" ] ]
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
    (* TODO: inputmode="decimal"
       * https://css-tricks.com/better-form-inputs-for-better-mobile-user-experiences/
       * https://github.com/ocsigen/tyxml/issues/278
     *)
    let input = [%html{|
       <input class="font-mono border-2 border-indigo-900 rounded p-1 focus:ring w-16"
              type="text"
              inputmode="numeric"
              value=|}input_value{|
       >|}]
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
