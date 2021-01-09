open Base
open Brr
open Brr_note
open Lvca_syntax
open Note
open Prelude

type term = (OptRange.t, Primitive.t) Nominal.term

let html_eq = Caml.(=)
let htmls_eq = List.equal Caml.(=)

module Action = struct
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

let lambda_pretty = Lvca_languages.LambdaCalculus.pp (* XXX why used twice? *)

let demo_template handler input_desc input_elem output_desc output_elem =
  let button, div, h2, h3 = El.(button, div, h2, h3) in
  let txt str = El.txt (Jstr.v str) in
  let button = button
    ~at:(classes "p-2 border-2 border-indigo-900 rounded")
    [ txt "switch input languages" ]
  in
  let _ : bool event = Evr.on_el Ev.click handler button in

  div
    [ h2 [ txt "Demo" ]
    ; div ~at:[ class' "container" ]
      [ div ~at:[ class' "py-4" ]
        [ h3 [ input_desc ]
        ; input_elem
        ]
      ; div ~at:[ class' "switch-languages" ] [ button ]
      ; div ~at:[ class' "py-4" ]
        [ h3 [ output_desc ]
        ; output_elem
        ]
      ]
    ]

type input_event =
  | InputUpdate of string
  | InputSelect of int * int
  | InputUnselect

let mk_output elt_s =
  let div = El.div ~at:[class' "bg-gray-100"; class' "p-1"] [] in
  let () = Elr.def_children div (elt_s |> S.map (fun elt -> [elt])) in
  div

type digits_update =
  | SetDigits of int
  | IncrDigits
  | DecrDigits

let mk_digits_entry digits_s =
  (* TODO: inputmode="decimal"
     * https://css-tricks.com/better-form-inputs-for-better-mobile-user-experiences/
     * https://github.com/ocsigen/tyxml/issues/278
   *)

  let at = At.
    [ type' (Jstr.v "text")
    ; inputmode "numeric"
    ] @ classes "font-mono border-2 border-indigo-900 rounded p-1 focus:ring w-16"
  in
  let input = El.input ~at () in

  (* TODO: this is different *)
  let digits_s = digits_s |> S.map (fun str -> Some (Jstr.v (Int.to_string str))) in
  let () = Elr.def_at (Jstr.v "value") digits_s input in

  let digits_event = Evr.on_el Ev.keydown (fun evt ->
    let key_evt = Ev.as_type evt in
    let key_name = key_evt |> Ev.Keyboard.key |> Jstr.to_string in
    match key_name with
      | "Enter" -> (
       Ev.prevent_default evt;
       try Some (SetDigits (input
         |> El.at At.Name.value
         |> Option.value_exn
         |> Jstr.to_string
         |> Int.of_string))
       with
         _ -> None
      )
      | "ArrowUp" | "ArrowRight" ->
        Ev.prevent_default evt;
        Some IncrDigits
      | "ArrowDown" | "ArrowLeft" ->
        Ev.prevent_default evt;
        Some DecrDigits
      | _ -> None
    ) input
    |> E.Option.on_some
  in

  (* TODO
  let (_: unit event) = digits_s
    (* Create an event when the input has changed *)
    |> S.map ~eq:Caml.(=) Fn.id
    |> S.changes
    |> E.map (fun i -> input_dom##.value := Js.string (Int.to_string i))
  in
  *)
  input, digits_event
