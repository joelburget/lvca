open Base
open Brr
open Brr_note
open Note
open Prelude

type digits_update =
  | SetDigits of int
  | IncrDigits
  | DecrDigits

let mk digits_s =
  (* TODO: inputmode="decimal"
     * https://css-tricks.com/better-form-inputs-for-better-mobile-user-experiences/
     * https://github.com/ocsigen/tyxml/issues/278
   *)
  let at =
    At.[ type' (Jstr.v "text"); inputmode "numeric" ]
    @ classes "font-mono border-2 border-indigo-900 rounded p-1 focus:ring w-16"
  in
  let input = El.input ~at () in
  let digits_s = digits_s |> S.map (fun str -> Some (Jstr.v (Int.to_string str))) in
  let () = Elr.def_at (Jstr.v "value") digits_s input in
  let digits_event =
    Evr.on_el
      Ev.keydown
      (fun evt ->
        let key_evt = Ev.as_type evt in
        let key_name = key_evt |> Ev.Keyboard.key |> Jstr.to_string in
        match key_name with
        | "Enter" ->
          Ev.prevent_default evt;
          (try
             Some
               (SetDigits
                  (input
                  |> El.at At.Name.value
                  |> Option.value_exn
                  |> Jstr.to_string
                  |> Int.of_string))
           with
          | _ -> None)
        | "ArrowUp" | "ArrowRight" ->
          Ev.prevent_default evt;
          Some IncrDigits
        | "ArrowDown" | "ArrowLeft" ->
          Ev.prevent_default evt;
          Some DecrDigits
        | _ -> None)
      input
    |> E.Option.on_some
  in
  input, digits_event
;;
