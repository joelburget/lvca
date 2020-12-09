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

let mk_multiline_input
  ?autofocus:(autofocus=true)
  ?border:(border=true)
  ?rows:(rows=None)
  ?cols:(cols=60)
  input_s =
  let open Js_of_ocaml in
  let open Tyxml_js in

  let input_dirty_s, update_input_dirty = React.S.create false in
  let input_event, signal_event = React.E.create () in

  let needed_rows = match rows with
    | Some n -> n
    | None ->
      String.count (React.S.value input_s) ~f:(fun c -> Char.(c = '\n')) + 1
  in

  let input_dirty_elem = input_dirty_s
    |> React.S.map (function
      | true -> (match WebUtil.platform_special_combo () with
        | Some info_elems -> Html.(span (List.concat
          [ [ txt "updated, press " ]
          ; info_elems
          ; [ txt " to re-evaluate)" ]
          ]))
        | None -> Html.txt "updated (press Enter to re-evaluate)"
      )
      | false -> Html.txt ""
    )
  in

  let classes = List.filter_map ~f:Fn.id
    [ Some "mt-4"
    ; Some "p-8"
    ; Some "font-mono"
    ; if border then Some "border-2" else None
    ; if border then Some "border-indigo-900" else None
    ]
  in

  let input = Html.(textarea
    ~a:([ a_rows needed_rows
        ; a_cols cols
        ; a_class classes
        ] @ (if autofocus then [a_autofocus ()] else []))
     (R.Html.txt input_s)
    )
  in
  let input_dom = To_dom.of_textarea input in

  bind_event Ev.inputs input_dom (fun _evt ->
    update_input_dirty true;
    Lwt.return ()
  );

  bind_event Ev.keydowns input_dom (fun evt ->
      Lwt.return
        (if WebUtil.is_special_enter evt
        then (
          Dom.preventDefault evt;
          update_input_dirty false;
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
    |> React.E.map (fun input -> input_dom##.value := Js.string input)
  in

  let result = [%html{|
    <div class="flex flex-col">
      |}[input]{|
      |}[R.Html.span ~a:[Html.a_class ["my-2"]] (RList.singleton_s input_dirty_elem)]{|
    </div>
    |}]
  in

  result, input_event

let mk_single_line_input
  ?autofocus:(autofocus=false)
  ?highlights_s:(highlights_s=React.S.const [])
  input_s =
  let open Js_of_ocaml in
  let open Tyxml_js in

  let updated_s, update_updated = React.S.create false in

  let input_event, signal_input_event = React.E.create () in

  let input_value = React.S.value input_s in

  let a = List.filter_map ~f:Fn.id Html.
    [ Some (a_input_type `Text)
    (* ; inputmode |> Option.map ~f:a_inputmode *)
    ; Some (a_value input_value)
    ; Some (a_class
      [ "font-mono"
      (* ; "border-2" *)
      (* ; "border-indigo-900" *)
      (* ; "rounded" *)
      ; "p-1"
      (* ; "focus:ring" *)
      ; "bg-none"
      ; "border-none"
      ; "display-block";
      ])
    ; if autofocus then Some (a_autofocus ()) else None
    ]
  in

  let input = Html.input ~a () in

  let highlighted_input_s = React.S.l2
    (fun input_str highlight_ranges -> Ranges.mark_string highlight_ranges input_str
       |> List.map ~f:(fun string_status ->
         let Range.{ start; finish } =
           match string_status with Covered rng | Uncovered rng -> rng
         in
         let str = String.sub input_str ~pos:start ~len:(finish - start) in
         Html.(match string_status with
           | Covered _ -> span ~a:[a_class ["bg-pink-200"; "rounded"]] [txt str]
           | Uncovered _ -> txt str)
       )
    )
    input_s
    highlights_s
    |> RList.from_signal
  in

  (* TODO: sync scroll position *)
  let input_shadow = R.Html.(div
    ~a:[ Html.a_class
         [ "absolute"
         ; "-z-1"
         ; "left-1"
         ; "top-1"
         ; "text-transparent"
         ; "whitespace-pre"
         ]
       ; Html.a_aria "hidden" ["true"]
       ]
    highlighted_input_s
  )
  in

  let updated_elem = updated_s
    |> React.S.map (function
      | true -> "updated (press Enter to re-evaluate)"
      | false -> ""
    )
    |> R.Html.txt
  in

  let result = [%html{|
    <div>
      <div class="relative overflow-hidden b-1">
        |}[input; input_shadow]{|
      </div>
      |}[Html.(span ~a:[a_class ["ml-1"]] [updated_elem])]{|
    </div>
    |}]
  in

  let input_dom = To_dom.of_input input in

  bind_event Ev.inputs input_dom (fun _evt ->
    update_updated true;
    Lwt.return ()
  );

  bind_event Ev.keydowns input_dom (fun evt ->
    let key_name = evt##.code
      |> Js.Optdef.to_option
      |> Option.value_exn
      |> Js.to_string
    in
    let _ : unit = match key_name with
      | "Enter" -> (
        Dom.preventDefault evt;
        update_updated false;
        signal_input_event (Js.to_string input_dom##.value)
      )
      | _ -> ()
    in
    Lwt.return ()
  );

  let (_: unit React.event) = input_s
    (* Create an event when the input has changed *)
    |> React.S.map ~eq:Caml.(=) Fn.id
    |> React.S.changes
    |> React.E.map (fun s -> input_dom##.value := Js.string s)
  in

  result, input_event

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
