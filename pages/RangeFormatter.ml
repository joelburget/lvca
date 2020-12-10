open Base
open Lvca_syntax
open ReactiveData
module Ev = Js_of_ocaml_lwt.Lwt_js_events
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

(** The incoming signal holds the currently selected range. We return both a Dom element
    (<code>) to be inserted in the Dom and the formatter which is used to write formatted
    text to this element. Note that the returned Dom element is empty until the formatter
    is used and flushed. *)
let mk
    :  selection_s:(SourceRanges.t SafeReact.signal)
    -> set_selection:(SourceRanges.t -> unit)
    -> [> `Code ] Js_of_ocaml_tyxml.Tyxml_js.Html5.elt * Caml.Format.formatter * (unit -> unit)
  =
 fun ~selection_s:externally_selected_s ~set_selection ->
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let br, span, txt = Html.(br, span, txt) in
  let top_level_elems, top_level_handle = RList.create [] in
  let clear () = RList.set top_level_handle [] in
  (* - Push a new range and queue on the stack every time we enter a semantic tag - Pop
     every time we exit a tag - The queue is used to add elements (text and nested
     children) under this element - The range is the extent of this element, used to
     update the style when text is selected *)
  let stack : (SourceRanges.t * [> `Span ] Html5.elt Queue.t) Stack.t = Stack.create () in

  let start_range = ref None in

  let add_at_current_level elem = match Stack.top stack with
    | None -> RList.snoc elem top_level_handle
    | Some (_, q) -> Queue.enqueue q elem
  in

  let internal_reset_e, trigger_internal_reset = SafeReact.E.create () in

  let selected_s = SafeReact.E.select
    [ SafeReact.S.diff (fun v _ -> v) externally_selected_s
    ; internal_reset_e |> React.E.map (fun () -> SourceRanges.empty)
    ]
    |> SafeReact.S.hold ~eq:SourceRanges.(=) SourceRanges.empty
  in

  (*
  let _ : unit SafeReact.signal = externally_selected_s |> SafeReact.S.map ~eq:Caml.(=)
    (fun rng -> Caml.Printf.printf "externally selected: %s\n" SourceRanges.(to_string rng))
  in

  let _ : unit SafeReact.signal = internal_reset_e
    |> SafeReact.S.hold ~eq:(fun _ _ -> false) ()
    |> SafeReact.S.map ~eq:(fun _ _ -> false) (fun () -> Caml.Printf.printf "internal reset\n")
  in

  let _ : unit SafeReact.signal = selected_s |> SafeReact.S.map ~eq:Caml.(=)
    (fun rng -> Caml.Printf.printf "selected (1): %s\n" SourceRanges.(to_string rng))
  in
  *)

  let get_attrs () =
    match Stack.top stack with
    | None -> []
    | Some (rng, _) ->
      let style = selected_s
        |> SafeReact.S.map ~eq:String.(=) (fun selected_rng ->
          (* Highlight if this is a subset of the selected range *)
          if SourceRanges.is_subset rng selected_rng
          then "background-color: rgba(33 150 243 / 50%);"
          else "")
      in
      (* The data-range attribute is currently used just for debugging, it can be removed. *)
      [ R.Html5.a_style style; Html5.a_user_data "range" (SourceRanges.to_string rng) ]
  in

  let add_text str =
    let span = span ~a:(get_attrs ()) [ txt str ] in
    let span_elem = span |> To_dom.of_span in
    (match Stack.top stack with
      | None ->
        (* Caml.Printf.printf "not binding mouse events (%s)\n" str; *)
        ()
      | Some (rng, _) ->
        Common.bind_event Ev.mousedowns span_elem (fun _evt ->
          (* Caml.Printf.printf "down: %s\n" (SourceRanges.to_string rng); *)
          start_range := Some rng;
          Lwt.return ()
        );
        Common.bind_event Ev.mouseups span_elem (fun _evt ->
          (match !start_range with
            | Some start ->
              start_range := None;
              let str = Js.to_string Dom_html.window##getSelection##toString in
              let rng = if String.(str <> "")
                then SourceRanges.union start rng
                else SourceRanges.empty
              in
              set_selection rng;

              (* Clear the selection *)
              trigger_internal_reset ();
              ()
            | None -> () (* Error *)
          );
          Lwt.return ()
        );
    );
    add_at_current_level span
  in
  let add_spaces n = add_text (String.make n ' ') in

  let out_fns : Caml.Format.formatter_out_functions =
    { out_string =
        (fun str _start _char_count -> add_text str)
        (* No need to do anything -- we update the element immediately on receiving
           characters. *)
    ; out_flush = Fn.id
    ; out_newline = (fun () -> add_at_current_level (br ()))
    ; out_spaces = add_spaces
    ; out_indent = add_spaces
    }
  in

  let fmt = Caml.Format.formatter_of_out_functions out_fns in
  Caml.Format.pp_set_tags fmt true;
  Caml.Format.pp_set_formatter_stag_functions
    fmt
    (* We open a new span for every range tag we encounter. All children until we
       encounter the matching close tag will be nested under it (by enqueuing). *)
    { mark_open_stag =
        (function
          (*
        | Range.Stag rng ->
          Caml.Printf.printf "opening Range: %s\n" (Range.to_string rng);
          Stack.push stack (Some rng, Queue.create ());
          ""
        | SourceRange.Stag rng ->
          Caml.Printf.printf "opening SourceRange: %s\n" (SourceRange.to_string rng);
          ""
          *)
        | SourceRanges.Stag rng ->
          (* Caml.Printf.printf "opening SourceRanges: %s\n" (SourceRanges.to_string rng); *)
          Stack.push stack (rng, Queue.create ());
          ""
        | _ -> "")
        (* Closing a range; create the span holding all of the enqueued children. *)
    ; mark_close_stag =
        (fun _ ->
          (* Caml.Printf.printf "closing range\n"; *)
          (match Stack.pop stack with
          | None -> ()
          | Some (_, q) -> q |> Queue.to_list |> span |> add_at_current_level);
          "")
    ; print_open_stag = Fn.const ()
    ; print_close_stag = Fn.const ()
    };
  Html.pre [R.Html.code top_level_elems], fmt, clear
;;
