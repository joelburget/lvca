open Base
open Lvca_syntax
open ReactiveData

(** The incoming signal holds the currently selected range. We return both a Dom element
    (<code>) to be inserted in the Dom and the formatter which is used to write formatted
    text to this element. Note that the returned Dom element is empty until the formatter
    is used and flushed. *)
let mk
    :  OptRange.t React.signal
    -> [> `Code ] Js_of_ocaml_tyxml.Tyxml_js.Html5.elt * Caml.Format.formatter
  =
 fun rng_signal ->
  let open Js_of_ocaml_tyxml.Tyxml_js in
  let br, span, txt = Html.(br, span, txt) in
  let top_level_elems, top_level_handle = RList.create [] in
  (* - Push a new range and queue on the stack every time we enter a semantic tag - Pop
     every time we exit a tag - The queue is used to add elements (text and nested
     children) under this element - The range is the extent of this element, used to
     update the style when text is selected *)
  let stack : (OptRange.t * [> `Span ] Html5.elt Queue.t) Stack.t = Stack.create () in
  let add_at_current_level elem =
    match Stack.top stack with
    | None -> RList.snoc elem top_level_handle
    | Some (_, q) -> Queue.enqueue q elem
  in
  let get_attrs () =
    match Stack.top stack with
    | None -> []
    | Some (rng, _) ->
      let style =
        rng_signal
        |> React.S.map (fun selected_rng ->
               (* Highlight if this is a subset of the selected range *)
               if OptRange.is_subset rng selected_rng
               then "background-color: rgba(33 150 243 / 50%);"
               else "")
      in
      (* The data-range attribute is currently used just for debugging, it can be removed. *)
      [ R.Html5.a_style style; Html5.a_user_data "range" (OptRange.to_string rng) ]
  in
  let add_text str = add_at_current_level (span ~a:(get_attrs ()) [ txt str ]) in
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
        | Range.Stag rng ->
          Stack.push stack (Some rng, Queue.create ());
          ""
        | _ -> "")
        (* Closing a range; create the span holding all of the enqueued children. *)
    ; mark_close_stag =
        (fun _ ->
          (match Stack.pop stack with
          | None -> ()
          | Some (_, q) -> q |> Queue.to_list |> span |> add_at_current_level);
          "")
    ; print_open_stag = Fn.const ()
    ; print_close_stag = Fn.const ()
    };
  Html.pre [R.Html.code top_level_elems], fmt
;;
