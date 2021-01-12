open Base
open Brr
open Brr_note
open Lvca_syntax
open Note
open Stdio
open Prelude
module Format = Caml.Format

type action =
  | Clear
  | Add of El.t

type down_pos =
  | NoDown
  | InitialPadding
  | FinalPadding
  | AtPos of int

type result =
  { elem : El.t
  ; formatter : Format.formatter
  ; selection_e : SourceRanges.t event
  }

let do_action action elems =
  match action with Clear -> [] | Add elem -> Lvca_util.List.snoc elems elem
;;

(** The incoming signal holds the currently selected range. We return both a Dom element
    (<code>) to be inserted in the Dom and the formatter which is used to write formatted
    text to this element. Note that the returned Dom element is empty until the formatter
    is used and flushed. *)
let mk : ?clear:unit event -> selection_s:SourceRanges.t signal -> unit -> result =
 fun ?(clear = E.never) ~selection_s:externally_selected_s () ->
  let br, div, span, txt = El.(br, div, span, txt) in
  let selection_e, set_selection = E.create () in
  let add_elem_e, trigger_add_elem = E.create () in
  let do_action =
    E.select
      [ add_elem_e |> E.map (fun elem -> Add elem); clear |> E.map (fun () -> Clear) ]
    |> E.map do_action
  in
  let top_level_elems = S.accum [] do_action in
  (* - Push a new range and queue on the stack every time we enter a semantic tag - Pop
     every time we exit a tag - The queue is used to add elements (text and nested
     children) under this element - The range is the extent of this element, used to
     update the style when text is selected *)
  let stack : (SourceRanges.t * El.t Queue.t) Stack.t = Stack.create () in
  (* Every time we print a string (in add_text), enqueue the position attached
     to it. Record the indices for mousedown and mouseup events and take the
     union of every location in that range.
   *)
  let positions : SourceRanges.t Queue.t = Queue.create () in
  let selection_start = ref NoDown in
  let add_at_current_level elem =
    match Stack.top stack with
    | None -> trigger_add_elem elem
    | Some (_, q) -> Queue.enqueue q elem
  in
  (* Event triggered on mouseup to clear the (external) selection. *)
  let internal_reset_e, trigger_internal_reset = E.create () in
  let selected_s =
    E.select
      [ S.changes externally_selected_s
      ; internal_reset_e |> E.map (fun () -> SourceRanges.empty)
      ]
    |> S.hold ~eq:SourceRanges.( = ) SourceRanges.empty
  in
  let get_classes () =
    match Stack.top stack with
    | None -> S.const None
    | Some (rng, _) ->
      selected_s
      |> S.map (fun selected_rng ->
             (* Highlight if this is a subset of the selected range *)
             if SourceRanges.is_subset rng selected_rng
             then Some (Jstr.v "highlight")
             else None)
  in
  let get_data_range () =
    match Stack.top stack with
    | None -> "none"
    | Some (rng, _) -> SourceRanges.to_string rng
  in
  let handle_mousedown text_pos _evt =
    Console.log [ Jstr.v "mousedown" ];
    selection_start := text_pos
  in
  let handle_mouseup up_pos _evt =
    Console.log [ Jstr.v "mouseup" ];
    let go down_pos =
      let up_pos =
        match up_pos with
        | AtPos p -> p
        | InitialPadding -> 0
        | FinalPadding -> Queue.length positions
        | NoDown -> assert false
      in
      selection_start := NoDown;
      let selection = Jv.call (Window.to_jv G.window) "getSelection" [||] in
      let selected_str =
        (* TODO: use prelude Window / Selection *)
        Jv.call selection "toString" [||] |> Jv.to_string
      in
      Console.log [ Jstr.v "selected_str"; Jstr.v selected_str ];
      let rng =
        match selected_str with
        | "" -> SourceRanges.empty
        | _ ->
          let pos = Int.min up_pos down_pos in
          let len = Int.abs (up_pos - down_pos) in
          positions |> Queue.to_list |> List.sub ~pos ~len |> SourceRanges.unions
      in
      Console.log [ Jstr.v "rng"; rng |> SourceRanges.to_string |> Jstr.v ];
      set_selection rng;
      (* Clear the selection *)
      trigger_internal_reset ();
      ()
    in
    match !selection_start with
    | NoDown -> ()
    | AtPos down_pos -> go down_pos
    | InitialPadding -> go 0
    | FinalPadding -> go (Queue.length positions)
    (* XXX *)
    (* Error *)
  in
  let add_text str =
    let span =
      span ~at:[ Prelude.data' "range" (get_data_range ()) ] [ txt (Jstr.v str) ]
    in
    let () = Elr.def_at (Jstr.v "class") (get_classes ()) span in
    let () =
      match Stack.top stack with
      | None -> ()
      | Some (rng, _) ->
        let text_pos = Queue.length positions in
        Queue.enqueue positions rng;
        let _sink : Logr.t option =
          let evt = Evr.on_el Ev.mousedown Fn.id span in
          E.log evt (handle_mousedown (AtPos text_pos))
        in
        let _sink : Logr.t option =
          let evt = Evr.on_el Ev.mouseup Fn.id span in
          E.log evt (handle_mouseup (AtPos text_pos))
        in
        ()
    in
    add_at_current_level span
  in
  let add_spaces n =
    match () with
    | () when n > 0 -> add_text (String.make n ' ')
    | () when n < 0 -> printf "add_spaces negative value (!): %d\n" n
    | () -> ()
  in
  let out_fns : Format.formatter_out_functions =
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
  let stag_fns : Format.formatter_stag_functions =
    (* We open a new span for every range tag we encounter. All children until we
       encounter the matching close tag will be nested under it (by enqueuing). *)
    { mark_open_stag =
        (function
        | SourceRanges.Stag rng ->
          Stdio.printf "RangeFormatter opening stag %s\n" (SourceRanges.to_string rng);
          Stack.push stack (rng, Queue.create ());
          ""
        | _ ->
          Stdio.printf "RangeFormatter unknown stag\n";
          "")
        (* Closing a range; create the span holding all of the enqueued children. *)
    ; mark_close_stag =
        (fun _ ->
          match Stack.pop stack with
          | Some (_, q) ->
            q |> Queue.to_list |> span |> add_at_current_level;
            ""
          | None -> "")
    ; print_open_stag = Fn.const ()
    ; print_close_stag = Fn.const ()
    }
  in
  let fmt = Format.formatter_of_out_functions out_fns in
  Format.pp_set_tags fmt true;
  Format.pp_set_formatter_stag_functions fmt stag_fns;
  let code = Prelude.mk_reactive El.code top_level_elems in
  let pre_padding = div ~at:(classes "w-1 h-1") [] in
  let post_padding = div ~at:(classes "w-1 h-1") [] in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.mousedown Fn.id pre_padding in
    E.log evt (handle_mousedown InitialPadding)
  in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.mouseup Fn.id pre_padding in
    E.log evt (handle_mouseup FinalPadding)
  in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.mousedown Fn.id post_padding in
    E.log evt (handle_mousedown InitialPadding)
  in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.mouseup Fn.id post_padding in
    E.log evt (handle_mouseup FinalPadding)
  in
  let elem =
    div ~at:(classes "flex flex-row") [ pre_padding; El.pre [ code ]; post_padding ]
  in
  { elem; formatter = fmt; selection_e }
;;
