open Lvca_syntax
open ReactiveData
open Base
open Js_of_ocaml_tyxml.Tyxml_js

module Ev = Js_of_ocaml_lwt.Lwt_js_events
module React = SafeReact

let a_class, pre, span, txt, td, tr = Html.(a_class, pre, span, txt, td, tr)

type var_status =
  | Unselected
  | Selected
  | Shadowed
  | Shadowing

type default_expanded_depth
  = ExpandedTo of int
  | FullyExpanded

let decrease_depth = function
  | ExpandedTo 0 -> ExpandedTo 0
  | ExpandedTo n -> ExpandedTo (n - 1)
  | FullyExpanded -> FullyExpanded

type 'a signal =
  { signal: 'a React.signal
  ; set_s: 'a -> unit
  }

let create_s ~eq init =
  let signal, set_s = React.S.create ~eq init in
  { signal; set_s }

type 'a event =
  { event : 'a React.event
  ; trigger : 'a -> unit
  }

let create_e () =
  let event, trigger = React.E.create () in
  { event; trigger }

type definition_streams =
  { trigger_upstream_shadow: var_status -> unit
  (** Alert upstream (a var that we're shadowing) to light up *)
  ; trigger_downstream_shadow: var_status -> unit
  (** Alert downstream (a var that shadows us) to light up *)
  }

type render_params =
  { source_column: bool
  (** Show the "source" column or not *)
  ; range_column: bool
  (** Show the "range" column or not *)
  ; depth: int
  ; var_selected_events: var_status event Lvca_util.String.Map.t
  ; queue: ([ `Tr ] Html.elt * SourceRanges.t React.event) Queue.t
  }

type var_pos =
  | Reference
  (** This variable is a reference to a binding site *)
  | Definition of definition_streams
  (** This variable is a binding site *)

type expanded = bool

type term_index =
  | OperatorIx of SourceRange.t option * expanded signal
  | VarDefIx of SourceRange.t option * var_status event
  | LocIx of SourceRange.t option

let select_source_range : SourceRanges.t -> SourceRange.t option
  = fun source_ranges -> match Map.to_alist source_ranges with
    | [source, [range]] -> Some { source; range }
    | _ -> None

let in_scope_cls = "bg-green-50"
let reference_cls = "bg-blue-200"
let definition_cls = "bg-pink-200"
let upstream_shadow_cls = "bg-yellow-200"
let downstream_shadow_cls = "bg-yellow-500"

let grid_tmpl ~render_params left loc
  : [> Html_types.tr ] Html.elt * SourceRanges.t React.event =
  let { var_selected_events; source_column; range_column; _ } = render_params in
  let classes = var_selected_events
    |> Map.to_alist
    (* Highlight background if any variable in scope is selected *)
    |> List.map ~f:(fun (_k, { event; _ }) -> event
      |> React.E.map (function Selected -> true | _ -> false)
    )
    |> React.E.select
    |> React.S.hold ~eq:Bool.(=) false
    |> React.S.map ~eq:(List.equal Caml.(=)) (function
      | true -> [in_scope_cls; "col-span-2"; "px-2"; "py-0"]
      | false -> ["col-span-2"; "px-2"; "py-0"]
    )
  in

  let fst_col = td ~a:[R.Html.a_class classes] left in
  let cols = match loc with
    | None -> [ fst_col ]
    | Some SourceRange.{ source; range } ->
      [ Some fst_col
      ; if source_column
        then Some (td ~a:[a_class ["px-2"; "py-0"]] [txt source])
        else None
      ; if range_column
        then Some (td ~a:[a_class ["px-2"; "py-0"]] [txt (Range.to_string range)])
        else None
      ]
      |> List.filter_map ~f:Fn.id
  in

  let tr = tr cols in
  let tr_elem = To_dom.of_tr tr in

  let evt, trigger_evt = React.E.create () in

  (match loc with
    | None -> ()
    | Some loc ->
      Common.bind_event Ev.mouseovers tr_elem (fun _evt ->
        trigger_evt (SourceRanges.of_source_range loc);
        Lwt.return ()
      );

      Common.bind_event Ev.mouseouts tr_elem (fun _evt ->
        trigger_evt SourceRanges.empty;
        Lwt.return ()
      )
  );

  tr, evt

let indent _ = span ~a:[a_class ["border-l-2"; "border-dotted"]] [txt "  "]

let padded_txt depth text =
  let indents = List.init depth ~f:indent in
  pre ~a:[a_class ["inline-block"]] (Lvca_util.List.snoc indents (txt text))

let render_var ~render_params ~var_pos ~suffix ~selected_event ~loc ~name : unit =
  let { depth; queue; _ } = render_params in
  let { event = selected_event; trigger = trigger_selected } = selected_event in

  let classes_s = selected_event
    |> React.S.hold ~eq:Caml.(=) Unselected
    |> React.S.map ~eq:(List.equal String.(=)) (fun evt ->
      let cls = match evt, var_pos with
        | Selected, Reference -> [reference_cls]
        | Selected, Definition _ -> [definition_cls]
        | Shadowed, Definition _ -> [upstream_shadow_cls]
        | Shadowing, Definition _ -> [downstream_shadow_cls]
        | _, _ -> []
      in
      "inline-block" :: cls
    )
  in

  let indents = List.init depth ~f:indent in
  let name' = span
    [ span ~a:[R.Html.a_class classes_s] [txt name]
    ; txt suffix
    ]
  in
  let name_elem = To_dom.of_span name' in

  let trigger_upstream_shadow, trigger_downstream_shadow = match var_pos with
    | Definition { trigger_upstream_shadow; trigger_downstream_shadow } ->
      trigger_upstream_shadow, trigger_downstream_shadow
    | Reference -> Fn.const (), Fn.const ()
  in

  Common.bind_event Ev.mouseovers name_elem (fun _evt ->
    trigger_downstream_shadow Shadowing;
    trigger_upstream_shadow Shadowed;
    trigger_selected Selected;
    Lwt.return ()
  );

  Common.bind_event Ev.mouseouts name_elem (fun _evt ->
    trigger_upstream_shadow Unselected;
    trigger_downstream_shadow Unselected;
    trigger_selected Unselected;
    Lwt.return ()
  );

  let left_col = pre (Lvca_util.List.snoc indents name') in
  Queue.enqueue queue (grid_tmpl ~render_params [left_col] loc)

let get_suffix ~last_slot ~last_term = match last_term, last_slot with
  | true, true -> ""
  | true, false -> ";"
  | false, _ -> ","

let rec index_pat = function
  | Pattern.Primitive (loc, p) -> Pattern.Primitive (LocIx loc, p)
  | Var (loc, name) -> Var (VarDefIx (loc, create_e ()), name)
  | Ignored (loc, name) -> Ignored (LocIx loc, name)
  | Operator (loc, name, slots) ->
    let slots = List.map slots ~f:(List.map ~f:index_pat) in
    Operator (LocIx loc, name, slots)

let rec index_tm ~expanded_depth = function
  | Nominal.Primitive (loc, p) -> Nominal.Primitive (LocIx loc, p), React.E.never
  | Var (loc, name) -> Var (LocIx loc, name), React.E.never
  | Operator (loc, name, scopes) ->
    let scopes, expanded_s, expanded_toggle_e =
      if not (List.is_empty scopes)
      then (
        let scopes, children_expanded_toggle_e = scopes
          |> List.map ~f:(index_scope ~expanded_depth)
          |> List.unzip
          |> Lvca_util.Tuple2.map2 ~f:React.E.select
        in

        let starts_expanded = match expanded_depth with
          | FullyExpanded -> true
          | ExpandedTo n -> n > 0
        in
        let expanded_s = create_s ~eq:Bool.(=) starts_expanded in
        let expanded_toggle_e = React.S.diff (fun _ _ -> ()) expanded_s.signal in
        let expanded_toggle_e =
          React.E.select [ expanded_toggle_e; children_expanded_toggle_e ]
        in
        scopes, expanded_s, expanded_toggle_e
      )
      else [], (* not actually used *) create_s true ~eq:Bool.(=), React.E.never
    in

    Operator (OperatorIx (loc, expanded_s), name, scopes), expanded_toggle_e

and index_scope ~expanded_depth (Nominal.Scope (pats, tms)) =
  let expanded_depth = decrease_depth expanded_depth in
  let tms, evt = tms
    |> List.map ~f:(index_tm ~expanded_depth)
    |> List.unzip
    |> Lvca_util.Tuple2.map2 ~f:React.E.select
  in
  let pats = pats |> List.map ~f:index_pat in
  Nominal.Scope (pats, tms), evt

let rec find_outermost_binding ~var_name = function
  | Nominal.Primitive _ | Var _ -> None
  | Operator (_, _, scopes) ->
    List.find_map scopes ~f:(find_outermost_binding_scope ~var_name)

and find_outermost_binding_scope ~var_name (Scope (pats, body)) =
  let found_var = pats
    |> List.find_map ~f:(fun pat -> pat
      |> Pattern.list_vars_of_pattern
      |> List.find_map ~f:(fun (info, name) -> match String.(name = var_name), info with
        | true, VarDefIx (_, evt) -> Some evt
        | _, _ -> None
      )
    )
  in
  match found_var with
    | Some v -> Some v
    | None -> List.find_map body ~f:(find_outermost_binding ~var_name)

let rec render_pattern ~render_params ~shadowed_var_streams ~suffix ~downstream
  : _ Pattern.t -> unit
  = let { depth; queue; _ } = render_params in
    function
  | Pattern.Primitive (LocIx loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl ~render_params [padded_txt depth str] loc)
  | Var (VarDefIx (loc, selected_event), name) ->

    let trigger_upstream_shadow = match Map.find shadowed_var_streams name with
      | None -> Fn.const ()
      | Some event_stream -> event_stream.trigger
    in
    let trigger_downstream_shadow =
      match List.find_map downstream ~f:(find_outermost_binding ~var_name:name) with
        | None -> Fn.const ()
        | Some event_stream -> event_stream.trigger
    in
    let var_pos = Definition { trigger_upstream_shadow; trigger_downstream_shadow } in

    render_var ~render_params ~var_pos ~suffix ~selected_event ~loc ~name

  | Ignored (LocIx loc, name) ->
    Queue.enqueue queue
      (grid_tmpl ~render_params [padded_txt depth ("_" ^ name ^ suffix)] loc)
  | Operator (LocIx loc, name, slots) -> (match slots with
    | [] -> Queue.enqueue queue
      (grid_tmpl ~render_params [ padded_txt depth (name ^ "()" ^ suffix) ] loc)
    | _ ->
      let open_elem = grid_tmpl ~render_params [ padded_txt depth (name ^ "(") ] loc in
      Queue.enqueue queue open_elem;

      let num_slots = List.length slots in
      List.iteri slots ~f:(fun i pats ->
        let num_pats = List.length pats in
        let last_slot = i = num_slots - 1 in
        List.iteri pats ~f:(fun j ->
          let last_term = j = num_pats - 1 in
          let suffix = get_suffix ~last_slot ~last_term in
          let render_params = { render_params with depth = Int.succ depth } in
          render_pattern ~render_params ~shadowed_var_streams ~suffix ~downstream
        )
      );

      let close_elem = grid_tmpl ~render_params [ padded_txt depth (")" ^ suffix) ] loc in
      Queue.enqueue queue close_elem
  )
  | _ -> failwith "invariant violation: wrong index"

let rec render_tm ~render_params ?suffix:(suffix="") : _ Nominal.term -> unit =
  let { depth; var_selected_events; queue; _ } = render_params in
  function
  | Nominal.Primitive (LocIx loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl ~render_params [padded_txt depth str] loc)
  | Var (LocIx loc, name) ->
    let selected_event = Map.find_exn var_selected_events name in
    render_var ~render_params ~var_pos:Reference ~suffix ~selected_event ~loc ~name
  | Operator (OperatorIx (loc, expanded_signal), name, scopes) ->
    let { signal = expanded_s; set_s = set_expanded } = expanded_signal in
    let button_event, button = Components.chevron_toggle expanded_s in

    let _ : unit React.event = button_event |> React.E.map set_expanded in

    let _: unit React.signal = expanded_s
      |> React.S.map ~eq:Unit.(=) (function
        | false ->
          let left_col = match scopes with
            | [] -> [padded_txt depth (name ^ "()" ^ suffix) ]
            | _ -> [padded_txt depth (name ^ "("); button; txt (")" ^ suffix) ]
          in
          Queue.enqueue queue (grid_tmpl ~render_params left_col loc)
        | true ->
          let open_elem =
            grid_tmpl ~render_params [ padded_txt depth (name ^ "("); button ] loc
          in
          Queue.enqueue queue open_elem;

          List.iteri scopes ~f:(fun i ->
            render_scope ~render_params ~last:(i = List.length scopes - 1));

          let close_elem =
            grid_tmpl ~render_params [ padded_txt depth (")" ^ suffix) ] loc
          in
          Queue.enqueue queue close_elem
      )
    in
    ()
  | _ -> failwith "invariant violation: wrong index"

and render_scope ~render_params ~last:last_slot (Nominal.Scope (pats, tms)) =
  let { depth; var_selected_events; _ } = render_params in
  let pattern_var_events = List.map pats ~f:(fun pat ->
      let newly_defined_vars = Pattern.list_vars_of_pattern pat in
      let newly_defined_var_events = newly_defined_vars
        |> List.map ~f:(fun (info, name) -> match info with
          | VarDefIx (_loc, event) -> name, event
          | _ -> failwith "invariant violation: wrong index"
        )
        |> Lvca_util.String.Map.of_alist_exn
      in

      let shadowed_var_streams = newly_defined_vars
        |> List.filter_map ~f:(fun (_loc, k) -> match Map.find var_selected_events k with
          | None -> None
          | Some evt -> Some (k, evt)
        )
        |> Lvca_util.String.Map.of_alist_exn
      in

      let var_selected_events = Map.merge_skewed ~combine:(fun ~key:_ _l r -> r)
        var_selected_events newly_defined_var_events
      in

      let render_params =
        { render_params with depth = Int.succ depth; var_selected_events }
      in
      render_pattern ~render_params ~shadowed_var_streams ~suffix:"." ~downstream:tms pat;
      newly_defined_var_events
    )
  in

  (* Events for variables bound in this scope *)
  let pattern_var_events = Lvca_util.String.Map.unions_right_biased pattern_var_events in
  let combine ~key:_ _l r = r in

  (* Select events for variables visible in this scope *)
  let var_selected_events =
    Map.merge_skewed ~combine var_selected_events pattern_var_events
  in
  let render_params = { render_params with depth = Int.succ depth; var_selected_events }
  in

  let num_tms = List.length tms in
  List.iteri tms ~f:(fun j ->
    let last_term = j = num_tms - 1 in
    let suffix = get_suffix ~last_term ~last_slot in
    render_tm ~render_params ~suffix
  )

let view_tm
  ?source_column:(source_column=true)
  ?range_column:(range_column=true)
  ?default_expanded_depth:(expanded_depth=FullyExpanded)
  tm =
  let tm = Nominal.map_loc ~f:select_source_range tm in

  (* First, create a stream for all free variables actions on them will work
     like normal. *)
  let free_vars = Nominal.free_vars tm in

  let var_selected_events = free_vars
    |> Set.to_list
    |> List.map ~f:(fun name -> name, create_e ())
    |> Lvca_util.String.Map.of_alist_exn
  in

  let tm, visible_toggle_e = index_tm ~expanded_depth tm in

  let selection_e, set_selection = React.E.create () in

  let render () =
    let queue = Queue.create () in

    let render_params =
      { var_selected_events; queue; depth = 0; source_column; range_column }
    in
    render_tm ~render_params tm;
    let rows, evts = queue |> Queue.to_list |> List.unzip in
    let tbody = rows |> Html.tbody in

    let _ : unit React.event = evts
      |> React.E.select
      |> React.E.map set_selection
    in

    let rows =
      [ Some [%html{|<td class="p-2 border-t-2 border-b-2 border-r-2 w-1/2">term</td>|}]
      ; if source_column
        then Some [%html{|<td class="p-2 border-t-2 border-b-2 border-r-2">source</td>|}]
        else None
      ; if range_column
        then Some [%html{|<td class="p-2 border-t-2 border-b-2">range</td>|}]
        else None
      ]
      |> List.filter_map ~f:Fn.id
    in

    if source_column || range_column
    then
      [%html{|
      <table class="w-full table-fixed border-2 cursor-default font-mono">
        <thead> <tr>|}rows{|</tr> </thead>
        |}[tbody]{|
      </table>
      |}]
    else
      [%html{|
      <table class="w-full table-fixed border-2 cursor-default font-mono">
        |}[tbody]{|
      </table>
      |}]
  in

  (* Any signal change coming from render is a real update, don't bother with
     equality testing. *)
  let eq _ _ = false in

  let elem = visible_toggle_e
    |> React.S.hold ~eq:Unit.(=) ()
    |> React.S.map ~eq render
    |> RList.singleton_s
    |> R.Html.div
  in

  elem, selection_e
