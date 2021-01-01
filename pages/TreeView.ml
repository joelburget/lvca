open Lvca_syntax
open ReactiveData
open Base
open Js_of_ocaml_tyxml.Tyxml_js

module Ev = Js_of_ocaml_lwt.Lwt_js_events
module React = SafeReact

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

type var_pos =
  | Reference
  | Definition of definition_streams

type expanded = bool

type term_index =
  | OperatorIx of SourceRange.t option * expanded signal
  | VarDefIx of SourceRange.t option * var_status event
  | LocIx of SourceRange.t option

let a_class, pre, span, txt, td, tr = Html.(a_class, pre, span, txt, td, tr)

let cvt_range : SourceRanges.t -> SourceRange.t option
  = fun source_ranges -> match Map.to_alist source_ranges with
    | [source, [range]] -> Some { source; range }
    | _ -> None

let any_var_selected_event selected_events = selected_events
  |> Map.to_alist
  |> List.map ~f:(fun (_k, { event; _ }) -> event
    |> React.E.map (function Selected -> true | _ -> false)
  )

let grid_tmpl ?scope_selection_e:(scope_selection_e=Lvca_util.String.Map.empty)
  ~source_column left loc =
  let classes = scope_selection_e
    |> any_var_selected_event
    |> React.E.select
    |> React.S.hold ~eq:Bool.(=) false
    |> React.S.map ~eq:(List.equal Caml.(=)) (function
      | true -> ["bg-green-50"; "col-span-2"; "px-2"; "py-0"]
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
      ; Some (td ~a:[a_class ["px-2"; "py-0"]] [txt (Range.to_string range)])
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

(* TODO: move into struct *)
(* Streams:
  - This var
  - Possibly a var this var shadows
  - Possibly a var that shadows this var
  *)
let show_var ~var_pos ~scope_selection_e ~queue ~source_column ~suffix
  ~selected_event ~depth ~loc ~name =
  let { event = selected_event; trigger = trigger_selected } = selected_event in

  let classes_s = selected_event
    |> React.S.hold ~eq:Caml.(=) Unselected
    |> React.S.map ~eq:(List.equal String.(=)) (fun evt -> match evt, var_pos with
      | Selected, Reference -> ["inline-block"; "bg-blue-200"]
      | Selected, Definition _ -> ["inline-block"; "bg-pink-200"]
      | Shadowed, Definition _ -> ["inline-block"; "bg-yellow-200"]
      | Shadowing, Definition _ -> ["inline-block"; "bg-yellow-500"]
      | _, _ -> ["inline-block"]
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
  Queue.enqueue queue (grid_tmpl ~scope_selection_e ~source_column [left_col] loc)

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
  | Operator (_, _, scopes) -> scopes
    |> List.find_map ~f:(find_outermost_binding_scope ~var_name)

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

let rec render_pattern
  ~scope_selection_e ~source_column ~var_selected_events ~shadowed_var_streams ~depth ~queue ~suffix
  ~downstream
  = function
  | Pattern.Primitive (LocIx loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl ~source_column [padded_txt depth str] loc)
  | Var (VarDefIx (loc, selected_event), name) ->
    let trigger_upstream_shadow = match Map.find shadowed_var_streams name with
      | None ->
        Stdio.printf "upstream_shadow %s -> None\n" name;
        Fn.const ()
      | Some event_stream ->
        Stdio.printf "upstream_shadow %s -> Some _\n" name;
        event_stream.trigger
    in
    let trigger_downstream_shadow =
      match List.find_map downstream ~f:(find_outermost_binding ~var_name:name) with
        | None ->
          Stdio.printf "downstream shadow %s -> None\n" name;
          Fn.const ()
        | Some event_stream ->
          Stdio.printf "downstream shadow %s -> Some _\n" name;
          event_stream.trigger
    in
    let var_pos = Definition { trigger_upstream_shadow; trigger_downstream_shadow } in
    show_var ~scope_selection_e ~var_pos ~queue ~source_column ~suffix ~selected_event ~depth ~loc ~name
  | Ignored (LocIx loc, name) ->
    Queue.enqueue queue (grid_tmpl ~source_column [padded_txt depth (name ^ suffix)] loc)
  | Operator (LocIx loc, name, slots) ->
    let open_elem = grid_tmpl ~source_column [ padded_txt depth (name ^ "(") ] loc in
    Queue.enqueue queue open_elem;

    let num_slots = List.length slots in
    List.iteri slots ~f:(fun i pats ->
      let num_pats = List.length pats in
      let last_slot = i = num_slots - 1 in
      List.iteri pats ~f:(fun j ->
        let last_term = j = num_pats - 1 in
        let suffix = get_suffix ~last_slot ~last_term in
        render_pattern ~scope_selection_e ~source_column ~var_selected_events ~shadowed_var_streams
          ~depth:(Int.succ depth) ~queue ~suffix ~downstream
      )
    );

    let close_elem = grid_tmpl ~source_column [ padded_txt depth (")" ^ suffix) ] loc in
    Queue.enqueue queue close_elem
  | _ -> failwith "invariant violation: wrong index"

let rec render_tm ~scope_selection_e ~source_column ?depth:(depth=0) ~var_selected_events
  ~queue ?suffix:(suffix="") =
  function
  | Nominal.Primitive (LocIx loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue
      (grid_tmpl ~scope_selection_e ~source_column [padded_txt depth str] loc)
  | Var (LocIx loc, name) ->
    let selected_event = Map.find_exn var_selected_events name in
    show_var ~var_pos:Reference ~scope_selection_e ~queue ~source_column ~suffix ~selected_event ~depth ~loc ~name
  | Operator (OperatorIx (loc, { signal = expanded_s; set_s = set_expanded }), name, scopes) ->
    let button_event, button = Components.chevron_toggle expanded_s in

    let _ : unit React.event = button_event |> React.E.map set_expanded in

    let _: unit React.signal = expanded_s
      |> React.S.map ~eq:Unit.(=) (function
        | false ->
          let left_col = match scopes with
            | [] -> [padded_txt depth (name ^ "()" ^ suffix) ]
            | _ -> [padded_txt depth (name ^ "("); button; txt (")" ^ suffix) ]
          in
          Queue.enqueue queue (grid_tmpl ~scope_selection_e ~source_column left_col loc
        )
        | true ->
          let open_elem =
            grid_tmpl ~scope_selection_e ~source_column [ padded_txt depth (name ^ "("); button ] loc
          in
          Queue.enqueue queue open_elem;

          List.iteri scopes ~f:(fun i ->
            render_scope ~scope_selection_e ~source_column ~depth ~var_selected_events
              ~queue ~last:(i = List.length scopes - 1));

          let close_elem =
            grid_tmpl ~scope_selection_e ~source_column [ padded_txt depth (")" ^ suffix) ] loc
          in
          Queue.enqueue queue close_elem
      )
    in
    ()
  | _ -> failwith "invariant violation: wrong index"

and render_scope ~scope_selection_e ~source_column ~depth ~var_selected_events ~queue
  ~last:last_slot (Nominal.Scope (pats, tms)) =
  let _pattern_vars (* XXX remove *), pattern_var_events = pats
    |> List.map ~f:(fun pat ->
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

      render_pattern ~scope_selection_e ~source_column ~var_selected_events ~shadowed_var_streams
        ~depth:(depth + 1) ~queue ~suffix:"." ~downstream:tms pat;
      newly_defined_vars, newly_defined_var_events
    )
    |> List.unzip
  in

  let pattern_var_events = Lvca_util.String.Map.unions_right_biased pattern_var_events in

  let var_selected_events = Map.merge_skewed ~combine:(fun ~key:_ _l r -> r)
    var_selected_events pattern_var_events
  in

  (*
  let vars = pattern_vars
    |> List.map ~f:(fun pat_vars -> List.map pat_vars ~f:(fun (_info, name) -> name))
    |> List.join
  in

  (* TODO: rename scope_selection_es *)
  let scope_selection_e = vars
    |> List.fold ~init:scope_selection_e ~f:Map.remove
    |> any_var_selected_event
  in

  let any_pattern_var_selected_event = pattern_var_events in
  *)

  let scope_selection_e = Map.merge_skewed
    scope_selection_e pattern_var_events
    ~combine:(fun ~key:_ _l r -> r)
  in

  let num_tms = List.length tms in
  List.iteri tms ~f:(fun j ->
    let last_term = j = num_tms - 1 in
    let suffix = get_suffix ~last_term ~last_slot in
    render_tm ~scope_selection_e ~source_column ~depth:(depth + 1) ~var_selected_events
      ~queue ~suffix
  )

let view_tm
  ?source_column:(source_column=true)
  ?default_expanded_depth:(expanded_depth=FullyExpanded)
  tm =
  let tm = Nominal.map_loc ~f:cvt_range tm in

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
    let scope_selection_e = var_selected_events
      (*
      |> any_var_selected_event
      |> React.E.select
      *)
    in

    render_tm ~scope_selection_e ~source_column ~var_selected_events ~queue tm;
    let rows, evts = queue |> Queue.to_list |> List.unzip in
    let tbody = rows |> Html.tbody in

    let _ : unit React.event = evts
      |> React.E.select
      |> React.E.map set_selection
    in

    let rows = if source_column
      then
        [ [%html{|<td class="p-2 border-t-2 border-b-2 border-r-2 w-1/2">term</td>|}]
        ; [%html{|<td class="p-2 border-t-2 border-b-2 border-r-2">source</td>|}]
        ; [%html{|<td class="p-2 border-t-2 border-b-2">range</td>|}]
        ]
      else
        [ [%html{|<td class="p-2 border-t-2 border-b-2 border-r-2 w-2/3">term</td>|}]
        ; [%html{|<td class="p-2 border-t-2 border-b-2">range</td>|}]
        ]
    in

    [%html{|
    <table class="w-full table-fixed border-2 cursor-default font-mono">
      <thead> <tr>|}rows{|</tr> </thead>
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
