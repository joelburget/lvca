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

type var_pos =
  | Reference
  | Definition of (var_status React.event * (var_status -> unit)) option

type default_expanded_depth
  = ExpandedTo of int
  | FullyExpanded

let decrease_depth = function
  | ExpandedTo 0 -> ExpandedTo 0
  | ExpandedTo n -> ExpandedTo (n - 1)
  | FullyExpanded -> FullyExpanded

type indexed_map_contents =
  { expanded_s: bool React.signal
  ; set_expanded: bool -> unit
  }

type reactive_map_contents =
  { expanded: bool
  ; set_expanded: bool -> unit
  }

let a_class, pre, span, txt, td, tr = Html.(a_class, pre, span, txt, td, tr)

let cvt_range : SourceRanges.t -> SourceRange.t option
  = fun source_ranges -> match Map.to_alist source_ranges with
    | [source, [range]] -> Some { source; range }
    | _ -> None

let grid_tmpl ?scope_selection_e:(scope_selection_e=React.E.never) ~source_column left loc =
  let classes = scope_selection_e
    |> React.S.hold ~eq:Caml.(=) false
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
let show_var ~var_pos ~queue ~source_column ~suffix ~selected_events ~depth ~loc ~name =
  let select_events, set_selected = Map.find_exn selected_events name in

  let shadowed_e = match var_pos with
    | Definition (Some (shadowed_e, _)) -> shadowed_e
    | _ -> React.E.never
  in

  let classes_s = React.E.select
    [ shadowed_e |> React.E.map (fun e -> Either.First e)
    ; select_events |> React.E.map (fun e -> Either.Second e)
    ]
    |> React.S.hold ~eq:Caml.(=) (Either.Second Unselected)
    |> React.S.map ~eq:(List.equal String.(=)) (fun evt -> match evt, var_pos with
      (* highlight if this var is selected or shadowed *)
      | Either.Second Selected, Reference -> ["inline-block"; "bg-blue-200"]
      | Second Selected, Definition _ -> ["inline-block"; "bg-pink-200"]
      | Second Shadowed, Definition _ -> ["inline-block"; "bg-yellow-200"]
      (* The variable we're shadowing is selected, highlight in orange *)
      | First Selected, Definition _ -> ["inline-block"; "bg-yellow-500"]
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

  Common.bind_event Ev.mouseovers name_elem (fun _evt ->
    set_selected Selected;
    Lwt.return ()
  );

  Common.bind_event Ev.mouseouts name_elem (fun _evt ->
    set_selected Unselected;
    Lwt.return ()
  );

  (match var_pos with
    | Definition (Some (_shadowed_stream, set_shadowed)) ->
      Common.bind_event Ev.mouseovers name_elem (fun _evt ->
        set_shadowed Shadowed;
        Lwt.return ()
      );

      Common.bind_event Ev.mouseouts name_elem (fun _evt ->
        set_shadowed Unselected;
        Lwt.return ()
      );
    | _ -> ()
  );

  let left_col = pre (Lvca_util.List.snoc indents name') in
  Queue.enqueue queue (grid_tmpl ~source_column [left_col] loc)

let get_suffix ~last_slot ~last_term = match last_term, last_slot with
  | true, true -> ""
  | true, false -> ";"
  | false, _ -> ","

let rec index_tm ~expanded_depth ~expanded_map_ref path = function
  | Nominal.Primitive _ | Var _ -> ()
  | Operator (_loc, _name, scopes) -> if not (List.is_empty scopes) then (
    let starts_expanded = match expanded_depth with
      | FullyExpanded -> true
      | ExpandedTo n -> n > 0
    in
    let expanded_s, set_expanded = React.S.create ~eq:Bool.(=) starts_expanded in
    expanded_map_ref :=
      Map.set !expanded_map_ref ~key:path ~data:{ expanded_s; set_expanded };
    List.iteri scopes ~f:(fun i -> index_scope ~expanded_depth ~expanded_map_ref i path)
  )

and index_scope ~expanded_depth ~expanded_map_ref i path (Nominal.Scope (_pats, tms)) =
  let expanded_depth = decrease_depth expanded_depth in
  List.iteri tms ~f:(fun j -> index_tm ~expanded_depth ~expanded_map_ref ((i, j)::path))

let rec show_pattern
  ~source_column ~selected_events ~shadowed_var_streams ~depth ~queue ~suffix
  = function
  | Pattern.Primitive (loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl ~source_column [padded_txt depth str] loc)
  | Var (loc, name) ->
    let var_pos = Definition (Map.find shadowed_var_streams name) in
    show_var ~var_pos ~queue ~source_column ~suffix ~selected_events ~depth ~loc ~name
  | Ignored (loc, name) ->
    Queue.enqueue queue (grid_tmpl ~source_column [padded_txt depth (name ^ suffix)] loc)
  | Operator (loc, name, slots) ->
    let open_elem = grid_tmpl ~source_column [ padded_txt depth (name ^ "(") ] loc in
    Queue.enqueue queue open_elem;

    let num_slots = List.length slots in
    List.iteri slots ~f:(fun i pats ->
      let num_pats = List.length pats in
      let last_slot = i = num_slots - 1 in
      List.iteri pats ~f:(fun j ->
        let last_term = j = num_pats - 1 in
        let suffix = get_suffix ~last_slot ~last_term in
        show_pattern ~source_column ~selected_events ~shadowed_var_streams
          ~depth:(Int.succ depth) ~queue ~suffix
      )
    );

    let close_elem = grid_tmpl ~source_column [ padded_txt depth (")" ^ suffix) ] loc in
    Queue.enqueue queue close_elem

let rec show_tm ~scope_selection_e ~source_column ~path ~selected_events ~expanded_map ~queue ?suffix:(suffix="") =
  let depth = List.length path in
  function
  | Nominal.Primitive (loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl ~scope_selection_e ~source_column [padded_txt depth str] loc)
  | Var (loc, name) ->
    show_var ~var_pos:Reference ~queue ~source_column ~suffix ~selected_events ~depth ~loc ~name
  | Operator (loc, name, scopes) ->
    let { expanded; set_expanded } = Map.find_exn expanded_map path in
    let expanded_s, _unused_set_expanded = React.S.create ~eq:Bool.(=) expanded in
    let button_event, button = Components.chevron_toggle expanded_s in

    let _ : unit React.event = button_event |> React.E.map set_expanded in

    let _: unit React.signal = expanded_s
      |> React.S.map ~eq:Unit.(=) (function
        | false -> Queue.enqueue queue (grid_tmpl ~scope_selection_e ~source_column
          [padded_txt depth (name ^ "("); button; txt (")" ^ suffix) ]
          loc
        )
        | true ->
          let open_elem =
            grid_tmpl ~scope_selection_e ~source_column [ padded_txt depth (name ^ "("); button ] loc
          in
          Queue.enqueue queue open_elem;

          List.iteri scopes ~f:(fun i ->
            show_scope ~scope_selection_e ~source_column ~path ~selected_events
              ~expanded_map ~queue ~last:(i = List.length scopes - 1) i);

          let close_elem =
            grid_tmpl ~scope_selection_e ~source_column [ padded_txt depth (")" ^ suffix) ] loc
          in
          Queue.enqueue queue close_elem
      )
    in
    ()

and show_scope ~scope_selection_e ~source_column ~path ~selected_events ~expanded_map
  ~queue ~last:last_slot i (Nominal.Scope (pats, tms)) =
  let pattern_var_events = List.map pats ~f:(fun pat ->
    let newly_defined_vars = Pattern.list_vars_of_pattern pat in
    let newly_defined_var_events = newly_defined_vars
      |> List.map ~f:(fun (_info, name) ->
        let stream, setter = React.E.create () in
        name, (stream, fun b -> setter b))
      |> Lvca_util.String.Map.of_alist_exn
    in

    let shadowed_var_streams = newly_defined_vars
      |> List.filter_map ~f:(fun (_loc, k) -> match Map.find selected_events k with
        | None -> None
        | Some evt -> Some (k, evt)
      )
      |> Lvca_util.String.Map.of_alist_exn
    in

    let selected_events = Map.merge_skewed ~combine:(fun ~key:_ _l r -> r)
      selected_events newly_defined_var_events
    in

    show_pattern ~source_column ~selected_events ~shadowed_var_streams
      ~depth:(List.length path + 1) ~queue ~suffix:"." pat;
    newly_defined_var_events)
  in

  let selected_events = pattern_var_events
    |> Lvca_util.String.Map.unions_right_biased
    |> Map.merge_skewed ~combine:(fun ~key:_ _l r -> r) selected_events
  in

  let any_pattern_var_selected_event = selected_events
    |> Map.to_alist
    |> List.map ~f:(fun (_k, (evt, _)) -> evt
      |> React.E.map (function Selected -> true | _ -> false)
    )
  in

  let scope_selection_e =
    React.E.select (scope_selection_e :: any_pattern_var_selected_event)
  in

  let num_tms = List.length tms in
  List.iteri tms ~f:(fun j ->
    let last_term = j = num_tms - 1 in
    let suffix = get_suffix ~last_term ~last_slot in
    show_tm ~scope_selection_e ~source_column ~path:((i, j)::path) ~selected_events
      ~expanded_map ~queue ~suffix
  )

let view_tm
  ?source_column:(source_column=true)
  ?default_expanded_depth:(expanded_depth=FullyExpanded)
  tm =
  let tm = Nominal.map_loc ~f:cvt_range tm in

  (* First, create a stream for all free variables actions on them will work
     like normal. *)
  let free_vars = Nominal.free_vars tm in

  let selected_events = free_vars
    |> Set.to_list
    |> List.map ~f:(fun name ->
      let stream, setter = React.E.create () in
      name, (stream, fun b -> setter b))
    |> Lvca_util.String.Map.of_alist_exn
  in

  (* First index all of the terms, meaning we collect a mapping from their path
     to expansion status (so that subterms remember their status even if
     parents / ancestors are closed. *)
  let expanded_map_ref = ref (Base.Map.empty (module Path)) in

  index_tm ~expanded_depth ~expanded_map_ref [] tm;

  (* Any signal change coming from expanded_map_ref is a real update, don't bother with
     equality testing. *)
  let eq _ _ = false in

  let expanded_map_s = !expanded_map_ref
    |> Map.to_alist
    |> List.map ~f:(fun (path, { expanded_s; set_expanded }) -> expanded_s
      |> React.S.map ~eq (fun expanded -> path, { expanded; set_expanded })
    )
    |> React.S.merge ~eq (fun kvs kv -> kv::kvs) []
    |> React.S.map ~eq (Map.of_alist_exn (module Path))
  in

  let selection_e, set_selection = React.E.create () in

  let elem = expanded_map_s
    |> React.S.map ~eq (fun expanded_map ->
      let queue = Queue.create () in
      let scope_selection_e = React.E.never in

      show_tm ~scope_selection_e ~source_column ~path:[] ~selected_events ~expanded_map ~queue tm;
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
    )
    |> RList.singleton_s
    |> R.Html.div
  in

  elem, selection_e
