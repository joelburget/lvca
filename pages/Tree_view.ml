(** Overview: This works by creating a queue holding rows of rendered text. We traverse
    the given term, enqueuing one row at a time, which we then render. The result is a
    single div, but it's reactive, so its children (rows) change as you expand (adding
    rows) or contract (removing rows) (term) children.

    Rendering rows happens every time a visibility is toggled.

    Only once we index the given term and its descendents, changing its info type from
    [Opt_range.t] to [term_index]. This is used to highlight variables without
    re-rendering and to create a signal holding the expansion state for each operator (so
    even if an enclosing scope is contracted, the signal retains its state). *)

open Base
open Brr
open Brr_note
open Lvca_provenance
open Lvca_syntax
open Lvca_util
open Note
open Prelude

let div, tbody, table, pre, span, td, tr, thead, txt' =
  El.(div, tbody, table, pre, span, td, tr, thead, txt')
;;

module Var_status = struct
  type t =
    | Unselected
    | Selected
    | Shadowed
    | Shadowing

  let ( = ) x y =
    match x, y with
    | Unselected, Unselected
    | Selected, Selected
    | Shadowed, Shadowed
    | Shadowing, Shadowing ->
      true
    | _ -> false
  ;;
end

type default_expanded_depth =
  | ExpandedTo of int
  | FullyExpanded

let decrease_depth = function
  | ExpandedTo 0 -> ExpandedTo 0
  | ExpandedTo n -> ExpandedTo (n - 1)
  | FullyExpanded -> FullyExpanded
;;

type 'a signal' =
  { signal : 'a signal
  ; set_s : 'a -> unit
  }

let create_s ~eq init =
  let signal, set_s = S.create ~eq init in
  { signal; set_s }
;;

type 'a event' =
  { event : 'a event
  ; trigger : 'a -> unit
  }

let create_e () =
  let event, trigger = E.create () in
  { event; trigger }
;;

type definition_streams =
  { trigger_upstream_shadow : Var_status.t -> unit
        (** Alert upstream (in an enclosing scope, a var that we're shadowing) to light up *)
  ; trigger_downstream_shadow : Var_status.t -> unit
        (** Alert downstream (in an enclosed scope, a var that shadows us) to light up *)
  }

type render_params =
  { source_column : bool (** Show the "source" column or not *)
  ; range_column : bool (** Show the "range" column or not *)
  ; highlighted_ranges : Source_ranges.t
  ; depth : int
        (** How many times we've recursed to a child. Used both when deciding if we should
            render at this depth and for indentation. *)
  ; var_selected_events : Var_status.t event' String.Map.t
        (** Variables that are in scope. Used to light them up (see [render_var]). *)
  ; queue : (El.t * Source_ranges.t event) Queue.t
        (** Queue of rows that have been rendered. *)
  }

type var_pos =
  | Reference (** This variable is a reference to a binding site *)
  | Definition of definition_streams (** This variable is a binding site *)

type expanded = bool

type term_index =
  | Operator_ix of Source_range.t option * expanded signal'
  | Var_def_ix of Source_range.t option * Var_status.t event'
  | Loc_ix of Source_range.t option

let select_source_range = function
  | Provenance.Located loc | Provenance.Calculated (loc, _) ->
    (match loc with
    | Parse_located
        { input = Provenance.Parse_input.Buffer_name source; range = Some range } ->
      Some Source_range.{ source; range }
    | _ -> None)
  | _ -> None
;;

module Decoration = struct
  let in_scope = "bg-green-50"
  let reference = "bg-blue-200"
  let definition = "bg-pink-200"
  let upstream_shadow = "bg-yellow-200"
  let downstream_shadow = "bg-yellow-500"
  let highlighted = "underline"
end

(** Render a row in the grid. *)
let grid_tmpl ~render_params left loc : El.t * Source_ranges.t event =
  let { var_selected_events; source_column; range_column; _ } = render_params in
  let in_scope_cls_s =
    var_selected_events
    |> Map.to_alist
    (* Highlight background if any variable defined in this scope is selected *)
    |> List.map ~f:(fun (_k, { event; _ }) ->
           event |> E.map (function Var_status.Selected -> true | _ -> false))
    |> E.select
    |> S.hold ~eq:Bool.( = ) false
  in
  let fst_col = td ~at:(classes "col-span-2 px-2 py-0") left in
  Elr.def_class (Jstr.v Decoration.in_scope) in_scope_cls_s fst_col;
  (match loc with
  | Some loc ->
    Source_ranges.(
      Fmt.pr
        "Tree_view: comparing locations %a / %a\n"
        pp
        (of_source_range loc)
        pp
        render_params.highlighted_ranges);
    (match
       Map.is_empty
         Source_ranges.(intersect (of_source_range loc) render_params.highlighted_ranges)
     with
    | true -> ()
    | false -> Elr.def_class (Jstr.v Decoration.highlighted) in_scope_cls_s fst_col)
  | _ ->
    Source_ranges.(
      Fmt.pr
        "Tree_view: comparing locations None / %a\n"
        pp
        render_params.highlighted_ranges);
    ());
  let cols =
    match loc with
    | None -> [ fst_col ]
    | Some Source_range.{ source; range } ->
      [ Some fst_col
      ; (if source_column
        then Some (td ~at:[ class' "px-2"; class' "py-0" ] [ txt' source ])
        else None)
      ; (if range_column
        then
          Some
            (td
               ~at:[ class' "px-2"; class' "py-0" ]
               [ txt' (Fmt.to_to_string Range.pp range) ])
        else None)
      ]
      |> List.filter_map ~f:Fn.id
  in
  let tr = tr cols in
  let evt =
    match loc with
    | None -> E.never
    | Some loc ->
      E.select
        [ Evr.on_el Ev.mouseover (fun _evt -> Source_ranges.of_source_range loc) tr
        ; Evr.on_el Ev.mouseout (fun _evt -> Source_ranges.empty) tr
        ]
  in
  tr, evt
;;

let indent _ = span ~at:[ class' "border-l-2"; class' "border-dotted" ] [ txt' "  " ]

let pad_text depth text =
  let indents = List.init depth ~f:indent in
  indents @ [ text ]
;;

let padded_text depth text =
  pre ~at:[ class' "inline-block" ] (pad_text depth (txt' text))
;;

(** Variables have a lot of states / interactions. States:

    - Reference: this variable references the variable being hovered (blue)
    - Definition: this is the definition site of the variable being hovered (pink)
    - Upstream shadow: this variable is shadowed by the variable being hovered (yellow)
    - Downstream shadow: this variable shadows the one being hovered (orange)

    There are two places a variable can occur: in a pattern (where it's defined) or in a
    term (where it's used), and this function is used for both.

    - Definition: when hovered, send signals to references and shadow points to light up.
    - Referenced: when hovered, send signal just to definition and other references. *)
let render_var ~render_params ~var_pos ~suffix ~selected_event ~loc ~name : unit =
  let { depth; queue; _ } = render_params in
  let { event = selected_event; trigger = trigger_selected } = selected_event in
  let classes_s =
    selected_event
    |> S.hold ~eq:Var_status.( = ) Unselected
    |> S.map ~eq:Tuple4.Bool.( = ) (fun evt ->
           match evt, var_pos with
           | Var_status.Selected, Reference -> true, false, false, false (* reference *)
           | Selected, Definition _ -> false, true, false, false (* definition *)
           | Shadowed, Definition _ -> false, false, true, false (* upstream shadow *)
           | Shadowing, Definition _ -> false, false, false, true (* downstream shadow *)
           | _, _ -> false, false, false, false)
  in
  let inner_span = span ~at:[ class' "inline-block" ] [ txt' name ] in
  let add_active_class color getter =
    Elr.def_class (Jstr.v color) (classes_s |> S.map getter) inner_span
  in
  add_active_class Decoration.reference Tuple4.get1;
  add_active_class Decoration.definition Tuple4.get2;
  add_active_class Decoration.upstream_shadow Tuple4.get3;
  add_active_class Decoration.downstream_shadow Tuple4.get4;
  let name_elem = span [ inner_span; txt' suffix ] in
  let trigger_upstream_shadow, trigger_downstream_shadow =
    match var_pos with
    | Definition { trigger_upstream_shadow; trigger_downstream_shadow } ->
      trigger_upstream_shadow, trigger_downstream_shadow
    | Reference -> Fn.const (), Fn.const ()
  in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.mouseover Fn.id name_elem in
    E.log evt (fun _evt ->
        trigger_downstream_shadow Shadowing;
        trigger_upstream_shadow Shadowed;
        trigger_selected Selected)
  in
  let _sink : Logr.t option =
    let evt = Evr.on_el Ev.mouseout Fn.id name_elem in
    E.log evt (fun _evt ->
        trigger_upstream_shadow Unselected;
        trigger_downstream_shadow Unselected;
        trigger_selected Unselected)
  in
  let left_col = pre (pad_text depth name_elem) in
  Queue.enqueue queue (grid_tmpl ~render_params [ left_col ] loc)
;;

let get_suffix ~last_slot = match last_slot with true -> "" | false -> ";"

module type Index_s = sig
  val lookup : Provenance.t -> term_index
  val pat : Pattern.t -> Pattern.t

  val term
    :  expanded_depth:default_expanded_depth
    -> Nominal.Term.t
    -> Nominal.Term.t * unit event

  val find_outermost_binding
    :  var_name:string
    -> Nominal.Term.t
    -> Var_status.t event' option
end

module Mk_index () : Index_s = struct
  let hashtbl = Hashtbl.create (module Int)
  let current_ix = ref 0

  let lookup = function
    | Provenance.Indexed i -> Hashtbl.find_exn hashtbl i
    | _ -> failwith "Expected an indexed value"
  ;;

  let go data =
    let key = !current_ix in
    Int.incr current_ix;
    Hashtbl.set hashtbl ~key ~data;
    Provenance.Indexed key
  ;;

  let prim (loc, prim) = go (Loc_ix (select_source_range loc)), prim

  let rec pat = function
    | Pattern.Primitive p -> Pattern.Primitive (prim p)
    | Var (loc, name) -> Var (go (Var_def_ix (select_source_range loc, create_e ())), name)
    | Operator (loc, name, slots) ->
      Operator (go (Loc_ix (select_source_range loc)), name, List.map slots ~f:pat)
  ;;

  (** Index a term so that its info type changes from [Opt_range.t] to [term_index]. *)
  let rec term ~expanded_depth = function
    | Nominal.Term.Primitive p -> Nominal.Term.Primitive (prim p), E.never
    | Var (loc, name) -> Var (go (Loc_ix (select_source_range loc)), name), E.never
    | Operator (loc, name, scopes) ->
      let scopes, expanded_s, expanded_toggle_e =
        if List.is_empty scopes
        then [], (* not actually used *) create_s true ~eq:Bool.( = ), E.never
        else (
          let scopes, children_expanded_toggle_e =
            scopes
            |> List.map ~f:(scope ~expanded_depth)
            |> List.unzip
            |> Tuple2.map2 ~f:E.select
          in
          let starts_expanded =
            match expanded_depth with FullyExpanded -> true | ExpandedTo n -> n > 0
          in
          let expanded_s = create_s ~eq:Bool.( = ) starts_expanded in
          let expanded_toggle_e = expanded_s.signal |> S.changes |> E.map (fun _ -> ()) in
          let expanded_toggle_e =
            E.select [ expanded_toggle_e; children_expanded_toggle_e ]
          in
          scopes, expanded_s, expanded_toggle_e)
      in
      ( Operator (go (Operator_ix (select_source_range loc, expanded_s)), name, scopes)
      , expanded_toggle_e )

  and scope ~expanded_depth (Nominal.Scope.Scope (pats, tm)) =
    let expanded_depth = decrease_depth expanded_depth in
    let tm, evt = term ~expanded_depth tm in
    let pats = pats |> List.map ~f:pat in
    Nominal.Scope.Scope (pats, tm), evt
  ;;

  let rec find_outermost_binding ~var_name = function
    | Nominal.Term.Primitive _ | Var _ -> None
    | Operator (_, _, scopes) ->
      List.find_map scopes ~f:(find_outermost_binding_scope ~var_name)

  and find_outermost_binding_scope ~var_name (Scope (pats, body)) =
    let found_var =
      pats
      |> List.find_map ~f:(fun pat ->
             pat
             |> Pattern.list_vars_of_pattern
             |> List.find_map ~f:(fun (info, name) ->
                    match String.(name = var_name), info with
                    | true, Indexed i ->
                      Hashtbl.find hashtbl i
                      |> Option.bind ~f:(function
                             | Var_def_ix (_, evt) -> Some evt
                             | _ -> None)
                    | _, _ -> None))
    in
    match found_var with
    | Some v -> Some v
    | None -> find_outermost_binding ~var_name body
  ;;
end

module Mk_render (Index : Index_s) = struct
  let rec render_pattern ~render_params ~shadowed_var_streams ~suffix ~downstream
      : Pattern.t -> unit
    =
    let { depth; queue; _ } = render_params in
    function
    | Pattern.Primitive p ->
      let loc =
        match p |> Primitive.All.info |> Index.lookup with
        | Loc_ix loc -> loc
        | _ -> invariant_violation [%here] "Expected Loc_ix"
      in
      let str = Fmt.str "%a%s" Primitive.All.pp p suffix in
      Queue.enqueue queue (grid_tmpl ~render_params [ padded_text depth str ] loc)
    | Var (info, name) ->
      (match Index.lookup info with
      | Var_def_ix (loc, selected_event) ->
        let trigger_upstream_shadow =
          match Map.find shadowed_var_streams name with
          | None -> Fn.const ()
          | Some event_stream -> event_stream.trigger
        in
        let trigger_downstream_shadow =
          match Index.find_outermost_binding downstream ~var_name:name with
          | None -> Fn.const ()
          | Some event_stream -> event_stream.trigger
        in
        let var_pos = Definition { trigger_upstream_shadow; trigger_downstream_shadow } in
        render_var ~render_params ~var_pos ~suffix ~selected_event ~loc ~name
      | _ -> failwith "Expected Var_def_ix")
    | Operator (info, name, slots) ->
      (match Index.lookup info with
      | Loc_ix loc ->
        (match slots with
        | [] ->
          Queue.enqueue
            queue
            (grid_tmpl ~render_params [ padded_text depth (name ^ "()" ^ suffix) ] loc)
        | _ ->
          let open_elem =
            grid_tmpl ~render_params [ padded_text depth (name ^ "(") ] loc
          in
          Queue.enqueue queue open_elem;
          let num_slots = List.length slots in
          List.iteri slots ~f:(fun i pat ->
              let suffix = get_suffix ~last_slot:(i = num_slots - 1) in
              let render_params = { render_params with depth = Int.succ depth } in
              render_pattern ~render_params ~shadowed_var_streams ~suffix ~downstream pat);
          let close_elem =
            grid_tmpl ~render_params [ padded_text depth (")" ^ suffix) ] loc
          in
          Queue.enqueue queue close_elem)
      | _ -> failwith "Expected Loc_ix")
  ;;

  let rec term ~render_params ?(suffix = "") : Nominal.Term.t -> unit =
    let { depth; var_selected_events; queue; _ } = render_params in
    function
    | Nominal.Term.Primitive p ->
      let str = Fmt.str "%a%s" Primitive.All.pp p suffix in
      let loc =
        match p |> Primitive.All.info |> Index.lookup with
        | Loc_ix loc -> loc
        | _ -> invariant_violation [%here] "Expected Loc_ix"
      in
      Queue.enqueue queue (grid_tmpl ~render_params [ padded_text depth str ] loc)
    | Var (info, name) ->
      (match Index.lookup info with
      | Loc_ix loc ->
        let selected_event = Map.find_exn var_selected_events name in
        render_var ~render_params ~var_pos:Reference ~suffix ~selected_event ~loc ~name
      | _ -> failwith "expected Loc_ix")
    | Operator (info, name, scopes) ->
      (match Index.lookup info with
      | Operator_ix (loc, expanded_signal) ->
        let { signal = expanded_s; set_s = set_expanded } = expanded_signal in
        let button_event, button = Components.chevron_toggle expanded_s in
        let _sink : Logr.t option = E.log button_event set_expanded in
        (match scopes, S.value expanded_s with
        | [], _ ->
          Queue.enqueue
            queue
            (grid_tmpl ~render_params [ padded_text depth (name ^ "()" ^ suffix) ] loc)
        | _, false ->
          Queue.enqueue
            queue
            (grid_tmpl
               ~render_params
               [ padded_text depth (name ^ "("); button; txt' (")" ^ suffix) ]
               loc)
        | _, true ->
          let open_elem =
            grid_tmpl ~render_params [ padded_text depth (name ^ "("); button ] loc
          in
          Queue.enqueue queue open_elem;
          List.iteri scopes ~f:(fun i ->
              render_scope ~render_params ~last:(i = List.length scopes - 1));
          let close_elem =
            grid_tmpl ~render_params [ padded_text depth (")" ^ suffix) ] loc
          in
          Queue.enqueue queue close_elem)
      | _ -> failwith "Expected Operator_ix")

  and render_scope ~render_params ~last:last_slot (Nominal.Scope.Scope (pats, tm)) =
    let { depth; var_selected_events; _ } = render_params in
    let pattern_var_events =
      List.map pats ~f:(fun pat ->
          let newly_defined_vars = Pattern.list_vars_of_pattern pat in
          let newly_defined_var_events =
            newly_defined_vars
            |> List.map ~f:(fun (info, name) ->
                   match Index.lookup info with
                   | Var_def_ix (_loc, event) -> name, event
                   | _ -> failwith "invariant violation: wrong index")
            |> String.Map.of_alist_exn
          in
          let shadowed_var_streams =
            newly_defined_vars
            |> List.filter_map ~f:(fun (_loc, k) ->
                   Map.find var_selected_events k |> Option.map ~f:(fun evt -> k, evt))
            |> String.Map.of_alist_exn
          in
          let var_selected_events =
            Map.merge_skewed
              ~combine:(fun ~key:_ _l r -> r)
              var_selected_events
              newly_defined_var_events
          in
          let render_params =
            { render_params with depth = Int.succ depth; var_selected_events }
          in
          render_pattern
            ~render_params
            ~shadowed_var_streams
            ~suffix:"."
            ~downstream:tm
            pat;
          newly_defined_var_events)
    in
    (* Events for variables bound in this scope *)
    let pattern_var_events = String.Map.unions_right_biased pattern_var_events in
    let combine ~key:_ _l r = r in
    (* Select events for variables visible in this scope *)
    let var_selected_events =
      Map.merge_skewed ~combine var_selected_events pattern_var_events
    in
    let render_params =
      { render_params with depth = Int.succ depth; var_selected_events }
    in
    term ~render_params ~suffix:(get_suffix ~last_slot) tm
  ;;
end

let view_tm
    ?(source_column = true)
    ?(range_column = true)
    ?default_expanded_depth:(expanded_depth = ExpandedTo 3)
    ?(highlighted_ranges = Source_ranges.empty)
    tm
  =
  (* First, create a stream for all free variables. Actions on them will work
     like normal. *)
  let module Index = Mk_index () in
  let module Render = Mk_render (Index) in
  let free_vars = Nominal.Term.free_vars tm in
  let var_selected_events =
    free_vars
    |> Set.to_list
    |> List.map ~f:(fun name -> name, create_e ())
    |> String.Map.of_alist_exn
  in
  let tm, visible_toggle_e = Index.term ~expanded_depth tm in
  let selection_e, set_selection = E.create () in
  let render () =
    let queue = Queue.create () in
    let render_params =
      { var_selected_events
      ; queue
      ; depth = 0
      ; source_column
      ; range_column
      ; highlighted_ranges
      }
    in
    Render.term ~render_params tm;
    let rows, evts = queue |> Queue.to_list |> List.unzip in
    let tbody = tbody rows in
    let _sink : Logr.t option = E.log (E.select evts) set_selection in
    let rows =
      [ Some
          (td ~at:(classes "p-2 border-t-2 border-b-2 border-r-2 w-1/2") [ txt' "term" ])
      ; (if source_column
        then
          Some (td ~at:(classes "p-2 border-t-2 border-b-2 border-r-2") [ txt' "source" ])
        else None)
      ; (if range_column
        then Some (td ~at:(classes "p-2 border-t-2 border-b-2") [ txt' "range" ])
        else None)
      ]
      |> List.filter_map ~f:Fn.id
    in
    let table_contents =
      if source_column || range_column then [ thead [ tr rows ]; tbody ] else [ tbody ]
    in
    table
      ~at:(classes "w-full table-fixed border-2 cursor-default font-mono")
      table_contents
  in
  (* Any signal change coming from render is a real update, don't bother with
     equality testing. *)
  let eq _ _ = false in
  let elem_children =
    visible_toggle_e |> S.hold ~eq () |> S.map ~eq (fun () -> [ render () ])
  in
  mk_reactive div elem_children, selection_e
;;
