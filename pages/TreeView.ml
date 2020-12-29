open Lvca_syntax
open ReactiveData
open Base
open Js_of_ocaml_tyxml.Tyxml_js

module Ev = Js_of_ocaml_lwt.Lwt_js_events
module React = SafeReact

(* TODO: make into a table with source / range columns *)

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

let a_class, txt, td, tr = Html.(a_class, txt, td, tr)

let cvt_range : SourceRanges.t -> SourceRange.t option
  = fun source_ranges -> match Map.to_alist source_ranges with
    | [source, [range]] -> Some { source; range }
    | _ -> None

let grid_tmpl left loc =
  let fst_col = td ~a:[a_class ["col-span-2"; "px-2"]] left in
  let cols = match loc with
    | None -> [ fst_col ]
    | Some SourceRange.{ source; range } ->
      [ fst_col
      ; td ~a:[a_class ["px-2"]] [txt source]
      ; td ~a:[a_class ["px-2"]] [txt (Range.to_string range)]
      ]
  in

    (* div ~a:[a_class ["grid"; "grid-cols-3"]] cols in *)
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

let indent _ = Html.(span ~a:[a_class ["border-l-2"; "border-dotted"]] [txt "  "])

let padded_txt depth text =
  let indents = List.init depth ~f:indent in
  Html.pre
    ~a:[a_class ["inline-block"]]
    (Lvca_util.List.snoc indents (txt text))

let rec index_tm ~expanded_depth ~map_ref path = function
  | Nominal.Primitive _ | Var _ -> ()
  | Operator (_loc, _name, scopes) -> if not (List.is_empty scopes) then (
    let starts_expanded = match expanded_depth with
      | FullyExpanded -> true
      | ExpandedTo n -> n > 0
    in
    let expanded_s, set_expanded = React.S.create ~eq:Bool.(=) starts_expanded in
    map_ref := Map.set !map_ref ~key:path ~data:{ expanded_s; set_expanded };
    List.iteri scopes ~f:(fun i -> index_scope ~expanded_depth ~map_ref i path)
  )

and index_scope ~expanded_depth ~map_ref i path (Nominal.Scope (_pats, tms)) =
  let expanded_depth = decrease_depth expanded_depth in
  List.iteri tms ~f:(fun j -> index_tm ~expanded_depth ~map_ref ((i, j)::path))

let rec show_pattern ~depth ~queue ~suffix = function
  | Pattern.Primitive (loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl [str |> padded_txt depth] loc)
  | Var (loc, name) | Ignored (loc, name) ->
    Queue.enqueue queue (grid_tmpl [padded_txt depth (name ^ suffix)] loc)
  | Operator (loc, name, patss) ->
    let open_elem = grid_tmpl [ padded_txt depth (name ^ "(") ] loc in
    Queue.enqueue queue open_elem;

    let num_patss = List.length patss in
    List.iteri patss ~f:(fun i pats ->
      let num_pats = List.length pats in
      let last_outer = i = num_patss - 1 in
      List.iteri pats ~f:(fun j ->
        let last_inner = j = num_pats - 1 in
        let suffix = match last_inner, last_outer with
          | true, true -> ""
          | true, false -> ";"
          | false, _ -> ","
        in
        show_pattern ~depth:(Int.succ depth) ~queue ~suffix
      )
    );

    let close_elem = grid_tmpl [ padded_txt depth (")" ^ suffix) ] loc in
    Queue.enqueue queue close_elem

let rec show_tm ~path ~map ~queue ?suffix:(suffix="") =
  let depth = List.length path in
  function
  | Nominal.Primitive (loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl [str |> padded_txt depth] loc)
  | Var (loc, name) ->
    Queue.enqueue queue (grid_tmpl [padded_txt depth (name ^ suffix)] loc)
  | Operator (loc, name, scopes) ->
    let { expanded; set_expanded } = Map.find_exn map path in
    let expanded_s, _unused_set_expanded = React.S.create ~eq:Bool.(=) expanded in
    let button_event, button = Components.chevron_toggle expanded_s in

    let _ : unit React.event = button_event |> React.E.map set_expanded in

    let _: unit React.signal = expanded_s
      |> React.S.map ~eq:Unit.(=) (function
        | false -> Queue.enqueue queue (grid_tmpl
          [padded_txt depth (name ^ "("); button; txt (")" ^ suffix) ]
          loc
        )
        | true ->
          let open_elem = grid_tmpl [ padded_txt depth (name ^ "("); button ] loc in
          Queue.enqueue queue open_elem;

          List.iteri scopes ~f:(fun i ->
            show_scope ~path ~map ~queue ~last:(i = List.length scopes - 1) i);

          let close_elem = grid_tmpl [ padded_txt depth (")" ^ suffix) ] loc in
          Queue.enqueue queue close_elem
      )
    in
    ()

and show_scope ~path ~map ~queue ~last:last_scope i (Nominal.Scope (pats, tms)) =
  List.iter pats ~f:(show_pattern ~depth:(List.length path + 1) ~queue ~suffix:".");
  let num_tms = List.length tms in
  List.iteri tms ~f:(fun j ->
    let last_tm = j = num_tms - 1 in
    let suffix = match last_tm, last_scope with
      | true, true -> ""
      | true, false -> ";"
      | _, _ -> ","
    in
    show_tm ~path:((i, j)::path) ~map ~queue ~suffix
  )

let view_tm ?default_expanded_depth:(expanded_depth=FullyExpanded) tm =
  let tm = Nominal.map_loc ~f:cvt_range tm in

  (* First index all of the terms, meaning we collect a mapping from their path
     to expansion status (so that subterms remember their status even if
     parents / ancestors are closed. *)
  let map_ref = ref (Base.Map.empty (module Path)) in
  index_tm ~expanded_depth ~map_ref [] tm;

  (* Any signal change coming from map_ref is a real update, don't bother with
     equality testing. *)
  let eq _ _ = false in

  let map_s = !map_ref
    |> Map.to_alist
    |> List.map ~f:(fun (path, { expanded_s; set_expanded }) -> expanded_s
      |> React.S.map ~eq (fun expanded -> path, { expanded; set_expanded })
    )
    |> React.S.merge ~eq (fun kvs kv -> kv::kvs) []
    |> React.S.map ~eq (Map.of_alist_exn (module Path))
  in

  let selection_e, set_selection = React.E.create () in

  let elem = map_s
    |> React.S.map ~eq (fun map ->
      let queue = Queue.create () in
      show_tm ~path:[] ~map ~queue tm;
      let rows, evts = queue |> Queue.to_list |> List.unzip in
      let tbody = rows |> Html.tbody in

      let _ : unit React.event = evts
        |> React.E.select
        |> React.E.map set_selection
      in

      [%html{|
      <table class="w-full table-fixed border-2">
        <thead>
          <tr>
            <td class="p-2 border-t-2 border-b-2 border-r-2 w-1/2">term</td>
            <td class="p-2 border-t-2 border-b-2 border-r-2">source</td>
            <td class="p-2 border-t-2 border-b-2">range</td>
          </tr>
        </thead>
        |}[tbody]{|
      </table>
        |}]
    )
    |> RList.singleton_s
    |> R.Html.div (* ~a:[Html.a_class ["border-2"; "p-2"]] *)
  in

  elem, selection_e
