open Lvca_syntax
open ReactiveData
open Base
open Js_of_ocaml_tyxml.Tyxml_js
module React = SafeReact

(* TODO: make into a table with source / range columns *)

type indexed_map_contents =
  { expanded_s: bool React.signal
  ; set_expanded: bool -> unit
  }

type reactive_map_contents =
  { expanded: bool
  ; set_expanded: bool -> unit
  }

let a_class, txt, div, _ul, _li = Html.(a_class, txt, div, ul, li)

let view_loc loc = loc |> SourceRanges.to_string |> txt

let grid_tmpl left loc = div ~a:[a_class ["grid"; "grid-cols-3"]]
  [ div ~a:[a_class ["col-span-2"]] left
  ; div [ view_loc loc ]
  ]

let indent _ = Html.(span ~a:[a_class ["border-l-2"; "border-dotted"]] [txt "  "])

let padded_txt depth text =
  let indents = List.init depth ~f:indent in
  Html.pre
    ~a:[a_class ["inline-block"]]
    (Lvca_util.List.snoc indents (txt text))

let rec index_tm ~map_ref path = function
  | Nominal.Primitive _ | Var _ -> ()
  | Operator (_loc, _name, scopes) -> if not (List.is_empty scopes) then (
    let expanded_s, set_expanded = React.S.create ~eq:Bool.(=) false in
    map_ref := Map.set !map_ref ~key:path ~data:{ expanded_s; set_expanded };
    List.iteri scopes ~f:(fun i -> index_scope ~map_ref i path)
  )

and index_scope ~map_ref i path (Nominal.Scope (_pats, tms)) =
  List.iteri tms ~f:(fun j -> index_tm ~map_ref ((i, j)::path))

let rec show_pattern ~depth ~queue ~suffix = function
  | Pattern.Primitive (loc, p) ->
    let str = Primitive.to_string p ^ suffix in
    Queue.enqueue queue (grid_tmpl [str |> padded_txt depth] loc)
  | Var (loc, name) | Ignored (loc, name) ->
    Queue.enqueue queue (grid_tmpl [padded_txt depth (name ^ suffix)] loc)
  | Operator (loc, name, patss) ->
    let open_elem = grid_tmpl [ padded_txt depth (name ^ "(") ] loc in
    Queue.enqueue queue open_elem;

    List.iter patss
      ~f:(List.iter ~f:(show_pattern ~depth:(Int.succ depth) ~queue ~suffix:";"));

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
  List.iter pats ~f:(show_pattern ~depth:(List.length path) ~queue ~suffix:".");
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

let view_tm tm =
  (* First index all of the terms, meaning we collect a mapping from their path
     to expansion status (so that subterms remember their status even if
     parents / ancestors are closed. *)
  let map_ref = ref (Base.Map.empty (module Path)) in
  index_tm ~map_ref [] tm;

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

  (* let selection_s, set_selection = React.S.create () in *)

  let elem = map_s
    |> React.S.map ~eq (fun map ->
      let queue = Queue.create () in
      show_tm ~path:[] ~map ~queue tm;
      Html.div (Queue.to_list queue)
    )
    |> RList.singleton_s
    |> R.Html.div
  in

  elem (* , selection_s *)
