open Lvca_syntax
open ReactiveData
open Base
open Js_of_ocaml_tyxml.Tyxml_js
module React = SafeReact

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

type map_contents =
  { expanded_s: bool React.signal
  ; set_expanded: (bool -> unit)
  }

type map_contents' =
  { expanded: bool
  ; set_expanded: (bool -> unit)
  }

let rec index_tm ~map_ref path depth = function
  | Nominal.Primitive _ -> ()
  | Var _ -> ()
  | Operator (_loc, _name, scopes) -> if not (List.is_empty scopes) then (
    let expanded_s, set_expanded = React.S.create ~eq:Bool.(=) false in
    map_ref := Map.set !map_ref ~key:path ~data:{ expanded_s; set_expanded };
    scopes |> List.iteri ~f:(fun i -> index_scope ~map_ref i path (Int.succ depth))
  )

and index_scope ~map_ref i path depth (Nominal.Scope (_pats, tms)) =
  List.iteri tms ~f:(fun j -> index_tm ~map_ref ((i, j)::path) depth)

let rec show_pattern ~depth ~queue = function
  | Pattern.Primitive (loc, p) ->
    Queue.enqueue queue (grid_tmpl [p |> Primitive.to_string |> padded_txt depth] loc)
  | Var (loc, name) | Ignored (loc, name) ->
    Queue.enqueue queue (grid_tmpl [padded_txt depth name] loc)
  | Operator (loc, name, patss) ->
    let open_elem = grid_tmpl [ padded_txt depth (name ^ "(") ] loc in
    Queue.enqueue queue open_elem;

    patss |> List.iter ~f:(fun pats -> pats
      |> List.iter ~f:(show_pattern ~depth:(Int.succ depth) ~queue)
    );

    let close_elem = grid_tmpl [ padded_txt depth ")" ] loc in
    Queue.enqueue queue close_elem

let rec show_tm ~depth ~path ~map ~queue = function
  | Nominal.Primitive (loc, p) ->
    Queue.enqueue queue (grid_tmpl [p |> Primitive.to_string |> padded_txt depth] loc)
  | Var (loc, name) ->
    Queue.enqueue queue (grid_tmpl [padded_txt depth name] loc)
  | Operator (loc, name, scopes) ->
    let { expanded; set_expanded } = Map.find_exn map path in
    let expanded_s, _xxx = React.S.create ~eq:Bool.(=) expanded in
    let button_event, button = Components.ellipsis_toggle expanded_s in
    (* let _ : unit React.event = button_event |> React.E.map set_expanded in *)
    let _ : unit React.event = button_event |> React.E.map set_expanded in

    let _: unit React.signal = expanded_s
      |> React.S.map ~eq:Unit.(=) (function
        | false -> Queue.enqueue queue (grid_tmpl
          [padded_txt depth (name ^ "("); button; txt ")" ]
          loc
        )
        | true ->
          let open_elem = grid_tmpl [ padded_txt depth (name ^ "("); button ] loc in
          Queue.enqueue queue open_elem;

          scopes |> List.iteri ~f:(show_scope ~depth:(Int.succ depth) ~path ~map ~queue);

          let close_elem = grid_tmpl [ padded_txt depth ")" ] loc in
          Queue.enqueue queue close_elem
      )
    in
    ()

and show_scope ~depth ~path ~map ~queue i (Nominal.Scope (pats, tms)) =
  pats |> List.iter ~f:(show_pattern ~depth ~queue);
  tms |> List.iteri ~f:(fun j -> show_tm ~depth ~path:((i, j)::path) ~map ~queue)

let view_tm tm =
  let map_ref = ref (Base.Map.empty (module Path)) in
  index_tm ~map_ref [] 0 tm;

  let r_elem = !map_ref
    |> Map.to_alist
    |> List.map ~f:(fun (name, { expanded_s; set_expanded }) -> expanded_s
      |> React.S.map ~eq:Caml.(=) (fun expanded -> name, { expanded; set_expanded })
    )
    |> React.S.merge ~eq:(fun _ _ -> false) (fun kvs kv -> kv::kvs) []
    |> React.S.map ~eq:(fun _ _ -> false) (fun kvs ->
      let map = Map.of_alist_exn (module Path) kvs in
      let queue = Queue.create () in
      show_tm ~depth:0 ~path:[] ~map ~queue tm;
      Html.div (Queue.to_list queue)
    )
    |> RList.singleton_s
  in
  R.Html.div r_elem
