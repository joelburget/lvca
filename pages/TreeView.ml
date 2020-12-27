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

let padded_txt depth text = Html.pre
  ~a:[a_class ["inline-block"]]
  [txt (String.make depth ' ' ^ text)]

type map_contents =
  { expanded_s: bool React.signal
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

let rec show_tm ~depth ~path ~map ~queue = function
  | Nominal.Primitive (loc, p) ->
    Queue.enqueue queue (grid_tmpl [p |> Primitive.to_string |> padded_txt depth] loc)
  | Var (loc, name) ->
    Queue.enqueue queue (grid_tmpl [padded_txt depth name] loc)
  | Operator (loc, name, scopes) ->
    let { expanded_s; set_expanded } = Map.find_exn map path in
    let button_event, button = Components.ellipsis_toggle expanded_s in
    (* let _ : unit React.event = button_event |> React.E.map set_expanded in *)
    let _ : unit React.event = button_event |> React.E.map (fun evt ->
      Stdio.printf "setting expanded %b\n" evt;
      set_expanded evt
    )
    in

    Stdio.printf "showing operator %s, expanded: %b\n" name (React.S.value expanded_s);
    let _: unit React.signal = expanded_s
      |> React.S.map ~eq:Unit.(=) (function
        | false ->
          Stdio.printf "enqueuing collapsed %s\n" name;
            Queue.enqueue queue (grid_tmpl
          [padded_txt depth (name ^ "("); button; txt ")" ]
          loc
        )
        | true ->
          Stdio.printf "enqueuing expanded %s\n" name;
          let open_elem = grid_tmpl [ padded_txt depth (name ^ "("); button ] loc in
          Queue.enqueue queue open_elem;

          scopes |> List.iteri ~f:(show_scope ~depth:(Int.succ depth) ~path ~map ~queue);

          let close_elem = grid_tmpl [ padded_txt depth ")" ] loc in
          Queue.enqueue queue close_elem
      )
    in
    ()

and show_scope ~depth ~path ~map ~queue i (Nominal.Scope (_pats, tms)) =
  (* TODO: show pattern *)
  let elem = txt "TODO: scope pattern" in
  Queue.enqueue queue elem;
  tms |> List.iteri ~f:(fun j -> show_tm ~depth ~path:((i, j)::path) ~map ~queue)

let view_tm tm =
  let map_ref = ref (Base.Map.empty (module Path)) in
  index_tm ~map_ref [] 0 tm;

  let r_elem = !map_ref
    |> Map.to_alist
    |> List.map ~f:(fun (_, { expanded_s; _ }) -> expanded_s)
    |> React.S.merge ~eq:(fun _ _ -> false) (fun () _ -> ()) ()
    |> React.S.map ~eq:(fun _ _ -> false) (fun () ->
      Stdio.printf "re-rendering\n";
      let map = !map_ref in
      let queue = Queue.create () in
      show_tm ~depth:0 ~path:[] ~map ~queue tm;
      Stdio.printf "queue length: %d\n" (Queue.length queue);
      Html.div (Queue.to_list queue)
    )
    |> RList.singleton_s
  in
  R.Html.div r_elem
