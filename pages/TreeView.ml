open Lvca_syntax
open ReactiveData
open Base
open Js_of_ocaml_tyxml.Tyxml_js

let a_class, txt, div, ul, li = Html.(a_class, txt, div, ul, li)

let view_loc loc = loc |> SourceRanges.to_string |> txt

let rec view_tm = function
  | Nominal.Primitive (loc, p) ->
    div [ p |> Primitive.to_string |> txt; view_loc loc ]
  | Var (loc, name) ->
    div [ txt name; view_loc loc ]
  | Operator (loc, name, scopes) ->
    if List.is_empty scopes
    then div [ txt name; view_loc loc ]
    else
      let expanded, set_expanded = React.S.create ~eq:Bool.(=) false in
      let button_event, button = Components.chevron_toggle expanded in
      let _ : unit React.event = button_event |> React.E.map set_expanded in
      let scope_children = expanded
        |> React.S.map (function
          | true -> scopes |> List.map ~f:(fun scope -> li [view_scope scope])
          | false -> []
        )
        |> RList.from_signal
        |> R.Html.ul ~a:[a_class ["list-none"]]
      in
      div
        [ txt name
        ; view_loc loc
        ; button
        ; scope_children
        ]

and view_scope (Nominal.Scope (_pats, tms)) = ul
  ~a:[a_class ["list-none"]]
  (tms |> List.map ~f:(fun tm -> li [view_tm tm]))
