open Bonsai_web
open Core_kernel
open Lvca
open Term_render

exception FailedRender of string

let str_of_tm = fun tm -> tm
  |> NonBinding.to_nominal
  |> Binding.Nominal.pp_term'

let render_inline_atom : NonBinding.term -> Vdom.Node.t
  = function
  | NonBinding.Operator ("inlineAtom", [ _attrs; Primitive(PrimString(str)) ])
  -> Vdom.Node.span [] [ Vdom.Node.text str ]
  | tm
  -> raise @@ FailedRender ("inline atom: unexpected term: " ^ str_of_tm tm)

let render_paragraph : NonBinding.term -> Vdom.Node.t
  = function
  | NonBinding.Operator ("inline", [Sequence(inline_atoms)])
  ->
    let inline_atoms' = List.map inline_atoms ~f:render_inline_atom in
    Vdom.Node.div [] inline_atoms'

  | tm -> raise @@ FailedRender
    ("render_paragraph: unexpected term: " ^ str_of_tm tm)
;;

let render_block : NonBinding.term -> Vdom.Node.t
  = function
  | NonBinding.Operator ("header", [ level; Primitive (PrimString str) ])
  -> (match level with
    | Operator ("h1", []) -> Vdom.Node.h1 [] [ Vdom.Node.text str ]
    | Operator ("h2", []) -> Vdom.Node.h2 [] [ Vdom.Node.text str ]
    | Operator ("h3", []) -> Vdom.Node.h3 [] [ Vdom.Node.text str ]
    | tm -> raise @@ FailedRender
      ("render_block header: unexpected term: " ^ str_of_tm tm)
  )
  | Operator("paragraph", [para]) -> render_paragraph para
  | tm -> raise @@ FailedRender
    ("render_block paragraph: unexpected term: " ^ str_of_tm tm)
;;

let render_doc : NonBinding.term -> Vdom.Node.t
  = function
  | Operator ("document", [Sequence(blocks)])
  ->
    let blocks' = blocks |> List.map ~f:render_block in
    Vdom.Node.div [] blocks'
  | tm
  -> raise @@ FailedRender ("render_doc: unexpected term: " ^ str_of_tm tm)
;;

let render_tm : Binding.Nominal.term -> Vdom.Node.t
  = fun tm -> match NonBinding.from_nominal tm with
  | None -> Vdom.Node.div [] [ Vdom.Node.text "Failed to convert term" ]
  | Some db_tm ->
    try
      render_doc db_tm
    with
      FailedRender msg -> Vdom.Node.div [] [ Vdom.Node.text msg ]
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone
    ~initial_input:(Binding.Nominal.Primitive (PrimString "foo"))
    ~initial_model:()
    ~bind_to_element_with_id:"app"
    (Bonsai.pure ~f:render_tm)
;;
