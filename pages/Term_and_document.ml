open Base
open Brr
open Note
open Lvca_syntax
module Document = Lvca_languages.Document
open Document.Lang
module List_model = Lvca_core.List_model
module Option_model = Lvca_core.Option_model

type term = unit Doc.t

let parse = Lvca_languages.Document.parse

module Brr = struct
  let mk_attr (Attribute.Attribute (_, (_, name), (_, value))) =
    Brr.At.v (Jstr.v name) (Jstr.v value)
  ;;

  let mk_attrs lst = lst |> List_model.out |> List.map ~f:mk_attr
  let alt str = Brr.At.v (Jstr.v "alt") (Jstr.v str)

  let mk_img _label dest (title : _ Option_model.Option.t) attrs =
    let title_attr =
      match title with None _ -> [] | Some (_, (_, t)) -> At.[ title (Jstr.v t) ]
    in
    let at =
      (At.href (Jstr.v dest) :: At.[ src (Jstr.v dest) (* ; alt label *) ])
      @ title_attr
      @ mk_attrs attrs
    in
    Brr.El.img ~at ()
  ;;

  let rec mk_link label dest (title : _ Option_model.Option.t) attrs =
    let title_attr =
      match title with None _ -> [] | Some (_, (_, t)) -> At.[ title (Jstr.v t) ]
    in
    let at = (At.href (Jstr.v dest) :: title_attr) @ mk_attrs attrs in
    Brr.El.a ~at (of_inline label)

  and of_inline : _ Inline.t -> Brr.El.t list =
   fun inline ->
    Brr.El.(
      let at = mk_attrs (Document.Attrs.inline inline) in
      match inline with
      | Concat (_, _, inlines) ->
        inlines |> List_model.out |> List.map ~f:of_inline |> List.concat
      | Text (_, _, (_, str)) -> [ txt' str ]
      | Emph (_, _, content) -> [ em ~at (of_inline content) ]
      | Strong (_, _, content) -> [ strong ~at (of_inline content) ]
      | Code (_, _, (_, content)) -> [ code ~at [ txt' content ] ]
      | Hard_break _ -> [ br ~at () ]
      | Soft_break _ -> []
      | Link (_, attrs, LinkDef (_, label, (_, dest), title)) ->
        [ mk_link label dest title attrs ]
      | Image (_, attrs, LinkDef (_, label, (_, dest), title)) ->
        [ mk_img label dest title attrs ]
      | Html _ -> [ txt' "raw html not supported" ])
 ;;

  let rec of_block : _ Block.t -> Brr.El.t =
   fun block ->
    Brr.El.(
      let at = mk_attrs (Document.Attrs.block block) in
      match block with
      | Paragraph (_, _, inline) -> p ~at (of_inline inline)
      | List (_, _, list_ty, _, blocks) ->
        let f, bullet_char =
          match list_ty with
          | Ordered (_, _, (_, c)) -> ol, c
          | Bullet (_, (_, c)) -> ul, c
        in
        let blocks = blocks |> List_model.out |> List.map ~f:List_model.out in
        let blocks = blocks |> List.concat_map ~f:(List.map ~f:of_block) in
        let elem = f ~at blocks in
        set_inline_style
          ~important:true
          (Jstr.v "list-style-type")
          (Jstr.v (Printf.sprintf "%C" bullet_char))
          elem;
        elem
      | Definition_list (_, _, def_elts) ->
        let def_elts =
          def_elts
          |> List_model.out
          |> List.concat_map ~f:(fun (Def_elt (_, label, body)) ->
                 let body = body |> List_model.out |> List.concat_map ~f:of_inline in
                 dt (of_inline label) :: body)
        in
        dl ~at def_elts
      | Blockquote (_, _, blocks) ->
        blocks |> List_model.out |> List.map ~f:of_block |> blockquote ~at
      | Thematic_break (_, _) -> hr ~at ()
      | Heading (_, _, (_, level), inline) ->
        let f =
          match Int32.to_int_exn level with
          | 1 -> h1
          | 2 -> h2
          | 3 -> h3
          | 4 -> h4
          | 5 -> h5
          | _ -> h6
        in
        f ~at (of_inline inline)
      | Code_block (_, _, _, (_, str)) -> pre ~at [ code [ txt' str ] ]
      | Html_block (_, _, _) -> failwith "TODO (html block)")
 ;;

  let of_doc : _ Doc.t -> Brr.El.t = function
    | Doc (_, blocks) -> blocks |> List_model.out |> List.map ~f:of_block |> Brr.El.div
  ;;
end

module Model = struct
  type t =
    { input : string
    ; result : term
    }

  let input =
    {|# document

paragraph

* li 1
* li 2

First Term
: This is the definition of the first term.

Second Term
: This is one definition of the second term.
: This is another definition of the second term.

***

* This is the first list item.
* Here's the second list item.

    > A blockquote would look great below the second list item.

* And here's the third list item.

---

1. Open the file.
2. Find the following code block on line 21:

        <html>
          <head>
            <title>Test</title>
          </head>

3. Update the title to match the name of your website.|}
  ;;

  let initial_model = { input; result = parse input }

  module Doc = Nominal.Convertible.Extend (Doc)

  let pp ppf { input; result } =
    Fmt.pf ppf "{ input = %s; result = %a }" input Doc.pp result
  ;;

  let ( = ) m1 m2 =
    let term_eq x y =
      Nominal.Term.equal ~info_eq:Unit.( = ) (Doc.to_nominal x) (Doc.to_nominal y)
    in
    String.(m1.input = m2.input) && term_eq m1.result m2.result
  ;;
end

module Action = struct
  type t = Evaluate of string
end

module Controller = struct
  let update (action : Action.t) _model =
    match action with Evaluate iput -> Model.{ input = iput; result = parse iput }
  ;;
end

module View = struct
  let view model_s =
    let input_s = S.map ~eq:String.( = ) (fun Model.{ input; _ } -> input) model_s in
    let input_elem, input_evt = Multiline_input.mk input_s in
    let enter_input_e =
      input_evt
      |> E.filter_map (function
             | Common.EvaluateInput str -> Some (Action.Evaluate str)
             | _ -> None)
    in
    let output_view =
      model_s |> S.map ~eq:phys_equal (fun model -> Brr.of_doc model.Model.result)
    in
    let elem = El.(div [ input_elem; Prelude.mk_reactive' div output_view ]) in
    enter_input_e, elem
  ;;
end

let stateless_view () =
  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum ~eq:Model.( = ) (S.value model_s) do_action in
    model_s', (model_s', elem)
  in
  let model_s, elem = S.fix ~eq:Model.( = ) Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
