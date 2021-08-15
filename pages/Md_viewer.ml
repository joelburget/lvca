open Base
open Brr
module Document = Lvca_languages.Document
open Document.Lang
module List_model = Lvca_core.List_model
module Option_model = Lvca_core.Option_model

let known_languages = Lvca_util.String.Set.of_list [ "edit"; "diff"; "ocaml" ]

module Brr = struct
  let mk_attr (Attribute.Attribute (_, (_, name), (_, value))) =
    Brr.At.v (Jstr.v name) (Jstr.v value)
  ;;

  let mk_attrs lst = lst |> List_model.to_list |> List.map ~f:mk_attr
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
        inlines |> List_model.to_list |> List.map ~f:of_inline |> List.concat
      | Text (_, _, (_, str)) -> [ txt' str ]
      | Emph (_, _, content) -> [ em ~at (of_inline content) ]
      | Strong (_, _, content) -> [ strong ~at (of_inline content) ]
      | Code (_, _, (_, content)) -> [ code ~at [ txt' content ] ]
      | Hard_break _ -> [ br ~at () ]
      | Soft_break _ -> []
      | Link (_, attrs, Link_def (_, label, (_, dest), title)) ->
        [ mk_link label dest title attrs ]
      | Image (_, attrs, Link_def (_, label, (_, dest), title)) ->
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
        let blocks = blocks |> List_model.to_list |> List.map ~f:List_model.to_list in
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
          |> List_model.to_list
          |> List.concat_map ~f:(fun (Def_elt (_, label, body)) ->
                 let body = body |> List_model.to_list |> List.concat_map ~f:of_inline in
                 dt (of_inline label) :: body)
        in
        dl ~at def_elts
      | Blockquote (_, _, blocks) ->
        blocks |> List_model.to_list |> List.map ~f:of_block |> blockquote ~at
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
      | Code_block (_, _, (_, cmd_str), (_, code_str)) ->
        let code_cls =
          if Set.mem known_languages cmd_str || String.(cmd_str = "")
          then "code-block"
          else "bg-red-200"
        in
        let at = Prelude.class' code_cls :: at in
        pre ~at [ txt' (cmd_str ^ "\n"); code [ txt' code_str ] ]
      | Html_block (_, _, _) -> failwith "TODO (html block)")
 ;;

  let of_doc : _ Doc.t -> Brr.El.t = function
    | Doc (_, blocks) ->
      blocks |> List_model.to_list |> List.map ~f:of_block |> Brr.El.div
  ;;
end

let view = Brr.of_doc
