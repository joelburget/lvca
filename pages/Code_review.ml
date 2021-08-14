open Base
open Brr
open Note
module Document = Lvca_languages.Document

let page_source = [%blob "make-code-review-easier.md"]
let page_doc = Document.parse page_source

module Model = struct
  type t = unit

  let initial_model = ()
  let ( = ) _ _ = true
end

module Action = struct
  type t = Nop
end

module Controller = struct
  let update (action : Action.t) model = match action with Nop -> model
end

module View = struct
  open El

  let rec to_list = function
    | Lvca_core.List_model.List.Plain.Nil -> []
    | Cons (x, xs) -> x :: to_list xs
  ;;

  let known_languages = Lvca_util.String.Set.of_list [ "edit"; "diff"; "ocaml" ]

  (* It gets shadowed by something... *)
  module D = Document

  let rec block : Document.Lang.Block.Plain.t -> Brr.El.t =
    let open Brr in
    El.(
      function
      | Paragraph (_attrs, i) -> p (inline i)
      | List (_, list_type, spacing, blocks) ->
        let f = match list_type with Ordered _ -> ul | Bullet _ -> ol in
        let at =
          match list_type with
          | Ordered (n, _) when Int32.(n <> one) ->
            [ At.int (Jstr.v "start") (Int32.to_int_exn n) ]
          | _ -> []
        in
        let li t =
          let block' t =
            match t, spacing with
            | D.Lang.Block.Plain.Paragraph (_, t), Tight -> inline t
            | _ -> [ block t ]
          in
          (* let nl = match spacing with Tight -> [] | _ -> nl in *)
          li (List.concat_map ~f:block' t)
        in
        let blocks : D.Lang.Wrapper.Plain.block list list =
          blocks |> to_list |> List.map ~f:to_list
        in
        let blocks : El.t list = List.map ~f:li blocks in
        f ~at blocks
      | Blockquote (_attrs, blocks) -> blockquote (blocks |> to_list |> List.map ~f:block)
      | Heading (_attrs, level, i) ->
        let node_creator =
          match Int32.to_int_exn level with
          | 1 -> h1
          | 2 -> h2
          | 3 -> h3
          | 4 -> h4
          | 5 -> h5
          | _ -> h6
          (* TODO: error *)
        in
        node_creator (inline i)
      | Code_block (_attrs, cmd_str, code_str) ->
        let at =
          if Set.mem known_languages cmd_str || String.(cmd_str = "")
          then Prelude.classes "code-block"
          else [ Prelude.class' "bg-red-200" ]
        in
        pre ~at [ txt' (cmd_str ^ "\n"); code [ txt' code_str ] ]
      | Thematic_break _ -> hr ()
      | Html_block _ -> txt' "html blocks not supported"
      | Definition_list _ -> txt' "definition lists not supported")

  and inline : Document.Lang.Inline.Plain.t -> Brr.El.t list = function
    | Concat (_attrs, inlines) ->
      let inlines = to_list inlines in
      List.concat_map ~f:inline inlines
    | Text (_attrs, str) -> [ txt' str ]
    | Emph (_attrs, inl) -> [ em (inline inl) ]
    | Strong (_attrs, inl) -> [ strong (inline inl) ]
    | Code (_attrs, str) -> [ code [ txt' str ] ]
    | Hard_break _attrs -> [ br () ]
    | Soft_break _attrs -> []
    | Html (_attrs, _str) -> [ txt' "TODO: Html" ]
    | Link (_attrs, _link_def) -> [ txt' "TODO: link" ]
    | Image (_attrs, _link_def) -> [ txt' "TODO: image" ]
  ;;

  let doc : Document.Lang.Doc.Plain.t -> Brr.El.t =
   fun (Doc blocks) ->
    let blocks = to_list blocks in
    div (List.map ~f:block blocks)
 ;;

  let view _model_s = E.select [], page_doc |> Document.Lang.Doc.to_plain |> doc
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
