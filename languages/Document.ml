open Base
open Lvca_syntax
open Omd
open Stdio

module Lang =
[%lvca.abstract_syntax_module
{|
char : *  // module Primitive.Char
int32 : *  // module Primitive.Int32
string : *  // module Primitive.String

maybe a := Nothing() | Just(a)

attribute := Attribute(string; string)

link := LinkDef(
  inline;
  string;
  maybe string
)

list a :=
  | Nil()
  | Cons(a; list a)

list_type :=
  | Ordered(int32; char)
  | Bullet(char)

list_spacing := Loose() | Tight()

inline :=
  | Concat(list attribute; list inline)
  | Text(list attribute; string)
  | Emph(list attribute; inline)
  | Strong(list attribute; inline)
  | Code(list attribute; string)
  | Hard_break(list attribute)
  | Soft_break(list attribute)
  | Link(list attribute; link)
  | Image(list attribute; link)
  | Html(list attribute; string)

def_elt := DefElt(inline; list inline)

block :=
  | Paragraph(list attribute; inline)
  | List(list attribute; list_type; list_spacing; list block)
  | Blockquote(list attribute; list block)
  | Thematic_break(list attribute)
  | Heading(list attribute; int32; inline)
  | Code_block(list attribute; string; string)
  | Html_block(list attribute; string)
  | Definition_list(list attribute; list def_elt)

doc := Doc(list block)
|}]

exception TranslationError of string

let term_of_option : f:('a -> unit Nonbinding.term) -> 'a option -> unit Nonbinding.term =
 fun ~f -> function
  | None -> Operator ((), "none", [])
  | Some a -> Operator ((), "some", [ f a ])
;;

let term_of_string : string -> unit Nonbinding.term = fun str -> Primitive ((), String str)

module Of_omd = struct
  let attribute : string * string -> Lang.Plain.attribute = fun (x, y) -> Attribute (x, y)

  let maybe : ('a -> 'b) -> 'a option -> 'b Lang.Plain.maybe =
   fun f -> function None -> Nothing | Some a -> Just (f a)
 ;;

  let rec list : ('a -> 'b) -> 'a list -> 'b Lang.Plain.list =
   fun f -> function [] -> Nil | x :: xs -> Cons (f x, list f xs)
 ;;

  let attributes : Omd.attributes -> Lang.Plain.attribute Lang.Plain.list = list attribute

  let list_type : Omd.list_type -> Lang.Plain.list_type = function
    | Ordered (i, c) -> Ordered (Int32.of_int_exn i, c)
    | Bullet c -> Bullet c
  ;;

  let list_spacing : Omd.list_spacing -> Lang.Plain.list_spacing = function
    | Loose -> Loose
    | Tight -> Tight
  ;;

  let rec inline : Omd.attributes Omd.inline -> Lang.Plain.inline = function
    | Concat (attrs, inlines) -> Concat (attributes attrs, list inline inlines)
    | Text (attrs, str) -> Text (attributes attrs, str)
    | Emph (attrs, inl) -> Emph (attributes attrs, inline inl)
    | Strong (attrs, inl) -> Strong (attributes attrs, inline inl)
    | Code (attrs, str) -> Code (attributes attrs, str)
    | Hard_break attrs -> Hard_break (attributes attrs)
    | Soft_break attrs -> Soft_break (attributes attrs)
    | Html (attrs, str) -> Html (attributes attrs, str)
    | Link (attrs, link_def) -> Link (attributes attrs, link link_def)
    | Image (attrs, link_def) -> Image (attributes attrs, link link_def)

  and link : Omd.attributes Omd.link -> Lang.Plain.link =
   fun { label; destination; title } ->
    LinkDef (inline label, destination, maybe Fn.id title)
 ;;

  let def_elt : Omd.attributes Omd.def_elt -> Lang.Plain.def_elt =
   fun { term; defs } -> DefElt (inline term, list inline defs)
 ;;

  let rec block : Omd.attributes Omd.block -> Lang.Plain.block = function
    | Paragraph (attrs, inl) -> Paragraph (attributes attrs, inline inl)
    | List (attrs, t, s, blocks) ->
      (match blocks with
      | [ blocks ] ->
        List (attributes attrs, list_type t, list_spacing s, list block blocks)
      | _ -> failwith "TODO")
    | Blockquote (attrs, blocks) -> Blockquote (attributes attrs, list block blocks)
    | Thematic_break attrs -> Thematic_break (attributes attrs)
    | Heading (attrs, n, inl) -> Heading (attributes attrs, Int32.of_int_exn n, inline inl)
    | Code_block (attrs, x, y) -> Code_block (attributes attrs, x, y)
    | Html_block (attrs, str) -> Html_block (attributes attrs, str)
    | Definition_list (attrs, def_elts) ->
      Definition_list (attributes attrs, list def_elt def_elts)
  ;;

  let document : Omd.doc -> Lang.Plain.doc = fun blocks -> Doc (list block blocks)
end

let mk_sequence tms = Nonbinding.Operator ((), "sequence", tms)

let rec term_of_inline : Omd.attributes Omd.inline -> unit Nonbinding.term = function
  | Concat (_, inlines) ->
    Operator ((), "concat", List.map inlines ~f:term_of_inline) (* XXX: convert to list *)
  | Text (_, str) -> Primitive ((), String str)
  | _ -> raise (TranslationError "Unsupported inline type")
;;

let term_of_inline_block : Omd.attributes Omd.block -> unit Nonbinding.term = function
  | Paragraph (_, inline) -> term_of_inline inline
  | List (_, _list_type, _list_spacing, _blocks) ->
    raise (TranslationError "TODO: list blocks")
  | Blockquote _ -> raise (TranslationError "TODO: blockquote")
  | Heading (_, level, inline) ->
    Operator
      ( ()
      , "heading"
      , [ Primitive ((), Integer (Z.of_int level))
        ; term_of_inline inline
          (* ; term_of_attributes attributes *)
        ] )
  (* | Code_block { kind = _; label; other = _; code; attributes } -> *)
  | Code_block (_, label, code) ->
    Operator
      ( ()
      , "code_block"
      , [ term_of_string label
        ; term_of_string code
          (* ; term_of_attributes attributes *)
        ] )
  | Thematic_break _ -> Operator ((), "thematic_break", [])
  | _ -> raise (TranslationError "Unsupported block type")
;;

(*
let rec dom_of_inline : Omd.inline -> Brr.El.t list =
  Brr.El.(
    function
    | Concat inlines -> inlines |> List.map ~f:dom_of_inline |> List.concat
    | Text str -> [ text str ]
    | Emph { style = _; kind; content } ->
      [ (match kind with
        | Normal -> create "em" [] (dom_of_inline content)
        | Strong -> strong [] (dom_of_inline content))
      ]
    | Code { level = _; content; attributes = _ } -> [ code [] [ text content ] ]
    | Hard_break -> [ br [] ]
    | Soft_break | Link _ (* inline Link.t *) -> [ text "link not supported" ]
    | Ref _ (* inline Ref.t *) -> [ text "ref not supported" ]
    | Html _ (* string *) -> [ text "html not supported" ]
    | Tag _ (* inline Tag.t *) -> [ text "tag not supported" ])
;;

let error_block msg = Brr.El.(div [] [ text msg ])

let eval_inline_block : store -> Omd.inline Omd.block -> Brr.El.t =
 fun store ->
  Brr.El.(
    function
    | Paragraph inline -> p [] (dom_of_inline inline)
    | List _blocks -> text "TODO eval_inline_block: list blocks"
    | Blockquote _ -> text "TODO eval_inline_block: blockquote"
    | Heading { level; text; attributes = _ } ->
      let node_creator =
        match level with 1 -> h1 | 2 -> h2 | 3 -> h3 | 4 -> h4 | 5 -> h5 | _ -> h6
        (* TODO: error *)
      in
      node_creator [ (* TODO: attributes *) ] (dom_of_inline text)
    | Code_block _ (* { kind = _; label = _; other = _; code; attributes = _ } *) ->
      failwith "TODO"
      (* printf "label: '%s'\n" (match label with | None -> "none" | Some l -> l);
         printf "other: '%s'\n" (match other with | None -> "none" | Some l -> l);
         printf "code: '%s'\n" (match code with | None -> "none" | Some l -> l);

         printf "attributes:\n- id: %s\n- classes: [%s]\n- attributes: [%s]\n"
         (match attributes.id with | None -> "none" | Some str -> str) (attributes.classes
         |> String.concat ~sep:"; ") (attributes.attributes |> List.map ~f:(fun (x, y) ->
         Printf.sprintf {|"%s", "%s"|} x y) |> String.concat ~sep:"; ");

         (match code with | None -> error_block "No code found in code block" | Some
         headered_code -> (match String.lsplit2 headered_code ~on:'\n' with | Some
         (cmd_str, code_str) -> (match parse_command cmd_str with | Ok cmd ->
         printf "code_str:\n%s\n" code_str; eval_command store cmd code_str | Error
         msg -> text msg) | None -> error_block (Printf.sprintf "Single line code block
         found -- must include a header: %s" headered_code))) *)
    | Thematic_break -> hr []
    | Html_block _ -> text "html blocks not supported"
    | Link_def _ (* string Link_def.t *) -> text "TODO: link defs"
    | Def_list _ (* 'a Def_list.t *) -> text "TODO: def lists"
    | Tag_block _ (* 'a block Tag_block.t *) -> text "TODO: tag blocks")
;;

let evaluate_and_produce_dom : store -> string -> Brr.El.t =
 fun store str ->
  let md = Omd.of_string str in
  Brr.El.div [] (List.map md ~f:(eval_inline_block store))
;;
*)

let parse : string -> (unit Nonbinding.term, string) Result.t =
 fun str ->
  try Ok (str |> Omd.of_string |> List.map ~f:term_of_inline_block |> mk_sequence) with
  | TranslationError msg -> Error msg
;;

let%test_module "markdown" =
  (module struct
    let print_parse str =
      match parse str with
      | Ok tm -> Fmt.pr "%a" Nonbinding.pp tm
      | Error msg -> print_string msg
    ;;

    let%expect_test _ =
      print_parse {|
# hello, world

para

```nolang
foo
```
    |};
      [%expect
        {|
    sequence(heading(1; "hello, world"); "para"; code_block("nolang"; "foo
    ")) |}]
    ;;
  end)
;;
