open Base
open Lvca_syntax
open Omd
open Stdio

module Lang =
[%abstract_syntax_module
{|
char : *
int : *
string : *

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
  | Ordered(int; char)
  | Bullet(char)

list_spacing := Loose() | Tight()

inline_desc :=
  | Concat(list inline)
  | Text(string)
  | Emph(inline)
  | Strong(inline)
  | Code(string)
  | Hard_break()
  | Soft_break()
  | Link(link)
  | Image(link)
  | Html(string)

inline := Inline(inline_desc; list attribute)

def_elt := DefElt(inline; list inline)

block_desc :=
  | Paragraph(inline)
  | List(list_type; list_spacing; list block)
  | Blockquote(list block)
  | Thematic_break()
  | Heading(int; inline)
  | Code_block(string; string)
  | Html_block(string)
  | Definition_list(list def_elt)

block := Block(block_desc; list attribute)

doc := Doc(list block)
|}]

exception TranslationError of string

let term_of_option : f:('a -> unit NonBinding.term) -> 'a option -> unit NonBinding.term =
 fun ~f -> function
  | None -> Operator ((), "none", [])
  | Some a -> Operator ((), "some", [ f a ])
;;

let term_of_string : string -> unit NonBinding.term = fun str -> Primitive ((), String str)

module Of_omd = struct
  module Lang = Lang (Primitive.Char) (Primitive.Int) (Primitive.String)

  let attribute : string * string -> Lang.Plain.attribute = fun (x, y) -> Attribute (x, y)

  let maybe : ('a -> 'b) -> 'a option -> 'b Lang.Plain.maybe =
   fun f -> function None -> Nothing | Some a -> Just (f a)
 ;;

  let rec list : ('a -> 'b) -> 'a list -> 'b Lang.Plain.list =
   fun f -> function [] -> Nil | x :: xs -> Cons (f x, list f xs)
 ;;

  let attributes : Omd.attributes -> Lang.Plain.attribute Lang.Plain.list = list attribute

  let list_type : Omd.list_type -> Lang.Plain.list_type = function
    | Ordered (i, c) -> Ordered (i, c)
    | Bullet c -> Bullet c
  ;;

  let list_spacing : Omd.list_spacing -> Lang.Plain.list_spacing = function
    | Loose -> Loose
    | Tight -> Tight
  ;;

  let rec inline : Omd.inline -> Lang.Plain.inline =
   fun { il_desc; il_attributes } -> Inline (inline_desc il_desc, attributes il_attributes)

  and inline_desc : Omd.inline_desc -> Lang.Plain.inline_desc = function
    | Concat inlines -> Concat (list inline inlines)
    | Text str -> Text str
    | Emph inl -> Emph (inline inl)
    | Strong inl -> Strong (inline inl)
    | Code str -> Code str
    | Hard_break -> Hard_break
    | Soft_break -> Soft_break
    | Html str -> Html str
    | Link link_def -> Link (link link_def)
    | Image link_def -> Image (link link_def)

  and link : Omd.link -> Lang.Plain.link =
   fun { label; destination; title } ->
    LinkDef (inline label, destination, maybe Fn.id title)
 ;;

  let def_elt : Omd.def_elt -> Lang.Plain.def_elt =
   fun { term; defs } -> DefElt (inline term, list inline defs)
 ;;

  let rec block_desc : Omd.block_desc -> Lang.Plain.block_desc = function
    | Paragraph inl -> Paragraph (inline inl)
    | Thematic_break -> Thematic_break
    | Heading (n, inl) -> Heading (n, inline inl)
    | Code_block (x, y) -> Code_block (x, y)
    | Html_block str -> Html_block str
    | List (t, s, blocks) ->
      (match blocks with
      | [ blocks ] -> List (list_type t, list_spacing s, list block blocks)
      | _ -> failwith "TODO")
    | Blockquote blocks -> Blockquote (list block blocks)
    | Definition_list def_elts -> Definition_list (list def_elt def_elts)

  and block : Omd.block -> Lang.Plain.block =
   fun { bl_desc; bl_attributes } -> Block (block_desc bl_desc, attributes bl_attributes)
 ;;

  let document : Omd.doc -> Lang.Plain.doc = fun blocks -> Doc (list block blocks)
end

let mk_sequence tms = NonBinding.Operator ((), "sequence", tms)

let rec term_of_inline_desc : Omd.inline_desc -> unit NonBinding.term = function
  | Concat inlines ->
    Operator ((), "concat", List.map inlines ~f:term_of_inline) (* XXX: convert to list *)
  | Text str -> Primitive ((), String str)
  | _ -> raise (TranslationError "Unsupported inline type")

and term_of_inline : Omd.inline -> unit NonBinding.term =
 fun { il_desc; il_attributes = _TODO } -> term_of_inline_desc il_desc
;;

let term_of_inline_block_desc : Omd.block_desc -> unit NonBinding.term = function
  | Paragraph inline -> term_of_inline inline
  | List (_list_type, _list_spacing, _blocks) ->
    raise (TranslationError "TODO: list blocks")
  | Blockquote _ -> raise (TranslationError "TODO: blockquote")
  | Heading (level, inline) ->
    Operator
      ( ()
      , "heading"
      , [ Primitive ((), Integer (Z.of_int level))
        ; term_of_inline inline
          (* ; term_of_attributes attributes *)
        ] )
  (* | Code_block { kind = _; label; other = _; code; attributes } -> *)
  | Code_block (label, code) ->
    Operator
      ( ()
      , "code_block"
      , [ term_of_string label
        ; term_of_string code
          (* ; term_of_attributes attributes *)
        ] )
  | Thematic_break -> Operator ((), "thematic_break", [])
  | _ -> raise (TranslationError "Unsupported block type")
;;

let term_of_inline_block : Omd.block -> unit NonBinding.term =
 fun { bl_desc; bl_attributes = _TODO } -> term_of_inline_block_desc bl_desc
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

let parse : string -> (unit NonBinding.term, string) Result.t =
 fun str ->
  try Ok (str |> Omd.of_string |> List.map ~f:term_of_inline_block |> mk_sequence) with
  | TranslationError msg -> Error msg
;;

let%test_module "markdown" =
  (module struct
    let print_parse str =
      match parse str with
      | Ok tm -> Fmt.pr "%a" NonBinding.pp tm
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
