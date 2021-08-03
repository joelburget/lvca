open Base
open Lvca_syntax
open Omd
module Option_model = Lvca_core.Option_model.Option
module List_model = Lvca_core.List_model.List

let ( >> ) = Lvca_util.( >> )

module Lang =
[%lvca.abstract_syntax_module
{|
char : *  // module Primitive.Char
int32 : *  // module Primitive.Int32
string : *  // module Primitive.String
option : * -> *  // module Option_model
list : * -> *  // module List_model

attribute := Attribute(string; string)

link := LinkDef(
  inline;
  string;
  option string
)

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

def_elt := Def_elt(inline; list inline)

block :=
  | Paragraph(list attribute; inline)
  | List(list attribute; list_type; list_spacing; list (list block))
  | Blockquote(list attribute; list block)
  | Thematic_break(list attribute)
  | Heading(list attribute; int32; inline)
  | Code_block(list attribute; string; string)
  | Html_block(list attribute; string)
  | Definition_list(list attribute; list def_elt)

doc := Doc(list block)
|}]

module Of_omd = struct
  let attribute : string * string -> Lang.Plain.attribute = fun (x, y) -> Attribute (x, y)

  let option : ('a -> 'b) -> 'a option -> 'b Option_model.Plain.t =
   fun f -> Lvca_core.Option_model.(into ~empty_info:() >> Option.to_plain f)
 ;;

  let list : ('a -> 'b) -> 'a list -> 'b List_model.Plain.t =
   fun f -> Lvca_core.List_model.(into ~empty_info:() >> List.to_plain f)
 ;;

  let attributes : Omd.attributes -> Lang.Plain.attribute List_model.Plain.t =
    list attribute
  ;;

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
    LinkDef (inline label, destination, option Fn.id title)
 ;;

  let def_elt : Omd.attributes Omd.def_elt -> Lang.Plain.def_elt =
   fun { term; defs } -> Def_elt (inline term, list inline defs)
 ;;

  let rec block : Omd.attributes Omd.block -> Lang.Plain.block = function
    | Paragraph (attrs, inl) -> Paragraph (attributes attrs, inline inl)
    | List (attrs, t, s, blocks) ->
      List (attributes attrs, list_type t, list_spacing s, list (list block) blocks)
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

let to_nonbinding
    :  'info Lang.Doc.t
    -> ('info Nonbinding.term, 'info Nonbinding.nominal_conversion_error) Result.t
  =
 fun doc -> doc |> Lang.Doc.to_nominal |> Nonbinding.of_nominal
;;

let term_of_doc : Omd.doc -> unit Lang.Doc.t = Of_omd.document >> Lang.Doc.of_plain
let parse : string -> unit Lang.Doc.t = Omd.of_string >> term_of_doc

let%test_module "markdown" =
  (module struct
    module Doc = Nominal.Convertible.Extend (Lang.Doc)

    let print_parse str = Fmt.pr "%a" Doc.pp (parse str)

    let%expect_test _ =
      print_parse
        {|
# hello, world

para

* ul 1
* ul 2

1. ol 1
2. ol 2

```nolang
foo
```
    |};
      [%expect
        {|
    Doc(Cons(Heading(Nil(); 1; Text(Nil(); "hello, world"));
        Cons(Paragraph(Nil(); Text(Nil(); "para"));
        Cons(List(Nil(); Bullet('*'); Tight(); Cons(Cons(Paragraph(Nil(); Text(Nil(); "ul 1")); Nil()); Cons(Cons(Paragraph(Nil(); Text(Nil(); "ul 2")); Nil()); Nil())));
        Cons(List(Nil(); Ordered(1; '.'); Tight(); Cons(Cons(Paragraph(Nil(); Text(Nil(); "ol 1")); Nil()); Cons(Cons(Paragraph(Nil(); Text(Nil(); "ol 2")); Nil()); Nil())));
        Cons(Code_block(Nil(); "nolang"; "foo
    "); Nil())))))) |}]
    ;;
  end)
;;
