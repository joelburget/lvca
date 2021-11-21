open Base
open Lvca_syntax
open Lvca_models
open Omd

let ( >> ) = Lvca_util.( >> )

module Lang =
[%lvca.abstract_syntax_module
{|
char : *
int32 : *
string : *
option : * -> *
list : * -> *

attribute := Attribute(string; string)

link := Link_def(
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
|}
, { char = "Primitive.Char"
  ; int32 = "Primitive.Int32"
  ; string = "Primitive.String"
  ; option = "Option_model.Option"
  ; list = "List_model"
  }]

module Attrs = struct
  let inline = function
    | Lang.Inline.Concat (_, attrs, _)
    | Text (_, attrs, _)
    | Emph (_, attrs, _)
    | Strong (_, attrs, _)
    | Code (_, attrs, _)
    | Hard_break (_, attrs)
    | Soft_break (_, attrs)
    | Link (_, attrs, _)
    | Image (_, attrs, _)
    | Html (_, attrs, _) ->
      attrs
  ;;

  let block = function
    | Lang.Block.Paragraph (_, attrs, _)
    | List (_, attrs, _, _, _)
    | Blockquote (_, attrs, _)
    | Thematic_break (_, attrs)
    | Heading (_, attrs, _, _)
    | Code_block (_, attrs, _, _)
    | Html_block (_, attrs, _)
    | Definition_list (_, attrs, _) ->
      attrs
  ;;
end

module Of_omd = struct
  let here = Provenance.of_here [%here]

  let attribute : string * string -> Lang.Attribute.t =
   fun (x, y) -> Attribute (here, (here, x), (here, y))
 ;;

  let option : ('a -> 'b) -> 'a option -> 'b Option_model.Option.t =
   fun f opt -> opt |> Option.map ~f |> Option_model.of_option
 ;;

  let list : ('a -> 'b) -> 'a list -> 'b List_model.List.t =
   fun f lst -> lst |> List.map ~f |> List_model.of_list
 ;;

  let attributes : Omd.attributes -> Lang.Attribute.t List_model.List.t = list attribute

  let list_type : Omd.list_type -> Lang.List_type.t = function
    | Ordered (i, c) -> Ordered (here, (here, Int32.of_int_exn i), (here, c))
    | Bullet c -> Bullet (here, (here, c))
  ;;

  let list_spacing : Omd.list_spacing -> Lang.List_spacing.t = function
    | Loose -> Loose here
    | Tight -> Tight here
  ;;

  let rec inline : Omd.attributes Omd.inline -> Lang.Inline.t = function
    | Concat (attrs, inlines) -> Concat (here, attributes attrs, list inline inlines)
    | Text (attrs, str) -> Text (here, attributes attrs, (here, str))
    | Emph (attrs, inl) -> Emph (here, attributes attrs, inline inl)
    | Strong (attrs, inl) -> Strong (here, attributes attrs, inline inl)
    | Code (attrs, str) -> Code (here, attributes attrs, (here, str))
    | Hard_break attrs -> Hard_break (here, attributes attrs)
    | Soft_break attrs -> Soft_break (here, attributes attrs)
    | Html (attrs, str) -> Html (here, attributes attrs, (here, str))
    | Link (attrs, link_def) -> Link (here, attributes attrs, link link_def)
    | Image (attrs, link_def) -> Image (here, attributes attrs, link link_def)

  and link : Omd.attributes Omd.link -> Lang.Link.t =
   fun { label; destination; title } ->
    let title =
      match title with
      | None -> Option_model.Option.None here
      | Some x -> Some (here, (here, x))
    in
    Link_def (here, inline label, (here, destination), title)
 ;;

  let def_elt : Omd.attributes Omd.def_elt -> Lang.Def_elt.t =
   fun { term; defs } -> Def_elt (here, inline term, list inline defs)
 ;;

  let rec block : Omd.attributes Omd.block -> Lang.Block.t = function
    | Paragraph (attrs, inl) -> Paragraph (here, attributes attrs, inline inl)
    | List (attrs, t, s, blocks) ->
      List (here, attributes attrs, list_type t, list_spacing s, list (list block) blocks)
    | Blockquote (attrs, blocks) -> Blockquote (here, attributes attrs, list block blocks)
    | Thematic_break attrs -> Thematic_break (here, attributes attrs)
    | Heading (attrs, n, inl) ->
      Heading (here, attributes attrs, (here, Int32.of_int_exn n), inline inl)
    | Code_block (attrs, x, y) -> Code_block (here, attributes attrs, (here, x), (here, y))
    | Html_block (attrs, str) -> Html_block (here, attributes attrs, (here, str))
    | Definition_list (attrs, def_elts) ->
      Definition_list (here, attributes attrs, list def_elt def_elts)
  ;;

  let document : Omd.doc -> Lang.Doc.t = fun blocks -> Doc (here, list block blocks)
end

(*
let to_nonbinding
    : Lang.Doc.t -> (Nonbinding.term, Nonbinding.nominal_conversion_error) Result.t
  =
 fun doc -> doc |> Lang.Doc.to_nominal |> Nonbinding.of_nominal
;;
   *)

let term_of_doc : Omd.doc -> Lang.Doc.t = Of_omd.document
let parse : string -> Lang.Doc.t = Omd.of_string >> term_of_doc

let%test_module _ =
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
        Cons(List(Nil();
             Bullet('*');
             Tight();
             Cons(Cons(Paragraph(Nil(); Text(Nil(); "ul 1")); Nil());
             Cons(Cons(Paragraph(Nil(); Text(Nil(); "ul 2")); Nil()); Nil())));
        Cons(List(Nil();
             Ordered(1; '.');
             Tight();
             Cons(Cons(Paragraph(Nil(); Text(Nil(); "ol 1")); Nil());
             Cons(Cons(Paragraph(Nil(); Text(Nil(); "ol 2")); Nil()); Nil())));
        Cons(Code_block(Nil(); "nolang"; "foo
    "); Nil())))))) |}]
    ;;
  end)
;;
