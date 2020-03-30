(* from https://github.com/ocaml/omd

ISC License

Copyright (c) 2013-2018, Philippe Wang <philippe.wang@cl.cam.ac.uk>
Copyright (c) 2018, Nicolás Ojeda Bär <n.oje.bar@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
(** A markdown parser in OCaml. *)

module Attributes = Ast.Attributes

module Link_def = Ast.Link_def
module Block_list = Ast.Block_list
module Code_block = Ast.Code_block
module Heading = Ast.Heading
module Def_list = Ast.Def_list
module Tag_block = Ast.Tag_block

type 'a block = 'a Ast.block =
  | Paragraph of 'a
  | List of 'a block Block_list.t
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of 'a Heading.t
  | Code_block of Code_block.t
  | Html_block of string
  | Link_def of string Link_def.t
  | Def_list of 'a Def_list.t
  | Tag_block of 'a block Tag_block.t

module Emph = Ast.Emph
module Code = Ast.Code
module Link = Ast.Link
module Ref = Ast.Ref
module Tag = Ast.Tag

type inline = Ast.inline =
  | Concat of inline list
  | Text of string
  | Emph of inline Emph.t
  | Code of Code.t
  | Hard_break
  | Soft_break
  | Link of inline Link.t
  | Ref of inline Ref.t
  | Html of string
  | Tag of inline Tag.t

type t = inline block list
(** A markdown document *)

val of_string: string -> t
