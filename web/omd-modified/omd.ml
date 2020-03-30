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
include Ast
module Parser = Omd_parser

type t = inline block list

let parse_inlines md =
  let parse_inline defs s = Parser.inline defs (Parser.P.of_string s) in
  let defs = Ast.defs md in
  let defs =
    List.map (fun (def: string Link_def.t) -> {def with label = Parser.normalize def.label}) defs
  in
  List.map (Ast.map (parse_inline defs)) md

let of_string s =
  let md = Block.Pre.of_string s in
  parse_inlines md