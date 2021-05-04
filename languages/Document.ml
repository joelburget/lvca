open Base
open Lvca_syntax
open Omd
open Stdio

(*
module Lang
    (Char : LanguageObject_intf.S)
    (Int : LanguageObject_intf.S)
    (String : LanguageObject_intf.S) =
      *)
module Lang =
functor
  (Char : LanguageObject_intf.S)
  (Int : LanguageObject_intf.S)
  (String : LanguageObject_intf.S)
  ->
  [%abstract_syntax_module
  {|
char : *
int : *
string : *

attributes :=
  | NoAttributes()
  | Attribute(string; string; attributes)

list_type :=
  | Ordered(int; char)
  | Bullet(char)

list_spacing := Loose() | Tight()

// inline :=
//   | Concat(attributes; inline_list)
//   | Text(attributes; string)
//   | Emph(attributes; inline)
//   | Strong(attributes; inline)
//   | Code(attributes; string)
//   | Hard_break(attributes)
//   | Soft_break(attributes)
//   // | Link()
//   // | Image()
//   | Html(attributes; string)

// inline_list :=
//   | InlineNil()
//   | InlineCons(inline; inline_list)

// block :=
//   | Paragraph(attributes)
//   | List(attributes; block_list)
//   | blockquote(attributes; block_list0
//   | Thematic_break(attributes)
//   | Heading(attributes; int; inline)
//   | Code_block(attributes; string; string)
//   | Html_block(attributes; string)
//   // | Definition_list(attributes; def_elt_list)

// block_list :=
//   | BlockNil()
//   | BlockCons(block; block_list)

// document := Document(block_list)
|}]

exception TranslationError of string

let term_of_option : f:('a -> unit NonBinding.term) -> 'a option -> unit NonBinding.term =
 fun ~f -> function
  | None -> Operator ((), "none", [])
  | Some a -> Operator ((), "some", [ f a ])
;;

let term_of_string : string -> unit NonBinding.term = fun str -> Primitive ((), String str)

(*
let term_of_attributes : Omd.attributes -> unit NonBinding.term =
 fun { id; classes; attributes } ->
  Operator
    ( "attributes"
    , [ term_of_option id ~f:term_of_string
      ; Sequence (List.map classes ~f:term_of_string)
      ; Sequence
          (List.map attributes ~f:(fun (x, y) ->
               NonBinding.Operator
                 ("pair", [ Primitive (PrimString x); Primitive (PrimString y) ])))
      ] )
;;
*)

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