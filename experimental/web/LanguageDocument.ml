open Bonsai_web
open Core_kernel
open Lvca
open Lvca_omd
open Store

let commands_abstract_syntax =
  {|
{ string, maybe }

// maybe(a) := nothing() | just(a)

sha_or_name := sha(string()) | name(string())

// A pair of abstract syntax and optionally concrete syntax
language := language(sha_or_name(); maybe(sha_or_name()))

command :=
  | define(
    maybe(string()); // name
    language()
  )
  | eval(
    sha_or_name(); // dynamics
    language() // target language
  )
  |}
;;

(* let commands_concrete_syntax = let str = {| DEFINE := "define" EVAL := "eval" COLON :=
   ":" LANGLE := "<" RANGLE := ">" SHA := /[0-9a-f]{64}/ IDENT := /[0-9a-zA-Z_]+/ STRING
   := /\{\{.*\}\}/

   sha_or_name := | sha = SHA { sha(string(sha)) } | ident = IDENT { name(string(ident)) }

   language := | abstract = sha_or_name LANGLE concrete = sha_or_name RANGLE {
   language(abstract; just(concrete)) } | abstract = sha_or_name { language(abstract;
   nothing()) }

   command := | DEFINE ident = IDENT COLON lang = language { define(just(string(ident));
   lang) } | EVAL sha_or_name = sha_or_name lang = language { eval(sha_or_name; lang) } |}
   in match Parse_concrete.parse str with | Error err -> failwith (ParseError.to_string
   err) | Ok (pre_terminal_rules, sort_rules) -> let desc =
   ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules in (match
   ConcreteSyntax.check_description_validity desc with | Some (InvalidGrammar msg) ->
   failwith msg | None -> desc) ;;

   let parse_command : string -> (NonBinding.term, string) Result.t = fun str ->
   Printf.printf "parsing command: '%s'\n" str; let str' = String.slice str 1 (-1) in
   match ConcreteSyntax.parse commands_concrete_syntax "command" str' with | Error _err ->
   Error (* (ParseError.to_string err) *) "parse error" | Ok tree -> match
   ConcreteSyntax.to_ast commands_concrete_syntax tree with | Error msg -> Error msg | Ok
   ast -> match NonBinding.from_nominal ast with | None -> Error "Failed to convert ast to
   non-binding" | Some tm -> Ok tm *)

let lookup_lang : store -> NonBinding.term -> store_value =
 fun { term_store; name_store } -> function
  | Operator ("sha", [ Primitive (PrimString sha_str) ]) -> lookup_sha term_store sha_str
  | Operator ("name", [ Primitive (PrimString name) ]) ->
    Hashtbl.find_exn name_store name |> lookup_sha term_store
  | tm ->
    failwith
      ("Failed to look up language term "
      ^ (tm |> NonBinding.to_nominal |> Binding.Nominal.pp_term'))
;;

let term_of_maybe : NonBinding.term -> NonBinding.term option = function
  | Operator ("just", [ a ]) -> Some a
  | Operator ("nothing", []) -> None
  | _ -> failwith "term_of_maybe: unexpected term"
;;

let lookup_maybe_concrete : store -> NonBinding.term -> store_value =
 fun store tm ->
  Printf.printf
    "lookup_maybe_concrete tm: %s\n"
    (tm |> NonBinding.to_nominal |> Binding.Nominal.pp_term');
  let tm' = term_of_maybe tm in
  match tm' with
  | None -> GenesisTermConcrete
  | Some tm'' ->
    Printf.printf
      "lookup_maybe_concrete tm'': %s\n"
      (tm'' |> NonBinding.to_nominal |> Binding.Nominal.pp_term');
    lookup_lang store tm''
;;

type parsed =
  | ParsedTerm of Binding.Nominal.term
  | ParsedAbstract of AbstractSyntax.abstract_syntax
  | ParsedStatics of Statics.rule list
  | ParsedDynamics of Core.Types.term

(* | ParsedParseable of parseable' *)

let term_of_parsed : parsed -> Binding.Nominal.term = function
  | ParsedTerm tm -> tm
  | ParsedAbstract abstract_syntax ->
    abstract_syntax |> AbstractSyntax.to_term |> NonBinding.to_nominal
  | ParsedStatics rules -> Statics.to_term rules
  | ParsedDynamics dynamics -> Dynamics.Core.to_term dynamics
;;

let parse_store_value : store_value -> store_value -> string -> parsed =
 fun _abstract_syntax_val concrete_syntax_val str ->
  let lex = Lexing.from_string str in
  Printf.printf "Parsing {|%s|}\n" str;
  match concrete_syntax_val with
  | GenesisTermConcrete -> ParsedTerm (Term.Parser.top_term Term.Lexer.read lex)
  | GenesisAbstractSyntaxConcrete ->
    ParsedAbstract (AbstractSyntax.Parser.language_def AbstractSyntax.Lexer.read lex)
  | GenesisStaticsConcrete -> ParsedStatics (Statics.Parser.rules Statics.Lexer.read lex)
  | GenesisDynamicsConcrete ->
    ParsedDynamics (Dynamics.Parser.dynamics Dynamics.Lexer.read lex)
  | Term concrete_syntax_tm ->
    (match NonBinding.from_nominal concrete_syntax_tm with
    | None -> failwith "TODO 1"
    | Some concrete_syntax_tm' ->
      let concrete_syntax = ConcreteSyntaxDescription.of_term concrete_syntax_tm' in
      let ast =
        match ConcreteSyntax.parse concrete_syntax "tm" (* XXX root name*) str with
        | Error _err -> Error (* (ParseError.to_string err) *) "parse error"
        | Ok tree -> ConcreteSyntax.to_ast concrete_syntax tree
      in
      (match ast with Ok ast -> ParsedTerm ast | Error msg -> failwith msg))
  | _ -> failwith "TODO 2"
;;

(* | Parseable { abstract_syntax = _; concrete_syntax } -> (match concrete_syntax with |
   None -> Term.Parser.top_term Term.Lexer.read lex | Some concrete_syntax' -> ) | _ ->
   failwith "TODO" *)

(* TODO: we end up double-wrapping the results of this call in eval_inline_block *)
let eval_command : store -> NonBinding.term -> string -> Vdom.Node.t =
 fun ({ term_store; name_store } as store) cmd body ->
  match cmd with
  | Operator
      ("define", [ maybe_ident; Operator ("language", [ abstract_tm; concrete_tm ]) ]) ->
    let lang_val = lookup_lang store abstract_tm in
    let concrete_val = lookup_maybe_concrete store concrete_tm in
    let parsed_defn = parse_store_value lang_val concrete_val body in
    let defn_tm = term_of_parsed parsed_defn in
    let key = Binding.Nominal.hash defn_tm in
    Hashtbl.set term_store ~key ~data:defn_tm;
    (match maybe_ident with
    | Operator ("just", [ Primitive (PrimString ident) ]) ->
      Printf.printf "setting name store %s -> %s\n" ident key;
      Hashtbl.set name_store ~key:ident ~data:key
    | _ ->
      Printf.printf "not setting name store (%s)\n" key;
      ());
    (* TODO: structured *)
    Vdom.Node.(pre [] [ code [] [ text @@ Binding.Nominal.pp_term' defn_tm ] ])
  | Operator ("eval", [ _ident ]) -> failwith "TODO eval"
  | _ -> failwith "TODO unknown command"
;;

let abstractSyntax =
  {|
import {list, string} from "builtin"

option(a) := some(a) | none()

pair(a; b) := pair(a; b)

emph_kind := normal() | strong()
emph_style := star() | underscore()
emph(inline) := emph(
  emph_kind();
  emph_style();
  inline
)

attributes := attributes(
  option(string()); // id
  list(string()); // classes
  list(pair(string(); string())) // attributes
)

heading(block) := heading(
  int(); // level
  block; // text
  attributes()
)

block_list_kind :=
  | ordered(int(); char())
  | unordered(char())

block_list_style := loose() | tight()

block_list(block) := block_list(
  block_list_kind();
  block_list_style();
  list(list(block))
)

inline :=
  | concat(list(inline()))
  | text(string())
  | emph(emph(inline()))
  | code(
    int(); // level
    string(); // content
    attributes()
  )
  | hard_break()
  | soft_break()
  // | link(
  // | ref
  // | html
  // | tag

block(a) :=
  | paragraph(a)
  | list(block_list(block(a)))
  // | blockquote(list(block(a)))
  | thematic_break()
  | heading(heading(a))
  | code_block(option(string); attributes())
  // | html_block
  // | link_def
  // | def_list
  // | tag_block

document := document(list(block(element())))
|}
;;

exception TranslationError of string

let term_of_option : f:('a -> NonBinding.term) -> 'a option -> NonBinding.term =
 fun ~f -> function None -> Operator ("none", []) | Some a -> Operator ("some", [ f a ])
;;

let term_of_string : string -> NonBinding.term = fun str -> Primitive (PrimString str)

let term_of_attributes : Omd.Attributes.t -> NonBinding.term =
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

let rec term_of_inline : Omd.inline -> NonBinding.term = function
  | Concat inlines -> Sequence (List.map inlines ~f:term_of_inline)
  | Text str -> Primitive (PrimString str)
  | _ -> raise (TranslationError "Unsupported inline type")
;;

let term_of_inline_block : Omd.inline Omd.block -> NonBinding.term = function
  | Paragraph inline -> term_of_inline inline
  | List _blocks -> raise (TranslationError "TODO: list blocks")
  | Blockquote _ -> raise (TranslationError "TODO: blockquote")
  | Heading { level; text; attributes } ->
    Operator
      ( "heading"
      , [ Primitive (PrimInteger (Bigint.of_int level))
        ; term_of_inline text
        ; term_of_attributes attributes
        ] )
  | Code_block { kind = _; label; other = _; code; attributes } ->
    Operator
      ( "code_block"
      , [ term_of_option label ~f:term_of_string
        ; term_of_option code ~f:term_of_string
        ; term_of_attributes attributes
        ] )
  | Thematic_break -> Operator ("thematic_break", [])
  | _ -> raise (TranslationError "Unsupported block type")
;;

let rec vdom_of_inline : Omd.inline -> Vdom.Node.t list =
  Vdom.Node.(
    function
    | Concat inlines -> inlines |> List.map ~f:vdom_of_inline |> List.concat
    | Text str -> [ text str ]
    | Emph { style = _; kind; content } ->
      [ (match kind with
        | Normal -> create "em" [] (vdom_of_inline content)
        | Strong -> strong [] (vdom_of_inline content))
      ]
    | Code { level = _; content; attributes = _ } -> [ code [] [ text content ] ]
    | Hard_break -> [ br [] ]
    | Soft_break | Link _ (* inline Link.t *) -> [ text "link not supported" ]
    | Ref _ (* inline Ref.t *) -> [ text "ref not supported" ]
    | Html _ (* string *) -> [ text "html not supported" ]
    | Tag _ (* inline Tag.t *) -> [ text "tag not supported" ])
;;

let error_block msg = Vdom.Node.(div [] [ text msg ])

let eval_inline_block : store -> Omd.inline Omd.block -> Vdom.Node.t =
 fun store ->
  Vdom.Node.(
    function
    | Paragraph inline -> p [] (vdom_of_inline inline)
    | List _blocks -> text "TODO eval_inline_block: list blocks"
    | Blockquote _ -> text "TODO eval_inline_block: blockquote"
    | Heading { level; text; attributes = _ } ->
      let node_creator =
        match level with 1 -> h1 | 2 -> h2 | 3 -> h3 | 4 -> h4 | 5 -> h5 | _ -> h6
        (* TODO: error *)
      in
      node_creator [ (* TODO: attributes *) ] (vdom_of_inline text)
    | Code_block _ (* { kind = _; label = _; other = _; code; attributes = _ } *) ->
      failwith "TODO"
      (* Printf.printf "label: '%s'\n" (match label with | None -> "none" | Some l -> l);
         Printf.printf "other: '%s'\n" (match other with | None -> "none" | Some l -> l);
         Printf.printf "code: '%s'\n" (match code with | None -> "none" | Some l -> l);

         Printf.printf "attributes:\n- id: %s\n- classes: [%s]\n- attributes: [%s]\n"
         (match attributes.id with | None -> "none" | Some str -> str) (attributes.classes
         |> String.concat ~sep:"; ") (attributes.attributes |> List.map ~f:(fun (x, y) ->
         Printf.sprintf {|"%s", "%s"|} x y) |> String.concat ~sep:"; ");

         (match code with | None -> error_block "No code found in code block" | Some
         headered_code -> (match String.lsplit2 headered_code ~on:'\n' with | Some
         (cmd_str, code_str) -> (match parse_command cmd_str with | Ok cmd ->
         Printf.printf "code_str:\n%s\n" code_str; eval_command store cmd code_str | Error
         msg -> text msg) | None -> error_block (Printf.sprintf "Single line code block
         found -- must include a header: %s" headered_code))) *)
    | Thematic_break -> hr []
    | Html_block _ -> text "html blocks not supported"
    | Link_def _ (* string Link_def.t *) -> text "TODO: link defs"
    | Def_list _ (* 'a Def_list.t *) -> text "TODO: def lists"
    | Tag_block _ (* 'a block Tag_block.t *) -> text "TODO: tag blocks")
;;

let evaluate_and_produce_vdom : store -> string -> Vdom.Node.t =
 fun store str ->
  let md = Omd.of_string str in
  Vdom.Node.div [] (List.map md ~f:(eval_inline_block store))
;;

let parse : string -> (NonBinding.term, string) Result.t =
 fun str ->
  let md = Omd.of_string str in
  try Ok (Sequence (List.map md ~f:term_of_inline_block)) with
  | TranslationError msg -> Error msg
;;

let%test_module "markdown" =
  (module struct
    let print_parse str =
      print_string
        (match parse str with Ok tm -> NonBinding.to_string tm | Error msg -> msg)
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
    [heading(1; "hello, world"; attributes(none(); []; [])), "para", code_block(
                                                                     some("nolang");
                                                                     some("foo");
                                                                     attributes(
                                                                     none();
                                                                     [];
                                                                     []))] |}]
    ;;
  end)
;;
