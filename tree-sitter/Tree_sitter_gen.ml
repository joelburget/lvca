open Cmdliner
open Lvca_syntax
open Tree_sitter

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let mk_concrete filename =
  Lvca_parsing.parse_string_or_failwith Concrete.parse (read_whole_file filename)
;;

let mk_abstract filename =
  let str = read_whole_file filename in
  match
    Lvca_parsing.parse_string_or_failwith Abstract_syntax.parse str
    |> Abstract_syntax.mk_unordered
  with
  | `Duplicate_key k -> failwith (Fmt.str "duplicate key %s" k)
  | `Ok Abstract_syntax.Unordered.{ sort_defs; _ } -> sort_defs
;;

let gen abstract_str concrete_str name =
  let concrete = mk_concrete concrete_str in
  let abstract = mk_abstract abstract_str in
  let grammar = Grammar.of_language ~name ~abstract ~concrete in
  Fmt.pr "%a@;" Grammar.pp grammar
;;

let abstract =
  let doc = "Abstract syntax definition." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ABSTRACT" ~doc)
;;

let concrete =
  let doc = "Concrete syntax definition." in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"CONCRETE" ~doc)
;;

let name' : string Term.t =
  let doc = "Language name." in
  Arg.(value & opt string "unnamed" & info [ "n"; "name" ] ~docv:"NAME" ~doc)
;;

let tree_sitter_t = Term.(const gen $ abstract $ concrete $ name')

let info =
  let doc = "Generate a Tree-sitter grammar from an LVCA concrete syntax" in
  let man = [ `S Manpage.s_bugs; `P "https://github.com/joelburget/lvca/issues" ] in
  Term.info "gen" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man
;;

let () = Term.exit @@ Term.eval (tree_sitter_t, info)
