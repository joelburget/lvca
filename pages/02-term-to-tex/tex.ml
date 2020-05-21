(* open Core_kernel *)
open Lvca
(* open ConcreteSyntax *)

let abstract_syntax_desc =
  {|
letter :=
  | alpha()
  | beta()
  | gamma()
  | delta()
  | epsilon()

delimiter :=
  | lparen()
  | rparen()
  | lbrack()
  | rbrack()
  | lbrace()
  | rbrace()
  | langle()
  | rangle()

atom :=
  | letter(letter)
  | delimiter(delimiter)
|}
;;

let abstract = Parsing.AbstractSyntax.parse abstract_syntax_desc
;;

(*
let concrete_syntax_desc =
  {|
BACKSLASH := "\\"

ALPHA := "alpha"
BETA := "beta"
GAMMA := "gamma"
DELTA := "delta"
EPSILON := "epsilon"

LPAREN := "lparen"
RPAREN := "rparen"
LBRACK := "lbrack"
RBRACK := "rbrack"
LBRACE := "lbrace"
RBRACE := "rbrace"
LANGLE := "langle"
RANGLE := "rangle"

letter :=
  | ALPHA { alpha() }
  | BETA { beta() }
  | GAMMA { gamma() }
  | DELTA { delta() }
  | EPSILON { epsilon() }

delimiter :=
  | LPAREN { lparen() }
  | RPAREN { rparen() }
  | LBRACK { lbrack() }
  | RBRACK { rbrack() }
  | LBRACE { lbrace() }
  | RBRACE { rbrace() }
  | LANGLE { langle() }
  | RANGLE { rangle() }

command :=
  | BACKSLASH letter { letter($2) }
  | BACKSLASH delimiter { delimiter($2) }
  |}
;;

let concrete =
  let pre_terminal_rules, sort_rules =
    match Parsing.ConcreteSyntax.parse concrete_syntax_desc with
    | Error err -> failwith (ParseError.to_string err)
    | Ok desc -> desc
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
;;

let render_abstract : NonBinding.term -> string
  = fun tm ->
    let tm' = NonBinding.to_nominal tm in
    to_string (of_ast (failwith "TODO") concrete (failwith "TODO") "command" 80 tm')

let%test_module "tex" = (module struct

  let parse_print str =
    match parse concrete "command" str with
    | Ok tree -> print_string (to_string tree)
    | Error _err -> failwith "TODO"
  ;;

  let%expect_test {|\alpha|} =
    parse_print {|\alpha|};
    [%expect{|\alpha|}]

  let%expect_test {|\lparen|} =
    parse_print {|\lparen|};
    [%expect{|\lparen|}]
end)

type letter =
  | Alpha
  | Beta
  | Gamma
  | Delta
  | Epsilon

(* TODO *)

let letter_to_string = function
  | Alpha -> {|\Alpha|}
  | Beta -> {|\Beta|}
  | Gamma -> {|\Gamma|}
  | Delta -> {|\Delta|}
  | Epsilon -> {|\Epsilon|}
;;

let letter_of_term : NonBinding.term -> (letter, string) Result.t = failwith "TODO"

type delimiter =
  | Lparen
  | Rparen
  | Lbrack
  | Rbrack
  | Lbrace
  | Rbrace
  | Langle
  | Rangle

let delimiter_to_string = function
  | Lparen -> {|\lparen|}
  | Rparen -> {|\rparen|}
  | Lbrack -> {|\lbrack|}
  | Rbrack -> {|\rbrack|}
  | Lbrace -> {|\lbrace|}
  | Rbrace -> {|\rbrace|}
  | Langle -> {|\langle|}
  | Rangle -> {|\rangle|}
;;

type atom =
  | Letter of letter
  | Delimiter of delimiter

let atom_to_string = function
  | Letter letter -> letter_to_string letter
  | Delimiter delimiter -> delimiter_to_string delimiter
;;

let atom_of_term = function
  | NonBinding.Operator ("letter", [ tm ]) ->
    let%map letter = letter_of_term tm in
    Letter letter
  | _ -> Error "Tex.atom_of_term: expected a sequence of atoms"
;;

type t = atom list

let to_string = String.concat ~sep:" "

let of_term : NonBinding.term -> (t, string) Result.t = function
  | Sequence atoms -> atoms |> List.map ~f:atom_of_term |> Result.all
  | _ -> Error "Tex.of_term: expected a sequence of atoms"
;;
*)
