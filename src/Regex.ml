type re_class_base =
  | Word (* \w / \W *)
  | Whitespace (* \s / \S *)
  | Digit (* \d / \D *)
  | Boundary (* \b / \B *)
(* TODO: other javascript classes *)
(* TODO: unicode categories *)

(** Accept the positive or negative version of a class *)
type re_class = PosClass of re_class_base | NegClass of re_class_base

(** A regular expression used for lexical analysis. *)
type regex =
  (** Just a string of characters, eg `foo` *)
  | ReString of string
  (** A character class, eg `\w` or `\d`. Syntactically, these are all
      started by a backslash. We just use javascript character classes.
  *)
  (* Question: do we support octal escapes (\40)? The lex manual points out
   * this is non-portable. But don't we presuppose unicode? We accept unicode
   * categories, right? `\cc`, `\cf`, etc. *)
  | ReClass  of re_class
  (** A character set, eg `[a-z]` or `[^abc]` *)
  | ReSet    of string
  (** Zero-or-more repetition, eg `(ab)*` *)
  | ReStar   of regex
  (** One-or-more repetition, eg `(ab)+` *)
  | RePlus   of regex
  (** Option, eg `(ab)?` *)
  | ReOption of regex
  (** Choice, eg `a|b` *)
  | ReChoice of regex * regex
  (** Any character *)
  | ReAny
  | ReConcat of regex list

type t = regex

let show_class = function
  | PosClass Word -> {|\w|}
  | PosClass Whitespace -> {|\s|}
  | PosClass Digit -> {|\d|}
  | PosClass Boundary -> {|\b|}
  | NegClass Word -> {|\W|}
  | NegClass Whitespace -> {|\S|}
  | NegClass Digit -> {|\D|}
  | NegClass Boundary -> {|\B|}

(* TODO: add parens (not a big deal; just used for debugging) *)
let rec show : regex -> string
  = function
    | ReString str -> "ReString " ^ str
    | ReClass cls -> "ReClass " ^ show_class cls
    | ReSet str -> "ReSet " ^ str
    | ReStar re -> "ReStar " ^ show re
    | RePlus re -> "RePlus " ^ show re
    | ReOption re -> "ReOption " ^ show re
    | ReChoice (re, re')
      -> Printf.sprintf "ReChoice (%s, %s)" (show re) (show re')
    | ReAny -> "ReAny"
    | ReConcat res -> Printf.sprintf "ReConcat [%s]" (res
                                                      |. Belt.List.map show
                                                      |> String.concat "; "
                                                     )

let rec accepts_empty : regex -> bool
  = function
    | ReString str -> Js.String2.length str = 0
    | ReClass cls -> cls = PosClass Boundary || cls = NegClass Boundary
    | RePlus re -> accepts_empty re
    | ReChoice (a, b) -> accepts_empty a || accepts_empty b
    | ReConcat pieces -> Belt.List.every pieces accepts_empty
    | ReStar _
    | ReOption _ -> true
    | ReSet _
    | ReAny -> false

let is_literal : regex -> string option = function
  | ReString str -> Some str
  | _            -> None

let class_char : re_class -> char
  = function
    | PosClass Word -> 'w'
    | PosClass Whitespace -> 's'
    | PosClass Digit -> 'd'
    | PosClass Boundary -> 'b'
    | NegClass Word -> 'W'
    | NegClass Whitespace -> 'S'
    | NegClass Digit -> 'D'
    | NegClass Boundary -> 'B'

let class_to_string : re_class -> string
  = fun cls -> Printf.sprintf "\\%c" (class_char cls)

(* precedence:
 * 2: * + ?
 * 1: concat
 * 0: |
*)

let parenthesize : bool -> string -> string
  = fun condition str ->
    if condition then "(" ^ str ^ ")" else str

let rec to_string' : int -> regex -> string
  = fun precedence re -> match re with
    (* We need to escape special characters in strings *)
    | ReString str -> Js.String.(str
                                 |> replaceByRe [%re {|/\\/g|}] {|\\|}
                                 |> replaceByRe [%re {|/\//g|}] {|\/|}
                                 |> replaceByRe [%re {|/\+/g|}] {|\+|}
                                 |> replaceByRe [%re {|/\*/g|}] {|\*|}
                                 |> replaceByRe [%re {|/\?/g|}] {|\?|}
                                 |> replaceByRe [%re {|/\-/g|}] {|\-|}
                                 |> replaceByRe [%re {|/\(/g|}] {|\(|}
                                 |> replaceByRe [%re {|/\)/g|}] {|\)|}
                                 |> parenthesize (precedence > 1 && length str > 1)
                                )

    | ReSet    str -> "[" ^ str ^ "]"
    | ReStar   re -> to_string' 2 re ^ "*"
    | RePlus   re -> to_string' 2 re ^ "+"
    | ReOption re -> to_string' 2 re ^ "?"
    | ReClass  cls -> class_to_string cls
    | ReChoice (re, re') -> parenthesize
                              (precedence > 0)
                              (to_string' 0 re ^ "|" ^ to_string' 0 re')
    | ReAny -> "."
    | ReConcat pieces -> pieces
                         |> List.map (to_string' 2)
                         |> String.concat ""
                         |> parenthesize (precedence > 1)

(** Convert a regex to a string which is parseable back to a regex. IE, for
    valid regexes,
    - to_string . parse = id
    - parse . to_string = id

    This has no delimiters, ie it returns "abc", not "/abc/".
*)
let to_string : regex -> string
  = to_string' 0
