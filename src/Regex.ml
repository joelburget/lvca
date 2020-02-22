open Core_kernel

type re_class_base =
  | Word (* \w / \W *)
  | Whitespace (* \s / \S *)
  | Digit (* \d / \D *)
  | Boundary (* \b / \B *)
(* TODO: other javascript classes *)
(* TODO: unicode categories *)

let class_to_re : re_class_base -> Re.t
  = Re.(function
  | Word -> wordc
  | Whitespace -> space
  | Digit -> digit
  | Boundary -> failwith "TODO: boundary"
  )

(** Accept the positive or negative version of a class *)
type re_class = PosClass of re_class_base | NegClass of re_class_base

(** A regular expression used for lexical analysis. *)
type regex =
  | ReString of string
  (** Just a string of characters, eg `foo` *)
  | ReClass  of re_class
  (** A character class, eg `\w` or `\d`. Syntactically, these are all
      started by a backslash. We just use javascript character classes.
  *)
  (* Question: do we support octal escapes (\40)? The lex manual points out
   * this is non-portable. But don't we presuppose unicode? We accept unicode
   * categories, right? `\cc`, `\cf`, etc. *)
  | ReSet    of string
  (** A character set, eg `[a-z]` or `[^abc]` *)
  | ReStar   of regex
  (** Zero-or-more repetition, eg `(ab)*` *)
  | RePlus   of regex
  (** One-or-more repetition, eg `(ab)+` *)
  | ReOption of regex
  (** Option, eg `(ab)?` *)
  | ReChoice of regex * regex
  (** Choice, eg `a|b` *)
  | ReAny
  (** Any character *)
  | ReConcat of regex list

type t = regex

let rec to_re : regex -> Re.t
  = Re.(function
  | ReString s -> str s
  | ReClass (PosClass cls) -> class_to_re cls
  | ReClass (NegClass _cls) -> failwith "TODO: class negation"
  | ReSet str -> set str
  | ReStar re -> rep (to_re re)
  | RePlus re -> rep1 (to_re re)
  | ReOption re -> opt (to_re re)
  | ReChoice (re1, re2) -> alt [to_re re1; to_re re2]
  | ReAny -> any
  | ReConcat res -> seq (List.map res ~f:to_re)
  )

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
                                                      |> List.map ~f:show
                                                      |> Caml.String.concat "; "
                                                     )

let rec accepts_empty : regex -> bool
  = function
    | ReString str -> String.length str = 0
    | ReClass cls -> Caml.(cls = PosClass Boundary || cls = NegClass Boundary)
    | RePlus re -> accepts_empty re
    | ReChoice (a, b) -> accepts_empty a || accepts_empty b
    | ReConcat pieces -> List.for_all pieces ~f:accepts_empty
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
  = fun precedence -> function
    (* We need to escape special characters in strings *)
    | ReString str -> str
      (*
      Re.(str
      |> replace ~re:(Re.of_string {|/\\/g|}) ~replacement:{|\\|}
      |> replace ~re:(Re.of_string {|/\//g|}) ~replacement:{|\/|}
      |> replace ~re:(Re.of_string {|/\|/g|}) ~replacement:{|\||}
      |> replace ~re:(Re.of_string {|/\+/g|}) ~replacement:{|\+|}
      |> replace ~re:(Re.of_string {|/\*/g|}) ~replacement:{|\*|}
      |> replace ~re:(Re.of_string {|/\?/g|}) ~replacement:{|\?|}
      |> replace ~re:(Re.of_string {|/\-/g|}) ~replacement:{|\-|}
      |> replace ~re:(Re.of_string {|/\(/g|}) ~replacement:{|\(|}
      |> replace ~re:(Re.of_string {|/\)/g|}) ~replacement:{|\)|}
      |> parenthesize (precedence > 1 && String.length str > 1)
      )
      *)

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
                         |> List.map ~f:(to_string' 2)
                         |> Caml.String.concat ""
                         |> parenthesize (precedence > 1)

(** Convert a regex to a string which is parseable back to a regex. IE, for
    valid regexes,
    - to_string . parse = id
    - parse . to_string = id

    This has no delimiters, ie it returns "abc", not "/abc/".
*)
let to_string : regex -> string
  = to_string' 0


let%test_module "regex tests" = (module struct
  let (=) = Caml.(=)
  let%test_module "accepts_empty" = (module struct
    let%test "" = accepts_empty (ReString "foo") = false
    let%test "" =
     (accepts_empty (ReConcat
          [ReStar (ReString "foo"); ReOption (ReString "bar")]
        ))
     = true
    let%test "" =
        (accepts_empty (ReConcat
          [ReStar (ReString "foo"); RePlus (ReString "bar")]
        ))
      = false

    let%test "" = accepts_empty (ReClass (PosClass Boundary))
    let%test "" = accepts_empty (ReClass (NegClass Boundary))
    let%test "" = not (accepts_empty (ReClass (PosClass Digit)))
    let%test "" = not (accepts_empty (ReClass (NegClass Digit)))
    let%test "" = not (accepts_empty (ReSet "a-z"))
    let%test "" = accepts_empty (RePlus (ReString ""))
  end)

  let%test_module "to_string" = (module struct
    let print_re re = printf "%s" (to_string re)
    let%expect_test _ =
      print_re (ReConcat [ReString "foo"; ReString "bar"]);
      [%expect]
    let%expect_test _ =
      print_re (ReConcat [ReString "foo"; ReString "bar"]);
      [%expect{|(foo)(bar)|}]
    let%expect_test _ = print_re (ReSet "a-z");
      [%expect{|[a-z]|}]
    let%expect_test _ = print_re
        (ReConcat [ReClass (PosClass Boundary); ReClass (NegClass Boundary)]);
      [%expect{|\b\B|}]
    let%expect_test _ =
      printf "%s" (to_string (ReConcat
        [ ReStar (ReString "foo");
          RePlus (ReString "foo");
          ReOption (ReString "foo");
        ]));
      [%expect]
      (* = "(foo)*(foo)+(foo)?" *)
    let%expect_test _ = printf "%s" (to_string (ReString "+")); [%expect] (* = {|\+|} *)
    let%expect_test _ = printf "%s" (to_string (ReString "*")); [%expect] (* = {|\*|} *)
    let%expect_test _ = printf "%s" (to_string (ReString "?")); [%expect] (* = {|\?|} *)
    let%expect_test _ = printf "%s" (to_string (ReString "-")); [%expect] (* = {|\-|} *)
  end)
end)
