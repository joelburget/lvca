open Core_kernel

type re_class_base =
  | Word (* \w / \W *)
  | Whitespace (* \s / \S *)
  | Digit (* \d / \D *)
  | Boundary (* \b / \B *)

(* TODO: other javascript classes *)
(* TODO: unicode categories *)

let class_to_re : re_class_base -> Re.t =
  Re.(
    function
    | Word -> wordc
    | Whitespace -> space
    | Digit -> digit
    | Boundary -> alt [start; stop; bow; eow; bol; eol; bow; eos])
;;

(** Accept the positive or negative version of a class *)
type re_class =
  | PosClass of re_class_base
  | NegClass of re_class_base

type set_member =
  | SingleCharacter of char
  | Range of char * char

type re_set = set_member list

(** A regular expression used for lexical analysis. *)
type regex =
  | ReChar of char (** Just a character, eg 'a' *)
  | ReClass of re_class
      (** A character class, eg `\w` or `\d`. Syntactically, these are all started by a
          backslash. We just use javascript character classes. *)
  (* Question: do we support octal escapes (\40)? The lex manual points out
   * this is non-portable. But don't we presuppose unicode? We accept unicode
   * categories, right? `\cc`, `\cf`, etc. *)
  | ReSet of re_set (** A character set, eg `[a-z]` or `[^abc]` *)
  | ReStar of regex (** Zero-or-more repetition, eg `(ab)*` *)
  | RePlus of regex (** One-or-more repetition, eg `(ab)+` *)
  | ReCount of regex * int (** A specific number of repetitions, eg `(ab){5}`. Must be greater than 0. *)
  | ReOption of regex (** Option, eg `(ab)?` *)
  | ReChoice of regex list (** Choice, eg `a|b` *)
  | ReAny (** Any character *)
  | ReConcat of regex list

type t = regex

let re_str str = ReConcat (str
  |> String.to_list
  |> List.map ~f:(fun c -> ReChar c)
)

let set_to_re : re_set -> Re.t =
 fun set_members ->
  Re.(
    set_members
    |> List.map ~f:(function SingleCharacter c -> char c | Range (c1, c2) -> rg c1 c2)
    |> alt)
;;

let rec to_re : regex -> Re.t =
  Re.(
    function
    | ReChar s -> char s
    | ReClass (PosClass cls) -> class_to_re cls
    | ReClass (NegClass cls) -> compl [class_to_re cls]
    | ReSet set -> set_to_re set
    | ReStar re -> rep (to_re re)
    | RePlus re -> rep1 (to_re re)
    | ReCount (re, n) -> repn (to_re re) n None
    | ReOption re -> opt (to_re re)
    | ReChoice res -> alt (List.map res ~f:to_re)
    | ReAny -> any
    | ReConcat res -> seq (List.map res ~f:to_re))
;;

let show_class = function
  | PosClass Word -> {|\w|}
  | PosClass Whitespace -> {|\s|}
  | PosClass Digit -> {|\d|}
  | PosClass Boundary -> {|\b|}
  | NegClass Word -> {|\W|}
  | NegClass Whitespace -> {|\S|}
  | NegClass Digit -> {|\D|}
  | NegClass Boundary -> {|\B|}
;;

let show_set : re_set -> string =
 fun re_set ->
  let set_elems =
    re_set
    |> List.map ~f:(function
           | SingleCharacter c -> Printf.sprintf "SingleCharacter '%c'" c
           | Range (r1, r2) -> Printf.sprintf "Range ('%c', '%c')" r1 r2)
    |> String.concat ~sep:"; "
  in
  Printf.sprintf "[%s]" set_elems
;;

(* TODO: add parens (not a big deal; just used for debugging) *)
let rec show : regex -> string = function
  | ReChar c -> Printf.sprintf "ReChar %c" c
  | ReClass cls -> "ReClass " ^ show_class cls
  | ReSet set -> "ReSet " ^ show_set set
  | ReStar re -> "ReStar " ^ show re
  | RePlus re -> "RePlus " ^ show re
  | ReCount (re, n) -> Printf.sprintf "ReCount (%s, %n)" (show re) n
  | ReOption re -> "ReOption " ^ show re
  | ReChoice res ->
    Printf.sprintf "ReChoice [%s]" (res |> List.map ~f:show |> String.concat ~sep:"; ")
  | ReAny -> "ReAny"
  | ReConcat res ->
    Printf.sprintf "ReConcat [%s]" (res |> List.map ~f:show |> String.concat ~sep:"; ")
;;

let rec accepts_empty : regex -> bool = function
  | ReClass cls -> Caml.(cls = PosClass Boundary || cls = NegClass Boundary)
  | ReCount (re, _) | RePlus re
  -> accepts_empty re
  | ReChoice res -> List.exists res ~f:accepts_empty
  | ReConcat pieces -> List.for_all pieces ~f:accepts_empty
  | ReStar _ | ReOption _ -> true
  | ReChar _ | ReSet _ | ReAny -> false
;;

let rec is_literal : regex -> string option
  = function
  | ReChar c -> Some (String.of_char c)
  (* surely there's a smarter way to do this: *)
  | ReConcat res -> List.fold_right res
     ~f:(fun re -> function
      | None -> None
      | Some str -> (match is_literal re with
        | None -> None
        | Some str' -> Some (str' ^ str)))
     ~init:(Some "")
  | _ -> None

let class_char : re_class -> char = function
  | PosClass Word -> 'w'
  | PosClass Whitespace -> 's'
  | PosClass Digit -> 'd'
  | PosClass Boundary -> 'b'
  | NegClass Word -> 'W'
  | NegClass Whitespace -> 'S'
  | NegClass Digit -> 'D'
  | NegClass Boundary -> 'B'
;;

let class_to_string : re_class -> string =
 fun cls -> Printf.sprintf {|\%c|} (class_char cls)
;;

(* precedence:
 * 2: * + ?
 * 1: concat
 * 0: |
 *)

let parenthesize : bool -> string -> string =
 fun condition str -> if condition then "(" ^ str ^ ")" else str
;;

let set_to_string : re_set -> string =
 fun re_set ->
  re_set
  |> List.map ~f:(function
         | SingleCharacter c -> String.of_char c
         | Range (r1, r2) -> Printf.sprintf "%c-%c" r1 r2)
  |> String.concat
;;

let rec to_string' : int -> regex -> string =
 fun precedence -> function
  (* We need to escape special characters in strings *)
  | ReChar c -> (match c with
    | '[' -> {|\[|}
    | ']' -> {|\]|}
    | '(' -> {|\(|}
    | ')' -> {|\)|}
    | '\\' -> {|\\|}
    | '.' -> {|\.|}
    | '|' -> {|\||}
    | '*' -> {|\*|}
    | '+' -> {|\+|}
    | '?' -> {|\?|}

    (* XXX no match in parser *)
    | '-' -> {|\-|}
    | '/' -> {|\/|}

    | _ -> String.of_char c
  )
  | ReSet set -> "[" ^ set_to_string set ^ "]"
  | ReStar re -> to_string' 2 re ^ "*"
  | RePlus re -> to_string' 2 re ^ "+"
  | ReCount (re, n) -> Printf.sprintf "%s{%n}" (to_string' 2 re) n
  | ReOption re -> to_string' 2 re ^ "?"
  | ReClass cls -> class_to_string cls
  | ReChoice res ->
    res
    |> List.map ~f:(to_string' 0)
    |> String.concat ~sep:"|"
    |> parenthesize (precedence > 0)
  | ReAny -> "."
  | ReConcat pieces ->
    pieces |> List.map ~f:(to_string' 2) |> String.concat |> parenthesize (precedence > 1)
;;

(** Convert a regex to a string which is parseable back to a regex. IE, for valid regexes,

    - to_string . parse = id
    - parse . to_string = id

    This has no delimiters, ie it returns "abc", not "/abc/". *)
let to_string : regex -> string = to_string' 0

(* TODO: is this okay? *)
let to_term : t -> NonBinding.term
  = fun re -> Operator ("regex", [Primitive (PrimString (to_string re))])

module Classes = struct
  let az = Range ('a', 'z')
  let az_cap = Range ('A', 'Z')
  let o_nine = Range ('0', '9')
  let lower_alpha = ReSet [ az ]
  let alpha = ReSet [ az; az_cap ]
  let words = ReSet [ az; az_cap; o_nine ]
  let underscore_words = ReSet [ az; az_cap; o_nine; SingleCharacter '_' ]
end

let%test_module "regex tests" =
  (module struct
    let ( = ) = Caml.( = )

    let%test_module "accepts_empty" =
      (module struct
        let%test _ = accepts_empty (re_str "foo") = false

        let%test _ =
          accepts_empty (ReConcat [ ReStar (re_str "foo"); ReOption (re_str "bar") ])
          = true
        ;;

        let%test _ =
          accepts_empty (ReConcat [ ReStar (re_str "foo"); RePlus (re_str "bar") ])
          = false
        ;;

        let%test _ = accepts_empty (ReClass (PosClass Boundary))
        let%test _ = accepts_empty (ReClass (NegClass Boundary))
        let%test _ = not (accepts_empty (ReClass (PosClass Digit)))
        let%test _ = not (accepts_empty (ReClass (NegClass Digit)))
        let%test _ = not (accepts_empty (ReSet [ Range ('a', 'z') ]))
        let%test _ = accepts_empty (RePlus (re_str ""))
        let%test _ = accepts_empty (ReCount (re_str "", 5))
        let%test _ = not (accepts_empty (ReCount (ReChar 'a', 5)))
      end)
    ;;

    let%test_module "to_string" =
      (module struct
        let print_re re = printf "%s" (to_string re)

        let%expect_test _ =
          print_re (ReConcat [ re_str "foo"; re_str "bar" ]);
          [%expect {| (foo)(bar) |}]
        ;;

        let%expect_test _ =
          print_re (ReConcat [ re_str "foo"; re_str "bar" ]);
          [%expect {|(foo)(bar)|}]
        ;;

        let%expect_test _ =
          print_re (ReSet [ Range ('a', 'z') ]);
          [%expect {|[a-z]|}]
        ;;

        let%expect_test _ =
          print_re (ReConcat [ ReClass (PosClass Boundary); ReClass (NegClass Boundary) ]);
          [%expect {|\b\B|}]
        ;;

        let%expect_test _ =
          print_re
            (ReConcat
               [ ReStar (re_str "foo")
               ; RePlus (re_str "foo")
               ; ReOption (re_str "foo")
               ; ReCount (re_str "foo", 5)
               ]);
          [%expect {| (foo)*(foo)+(foo)?(foo){5} |}]
        ;;

        let%expect_test _ =
          print_re (ReChar '+');
          [%expect {| \+ |}]
        ;;

        let%expect_test _ =
          print_re (ReChar '*');
          [%expect {| \* |}]
        ;;

        let%expect_test _ =
          print_re (ReChar '?');
          [%expect {| \? |}]
        ;;

        let%expect_test _ =
          print_re (ReChar '-');
          [%expect {| \- |}]
        ;;
      end)
    ;;

    let%test_module "matching" =
     (module struct
      let print_matches regex str =

        let re = Re.compile (to_re regex) in
        let group = Re.exec re str in
        Re.Group.pp Format.std_formatter group

      let%expect_test _ =
        print_matches (ReConcat
          [ RePlus (ReClass (PosClass Word))
          ; ReClass (PosClass Boundary)
          ; RePlus (ReClass (PosClass Whitespace))
          ; ReClass (PosClass Boundary)
          ; RePlus (ReClass (PosClass Word))
          ])
        "hello world";
        [%expect{| (Group (hello world (0 11))) |}]

      let%expect_test _ =
        print_matches (ReClass (NegClass Word))
        "hello world";
        [%expect{| (Group (  (5 6))) |}]

      let%expect_test _ =
        print_matches (RePlus (ReClass (NegClass Whitespace)))
        "hello world";
        [%expect{| (Group (hello (0 5))) |}]

        (* TODO
      let%expect_test _ =
        print_matches (ReClass (NegClass Boundary))
        "hello world";
        [%expect{| (Group (h (0 1))) |}]
        *)

      end)
    ;;
  end)
;;
