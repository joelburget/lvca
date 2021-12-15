open Base

(* TODO: safe_char is too restrictive *)
let safe_char = Angstrom.satisfy Char.(fun c -> is_alphanum c || c = '_')

module Class_base = struct
  type t =
    | Word (* \w / \W *)
    | Whitespace (* \s / \S *)
    | Digit (* \d / \D *)
    | Boundary
  (* \b / \B *)

  let ( = ) a b =
    match a, b with
    | Word, Word | Whitespace, Whitespace | Digit, Digit | Boundary, Boundary -> true
    | _ -> false
  ;;

  (* TODO: other javascript classes *)
  (* TODO: unicode categories *)

  let to_re =
    let open Re in
    function
    | Word -> wordc
    | Whitespace -> space
    | Digit -> digit
    | Boundary -> alt [ start; stop; bow; eow; bol; eol; bow; eos ]
  ;;
end

module Class = struct
  type t =
    | Pos of Class_base.t
    | Neg of Class_base.t

  let ( = ) a b =
    match a, b with Pos a, Pos b | Neg a, Neg b -> Class_base.(a = b) | _ -> false
  ;;

  let to_char = function
    | Pos Word -> 'w'
    | Pos Whitespace -> 's'
    | Pos Digit -> 'd'
    | Pos Boundary -> 'b'
    | Neg Word -> 'W'
    | Neg Whitespace -> 'S'
    | Neg Digit -> 'D'
    | Neg Boundary -> 'B'
  ;;

  let pp ppf cls = Fmt.pf ppf {|\%c|} (to_char cls)

  let parse =
    let open Angstrom in
    let p =
      char '\\'
      >>= fun _ ->
      choice
        [ char 'w' *> return (Pos Word)
        ; char 'W' *> return (Neg Word)
        ; char 'd' *> return (Pos Digit)
        ; char 'D' *> return (Neg Digit)
        ; char 's' *> return (Pos Whitespace)
        ; char 'S' *> return (Neg Whitespace)
        ; char 'b' *> return (Pos Boundary)
        ; char 'B' *> return (Neg Boundary)
        ]
    in
    p <?> "class"
  ;;
end

module Set_member = struct
  type t =
    | Single_char of char
    | Range of char * char

  let ( = ) a b =
    match a, b with
    | Single_char a, Single_char b -> Char.(a = b)
    | Range (a1, a2), Range (b1, b2) -> Char.(a1 = b1 && a2 = b2)
    | _ -> false
  ;;

  let debug_pp ppf = function
    | Single_char c -> Fmt.pf ppf "Single_char '%c'" c
    | Range (r1, r2) -> Fmt.pf ppf "Range ('%c', '%c')" r1 r2
  ;;

  let pp ppf = function
    | Single_char c -> Fmt.char ppf c
    | Range (r1, r2) -> Fmt.pf ppf "%c-%c" r1 r2
  ;;

  let parse =
    let open Angstrom in
    let p =
      safe_char
      >>= fun c1 ->
      choice
        [ (char '-' *> safe_char >>| fun c2 -> Range (c1, c2)); return (Single_char c1) ]
    in
    p <?> "set member"
  ;;
end

module Set = struct
  type t = Set_member.t list

  let ( = ) a b = List.equal Set_member.( = ) a b
  let debug_pp ppf = Fmt.(pf ppf "[%a]" (list Set_member.pp ~sep:semi))
  let pp = Fmt.(list Set_member.pp ~sep:nop)

  let parse =
    let open Angstrom in
    char '[' *> many Set_member.parse <* char ']' <?> "set"
  ;;
end

(* Question: do we support octal escapes (\40)? The lex manual points out
 * this is non-portable. But don't we presuppose unicode? We accept unicode
 * categories, right? [\cc], [\cf], etc. *)
type t =
  | Char of char
  | Class of Class.t
  | Set of Set.t
  | Star of t
  | Plus of t
  | Count of t * int
  | Option of t
  | Choice of t list
  | Any
  | Concat of t list

let rec ( = ) a b =
  match a, b with
  | Char a, Char b -> Char.(a = b)
  | Class a, Class b -> Class.(a = b)
  | Set a, Set b -> Set.(a = b)
  | Star a, Star b | Plus a, Plus b | Option a, Option b -> a = b
  | Count (a1, a2), Count (b1, b2) -> a1 = b1 && Int.(a2 = b2)
  | Choice a, Choice b | Concat a, Concat b -> List.equal ( = ) a b
  | Any, Any -> true
  | _ -> false
;;

let parse =
  let open Angstrom in
  let int_literal = take_while1 Char.is_digit >>| Int.of_string in
  let prec3 =
    choice
      [ (Set.parse >>| fun set -> Set set)
      ; (Class.parse >>| fun cls -> Class cls)
      ; (char '*' >>| fun _ -> Any)
      ; (safe_char >>| fun c -> Char c)
      ]
    <?> "regex (precedence 3)"
  in
  let prec2 =
    prec3
    >>= fun atom ->
    option
      atom
      (choice
         [ (char '*' >>| fun _ -> Star atom)
         ; (char '+' >>| fun _ -> Plus atom)
         ; (char '?' >>| fun _ -> Option atom)
         ; (char '{' *> int_literal <* char '}' >>| fun i -> Count (atom, i))
           (* TODO: handle too-large literals *)
         ])
  in
  let prec2 = prec2 <?> "regex (precedence 2)" in
  let prec1 =
    many1 prec2
    >>| function
    | [] -> failwith "invalid count (result of many1)" | [ t ] -> t | ts -> Concat ts
  in
  let prec1 = prec1 <?> "regex (precedence 1)" in
  let prec0 =
    sep_by1 (char '|') prec1
    >>| function
    | [] -> failwith "invalid count (result of many1)" | [ t ] -> t | ts -> Choice ts
  in
  prec0 <?> "regex"
;;

(* Create a character set from a string *)
let re_str str = Concat (str |> String.to_list |> List.map ~f:(fun c -> Char c))

let of_set set_members =
  let open Re in
  set_members
  |> List.map ~f:(function
         | Set_member.Single_char c -> char c
         | Range (c1, c2) -> rg c1 c2)
  |> alt
;;

let rec to_re =
  let open Re in
  function
  | Char s -> char s
  | Class (Pos cls) -> Class_base.to_re cls
  | Class (Neg cls) -> compl [ Class_base.to_re cls ]
  | Set set -> of_set set
  | Star re -> rep (to_re re)
  | Plus re -> rep1 (to_re re)
  | Count (re, n) -> repn (to_re re) n None
  | Option re -> opt (to_re re)
  | Choice res -> alt (List.map res ~f:to_re)
  | Any -> any
  | Concat res -> seq (List.map res ~f:to_re)
;;

let rec debug_pp ?(need_parens = false) ppf t =
  let open Fmt in
  let go ppf = function
    | Char c -> pf ppf "ReChar %c" c
    | Class cls -> pf ppf "ReClass %a" Class.pp cls
    | Set set -> pf ppf "ReSet %a" Set.pp set
    | Star re -> pf ppf "ReStar %a" (debug_pp ~need_parens:true) re
    | Plus re -> pf ppf "RePlus %a" (debug_pp ~need_parens:true) re
    | Count (re, n) -> pf ppf "ReCount (%a, %n)" (debug_pp ~need_parens:false) re n
    | Option re -> pf ppf "ReOption %a" (debug_pp ~need_parens:true) re
    | Choice res -> pf ppf "ReChoice [%a]" (list debug_pp ~sep:semi) res
    | Any -> pf ppf "ReAny"
    | Concat res -> pf ppf "ReConcat [%a]" (list debug_pp ~sep:semi) res
  in
  if need_parens then Fmt.pf ppf "(%a)" go t else go ppf t
;;

let rec accepts_empty = function
  | Class cls -> Class.(cls = Pos Boundary || cls = Neg Boundary)
  | Count (re, _) | Plus re -> accepts_empty re
  | Choice res -> List.exists res ~f:accepts_empty
  | Concat pieces -> List.for_all pieces ~f:accepts_empty
  | Star _ | Option _ -> true
  | Char _ | Set _ | Any -> false
;;

let rec is_literal = function
  | Char c -> Some (String.of_char c)
  (* surely there's a smarter way to do this: *)
  | Concat res ->
    List.fold_right
      res
      ~f:(fun re ->
        Option.bind ~f:(fun lit ->
            match is_literal re with None -> None | Some lit' -> Some (lit' ^ lit)))
      ~init:(Some "")
  | _ -> None
;;

(* precedence:
 * 2: * + ? {n}
 * 1: concat
 * 0: |
 *)

let rec pp' precedence ppf =
  let open Fmt in
  function
  (* We need to escape special characters in strings *)
  | Char c ->
    let str =
      match c with
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
    in
    string ppf str
  | Set set -> pf ppf "[%a]" Set.pp set
  | Star re -> pf ppf "%a*" (pp' 2) re
  | Plus re -> pf ppf "%a+" (pp' 2) re
  | Count (re, n) -> pf ppf "%a{%n}" (pp' 2) re n
  | Option re -> pf ppf "%a?" (pp' 2) re
  | Class cls -> Class.pp ppf cls
  | Choice res ->
    let pp = list (pp' 0) ~sep:(any "|") in
    if precedence > 0 then pf ppf "@[(%a)@]" pp res else pp ppf res
  | Any -> pf ppf "."
  | Concat pieces ->
    let pp = list (pp' 2) ~sep:nop in
    if precedence > 1 then pf ppf "@[(%a)@]" pp pieces else pp ppf pieces
;;

(** Convert a regex to a string which is parseable back to a regex. IE, for valid regexes,

    - pp . parse = id
    - parse . pp = id

    This has no delimiters, ie it returns "abc", not "/abc/". *)
let pp = pp' 0

(* TODO: is this okay? *)
let to_nonbinding re =
  let info = Provenance.of_here [%here] in
  Nonbinding.Operator
    (info, "Regex", [ Primitive (info, String (Fmt.to_to_string pp re)) ])
;;

module Classes = struct
  let az, az_cap, o_nine =
    Set_member.(Range ('a', 'z'), Range ('A', 'Z'), Range ('0', '9'))
  ;;

  let lower_alpha = Set [ az ]
  let alpha = Set [ az; az_cap ]
  let words = Set [ az; az_cap; o_nine ]
  let underscore_words = Set [ az; az_cap; o_nine; Single_char '_' ]
end

let%test_module "accepts_empty" =
  (module struct
    let ( = ) = Bool.( = )

    let%test _ = accepts_empty (re_str "foo") = false

    let%test _ =
      accepts_empty (Concat [ Star (re_str "foo"); Option (re_str "bar") ]) = true
    ;;

    let%test _ =
      accepts_empty (Concat [ Star (re_str "foo"); Plus (re_str "bar") ]) = false
    ;;

    let%test _ = accepts_empty (Class (Pos Boundary))
    let%test _ = accepts_empty (Class (Neg Boundary))
    let%test _ = not (accepts_empty (Class (Pos Digit)))
    let%test _ = not (accepts_empty (Class (Neg Digit)))
    let%test _ = not (accepts_empty (Set [ Range ('a', 'z') ]))
    let%test _ = accepts_empty (Plus (re_str ""))
    let%test _ = accepts_empty (Count (re_str "", 5))
    let%test _ = not (accepts_empty (Count (Char 'a', 5)))
  end)
;;

let%test_module "pp" =
  (module struct
    let print = pp Fmt.stdout

    let%expect_test _ =
      print (Concat [ re_str "foo"; re_str "bar" ]);
      [%expect {| (foo)(bar) |}]
    ;;

    let%expect_test _ =
      print (Concat [ re_str "foo"; re_str "bar" ]);
      [%expect {|(foo)(bar)|}]
    ;;

    let%expect_test _ =
      print (Set [ Range ('a', 'z') ]);
      [%expect {|[a-z]|}]
    ;;

    let%expect_test _ =
      print (Concat [ Class (Pos Boundary); Class (Neg Boundary) ]);
      [%expect {|\b\B|}]
    ;;

    let%expect_test _ =
      print
        (Concat
           [ Star (re_str "foo")
           ; Plus (re_str "foo")
           ; Option (re_str "foo")
           ; Count (re_str "foo", 5)
           ]);
      [%expect {| (foo)*(foo)+(foo)?(foo){5} |}]
    ;;

    let%expect_test _ =
      print (Char '+');
      [%expect {| \+ |}]
    ;;

    let%expect_test _ =
      print (Char '*');
      [%expect {| \* |}]
    ;;

    let%expect_test _ =
      print (Char '?');
      [%expect {| \? |}]
    ;;

    let%expect_test _ =
      print (Char '-');
      [%expect {| \- |}]
    ;;
  end)
;;

let%test_module "matching" =
  (module struct
    let print_matches regex str =
      let re = Re.compile (to_re regex) in
      let group = Re.exec re str in
      Re.Group.pp Fmt.stdout group
    ;;

    let%expect_test _ =
      print_matches
        (Concat
           [ Plus (Class (Pos Word))
           ; Class (Pos Boundary)
           ; Plus (Class (Pos Whitespace))
           ; Class (Pos Boundary)
           ; Plus (Class (Pos Word))
           ])
        "hello world";
      [%expect {| (Group (hello world (0 11))) |}]
    ;;

    let%expect_test _ =
      print_matches (Class (Neg Word)) "hello world";
      [%expect {| (Group (  (5 6))) |}]
    ;;

    let%expect_test _ =
      print_matches (Plus (Class (Neg Whitespace))) "hello world";
      [%expect {| (Group (hello (0 5))) |}]
    ;;

    (* TODO
        let%expect_test _ =
          print_matches (Class (Neg Boundary)) "hello\n           world";
          [%expect {| (Group (h (0 1))) |}]
        ;;
        *)
  end)
;;

let%test_module "parsing" =
  (module struct
    let parse_exn p str =
      Angstrom.parse_string ~consume:All p str |> Result.ok_or_failwith
    ;;

    let%test_module "Class parsing" =
      (module struct
        open Class

        let parse = parse_exn parse

        let%test _ = parse {|\w|} = Pos Word
        let%test _ = parse {|\W|} = Neg Word
        let%test _ = parse {|\s|} = Pos Whitespace
        let%test _ = parse {|\S|} = Neg Whitespace
        let%test _ = parse {|\d|} = Pos Digit
        let%test _ = parse {|\D|} = Neg Digit
        let%test _ = parse {|\b|} = Pos Boundary
        let%test _ = parse {|\B|} = Neg Boundary
      end)
    ;;

    let%test_module "Set_member parsing" =
      (module struct
        open Set_member

        let parse = parse_exn parse

        let%test _ = parse "c" = Single_char 'c'
        let%test _ = parse "a-b" = Range ('a', 'b')
      end)
    ;;

    let%test_module "Set parsing" =
      (module struct
        open Set

        let parse = parse_exn parse

        let%test _ = parse "[]" = []
        let%test _ = parse "[a]" = [ Single_char 'a' ]
        let%test _ = parse "[a-b]" = [ Range ('a', 'b') ]
      end)
    ;;

    let parse = parse_exn parse

    let%test _ = parse "c" = Char 'c'
    let%test _ = parse {|\b|} = Class (Pos Boundary)
    let%test _ = parse "[ab]" = Set [ Single_char 'a'; Single_char 'b' ]
    let%test _ = parse "a*" = Star (Char 'a')
    let%test _ = parse "a+" = Plus (Char 'a')
    let%test _ = parse "a{5}" = Count (Char 'a', 5)
    let%test _ = parse "a?" = Option (Char 'a')
    let%test _ = parse "a|b" = Choice [ Char 'a'; Char 'b' ]
    let%test _ = parse "*" = Any
    let%test _ = parse "ab" = re_str "ab"
  end)
;;