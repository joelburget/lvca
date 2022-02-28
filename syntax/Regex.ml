open Base

module Class_base = struct
  type t =
    | Word (* \w / \W *)
    | Whitespace (* \s / \S *)
    | Digit
  (* \d / \D *)

  let ( = ) a b =
    match a, b with
    | Word, Word | Whitespace, Whitespace | Digit, Digit -> true
    | _ -> false
  ;;

  let to_predicate = function
    | Word -> Char.is_alpha
    | Whitespace -> Char.is_whitespace
    | Digit -> Char.is_digit
  ;;

  (* TODO: other javascript classes *)
  (* TODO: unicode categories *)

  let to_re =
    let open Re in
    function Word -> wordc | Whitespace -> space | Digit -> digit
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
    | Neg Word -> 'W'
    | Neg Whitespace -> 'S'
    | Neg Digit -> 'D'
  ;;

  let to_predicate = function
    | Pos base -> Class_base.to_predicate base
    | Neg base -> Lvca_util.(not << Class_base.to_predicate base)
  ;;

  let pp ppf cls = Fmt.pf ppf {|\%c|} (to_char cls)
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
end

module Set = struct
  type t = Set_member.t list

  let ( = ) a b = List.equal Set_member.( = ) a b
  let debug_pp ppf = Fmt.(pf ppf "[%a]" (list Set_member.pp ~sep:semi))
  let pp = Fmt.(list Set_member.pp ~sep:nop)
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
  | Count (re, _) | Plus re -> accepts_empty re
  | Choice res -> List.exists res ~f:accepts_empty
  | Concat pieces -> List.for_all pieces ~f:accepts_empty
  | Star _ | Option _ -> true
  | Class _ | Char _ | Set _ | Any -> false
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
      print (Concat [ Class (Pos Word); Class (Neg Word) ]);
      [%expect {|\w\W|}]
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
           ; Plus (Class (Pos Whitespace))
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
  end)
;;
