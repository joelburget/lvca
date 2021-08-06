open Base
open Lvca_syntax
module List_model = Lvca_core.List_model.List

module Lang =
[%lvca.abstract_syntax_module
{|
string : *  // module Primitive.String
char : *  // module Primitive.Char
list : * -> *  // module List_model

tex :=
  | Control_seq(string)
  | Token(char)
  // | Literal(string)
  | Grouped(list tex)
  | Space()
|}]

open Lang

let trim_spaces tex_list =
  let go = List.drop_while ~f:(function Tex.Plain.Space -> true | _ -> false) in
  List.(tex_list |> go |> rev |> go |> rev)
;;

let starts_with ~f str =
  if String.length str = 0
  then false
  else (
    let c = String.get str 0 in
    f c)
;;

let ends_with ~f str =
  if String.length str = 0
  then false
  else (
    let c = String.get str (String.length str - 1) in
    f c)
;;

let is_ascii c = Char.to_int c < 128
let rec out = function List_model.Plain.Nil -> [] | Cons (x, xs) -> x :: out xs

let rec pp : Lang.Tex.Plain.t Fmt.t =
 fun ppf -> function
  | Token c -> Fmt.char ppf c
  (* | Literal str -> *)
  (*   let str = if starts_with ~f:Char.is_alpha str then " " ^ str else str in *)
  (*   Fmt.string ppf str *)
  | Control_seq str ->
    let f c = Char.is_alphanum c || not (is_ascii c) in
    let str = if starts_with ~f str then " " ^ str else str in
    Fmt.string ppf str
  | Grouped texs ->
    (match texs with
    | Cons (Grouped texs, Nil) -> pp ppf (Grouped texs)
    | _ -> Fmt.pf ppf "{%a}" Fmt.(list pp) (out texs))
  | Space -> Fmt.string ppf " "
;;

let parse : Lvca_provenance.Opt_range.t Lang.Tex.t list Lvca_parsing.t =
  let open Lvca_parsing in
  let open Lang.Tex in
  let is_token_char c =
    let code = Char.to_int c in
    code >= 31 && code <= 125
  in
  let control_seq =
    No_ws.char '\\' *> many1 (No_ws.satisfy Char.is_alpha)
    >>~ fun range value ->
    Control_seq (range, (range, String.of_char_list ('\\' :: value)))
  in
  let subsuptick =
    [ '_'; '^'; '\'' ]
    |> List.map ~f:(fun c ->
           No_ws.char c >>~ fun range c -> Control_seq (range, (range, String.of_char c)))
    |> choice ~failure_msg:"looking for `_`, `^`, or `'`"
  in
  let space = whitespace1 >>~ fun range _ -> Space range in
  let token =
    No_ws.satisfy is_token_char >>~ fun range value -> Token (range, (range, value))
  in
  let atom =
    fix (fun expr ->
        let grouped =
          No_ws.braces (many1 expr)
          >>~ fun range value ->
          Grouped (range, Lvca_core.List_model.into ~empty_info:range value)
        in
        choice
          ~failure_msg:
            "looking for a control sequence, `_`, `^`, `'`, group, space, or token"
          [ control_seq <?> "control sequence"
          ; subsuptick <?> "`_`, `^`, `'`"
          ; grouped <?> "group"
          ; space <?> "space"
          ; token <?> "token"
          ])
    <?> "atom"
  in
  many1 atom <?> "Tex math expression"
;;

let%test_module _ =
  (module struct
    let () = Stdlib.Format.(pp_set_margin std_formatter 200)

    let parse_print str =
      match Lvca_parsing.(parse_string parse) str with
      | Ok tms ->
        Fmt.pr "%a\n" Fmt.(list ~sep:(any "") pp) (List.map tms ~f:Lang.Tex.to_plain)
      | Error msg -> Fmt.pr "%s\n" msg
    ;;

    let%expect_test _ =
      parse_print "1";
      parse_print "1 + 1";
      parse_print " ";
      parse_print "a";
      parse_print {|\alpha|};
      parse_print {|\sqrt{1}|};
      parse_print {|\sqrt 1|};
      parse_print {|\frac{1}{2}|};
      parse_print {|\frac 1 2|};
      parse_print {|x_1|};
      parse_print {|x_{1 + 1}|};
      parse_print {|x^1|};
      parse_print {|x^{1 + 1}|};
      parse_print {|x^\begingroup1 + 1\endgroup|};
      parse_print {|x_1^2|};
      parse_print {|x^1_2|};
      parse_print {|x'''|};
      parse_print {|x \over y|};
      parse_print
        {|\f\relax{x} = \int_{-\infty}^\infty
    \f\hat\xi\,e^{2 \pi i \xi x}
    \,d\xi|};
      [%expect
        {|
    1
    1 + 1

    a
    \alpha
    \sqrt{1}
    \sqrt 1
    \frac{1}{2}
    \frac 1 2
    x_1
    x_{1 + 1}
    x^1
    x^{1 + 1}
    x^\begingroup1 + 1\endgroup
    x_1^2
    x^1_2
    x'''
    x \over y
    \f\relax{x} = \int_{-\infty}^\infty \f\hat\xi\,e^{2 \pi i \xi x} \,d\xi
    |}]
    ;;
  end)
;;
