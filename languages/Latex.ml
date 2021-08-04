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
  | Literal(string)
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
  | Literal str ->
    let str = if starts_with ~f:Char.is_alpha str then " " ^ str else str in
    Fmt.string ppf str
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
