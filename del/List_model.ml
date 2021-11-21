open Lvca_syntax
module Kernel = [%lvca.abstract_syntax_module "list a := Nil() | Cons(a; list a)"]
include Kernel.List

type 'a t = 'a Kernel.List.t =
  | Nil of Provenance.t
  | Cons of Provenance.t * 'a * 'a t

let rec of_list xs =
  let info = Provenance.of_here [%here] in
  match xs with [] -> mk_Nil ~info | x :: xs -> Cons (info, x, of_list xs)
;;

let rec to_list xs = match xs with Nil _ -> [] | Cons (_, x, xs) -> x :: to_list xs
let rec map ~f = function Nil i -> Nil i | Cons (i, x, xs) -> Cons (i, f x, map ~f xs)

let rec extract_vars_from_empty_pattern = function
  | Pattern.Operator (_, "Nil", []) -> []
  | Operator (_, "Cons", [ Var (_, name); pats ]) ->
    name :: extract_vars_from_empty_pattern pats
  | _ -> Lvca_util.invariant_violation ~here:[%here] "Invalid empty list pattern"
;;

let rec make_empty_pattern vars =
  match vars with
  | [] -> Pattern.Operator (Provenance.of_here [%here], "Nil", [])
  | (v, pos) :: vars ->
    Operator
      (Provenance.of_here [%here], "Cons", [ Var (v, pos); make_empty_pattern vars ])
;;
