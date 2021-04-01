open Base
open Lvca_syntax
open Option.Let_syntax

module Lang =
[%abstract_syntax_module
{|
nat := Z() | S(nat)

list a := Nil() | Cons(a; list(a))
|}]

(* very loosely *)
let correspondence =
  {|
let rec f = function
  | Z{info} <-> Nil{info}
  | S{info}(n) <-> Cons(info; f n)
|}
;;

let rec list_to_nat = function
  | NonBinding.Operator (info, "Nil", []) ->
    Some (NonBinding.Operator ((info, None), "Z", []))
  | Operator (info, "S", [ a; lst ]) ->
    let%map lst = list_to_nat lst in
    NonBinding.Operator ((info, Some a), "Cons", [ lst ])
  | _ -> None
;;

let rec nat_to_list = function
  | NonBinding.Operator ((info, None), "Z", []) ->
    Some (NonBinding.Operator (info, "Nil", []))
  | Operator ((info, Some a), "Cons", [ lst ]) ->
    let%map lst = nat_to_list lst in
    NonBinding.Operator (info, "S", [ a; lst ])
  | _ -> None
;;

(* TODO:
  - check:
    * list_to_nat, nat_to_list round-trip
    * correspondence parser, matches list_to_nat, nat_to_list
    * give example: implement addition, get concatenation
*)
