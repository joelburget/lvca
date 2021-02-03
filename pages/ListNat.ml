open Brr
open Lvca_syntax
open Prelude

module Language = struct
  let nat = [%lvca_abstract_syntax "nat := Z() | S(nat)"]
  let list = [%lvca_abstract_syntax "list a := Nil() | Cons(a; list a)"]

  let rec nat_to_list = function
    | NonBinding.Operator (_, "Z", []) -> NonBinding.Operator ((), "Nil", [])
    | Operator (i, "S", [ nat ]) -> Operator ((), "Cons", [ i; nat_to_list nat ])
    | Operator _ | Primitive _ -> failwith "invariant violation"
  ;;

  let rec list_to_nat = function
    | NonBinding.Operator (_, "Nil", []) -> NonBinding.Operator (None, "Z", [])
    | Operator ((), "Cons", [ i; list ]) -> Operator (Some i, "S", [ list_to_nat list ])
    | Operator _ | Primitive _ -> failwith "invariant violation"
  ;;

  (* From this defn, should be able to derive list concatenation *)
  let core_sum =
    {|let rec sum = \(x : nat) (y : nat) -> match x with {
    | Z() -> y
    | S(x') -> S(sum x' y)
    }|}
  ;;

  (* From this defn, should be able to derive predecessor *)
  let core_tail =
    {|let tail = \(x : a list) -> match x with {
      | Cons(a; as) -> as
      | Nil() -> Nil
      }
    |}
  ;;
end

module Model = struct
  let initial_model = ()
end

module View = struct
  let view _model = El.div [ txt "TODO" ]
end

let stateless_view () = View.view Model.initial_model
