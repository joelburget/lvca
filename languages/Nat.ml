open Base
open Lvca_syntax
open Lvca_util

module Lang =
[%lvca.abstract_syntax_module
{|
string : *

nat := Z() | S(nat);

list := Nil() | Cons(string; list);
|}
, { string = "Primitive.String" }]

(* very loosely *)
let correspondence =
  {|
let rec f = function
  | Z{info} <-> Nil{info}
  | S{info}(n) <-> Cons(info; f n)
|}
;;

module List = Nominal.Convertible.Extend (Lang.List)
module Nat = Nominal.Convertible.Extend (Lang.Nat)

module Foo : sig
  val list_to_nat : List.t -> Nat.t option
  val nat_to_list : Nat.t -> List.t option

  module Properties : sig
    val round_trip_1 : List.t -> Property_result.t
    val round_trip_2 : Nat.t -> Property_result.t
  end
end = struct
  open Option.Let_syntax

  let rec list_to_nat = function
    | Lang.Types.Nil info -> Some (Lang.Types.Z info)
    | Cons (info, _a, lst) ->
      let%map lst = list_to_nat lst in
      Lang.Types.S (info, lst)
  ;;

  let rec nat_to_list = function
    | Lang.Types.Z info -> Some (Lang.Types.Nil info)
    | S (info, n) ->
      let%map lst = nat_to_list n in
      Lang.Types.Cons (info, failwith "TODO", lst)
  ;;

  module Properties = struct
    let round_trip_1 lst =
      match list_to_nat lst with
      | None -> Property_result.Uninteresting
      | Some nat ->
        (match nat_to_list nat with
        | None -> Failed "Failed to convert nat back to list"
        | Some lst' -> Property_result.check (List.equivalent lst lst') "Lists not equal")
    ;;

    let round_trip_2 nat =
      match nat_to_list nat with
      | None -> Property_result.Uninteresting
      | Some lst ->
        (match list_to_nat lst with
        | None -> Failed "Failed to convert list back to nat"
        | Some nat' -> Property_result.check (Nat.equivalent nat nat') "Nats not equal")
    ;;
  end
end

(* TODO:
  - un-hardcode string
  - check:
    * list_to_nat, nat_to_list round-trip
    * correspondence parser, matches list_to_nat, nat_to_list
    * give example: implement addition, get concatenation
*)
