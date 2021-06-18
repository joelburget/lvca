open Base
open Lvca_syntax

module Mk_lang =
[%lvca.abstract_syntax_module
{|
string : *

nat := Z() | S(nat)

list := Nil() | Cons(string; list)
|}]

(* very loosely *)
let correspondence =
  {|
let rec f = function
  | Z{info} <-> Nil{info}
  | S{info}(n) <-> Cons(info; f n)
|}
;;

module Lang = Mk_lang (Primitive.String)
module List = Lang.List
module Nat = Lang.Nat

module Foo : sig
  val list_to_nat : 'info List.t -> ('info * 'info Primitive.String.t option) Nat.t option
  val nat_to_list : ('info * 'info Primitive.String.t option) Nat.t -> 'info List.t option

  module Properties : sig
    val round_trip_1 : unit List.t -> Property_result.t
    val round_trip_2 : (unit * unit Primitive.String.t option) Nat.t -> Property_result.t
  end
end = struct
  open Option.Let_syntax

  let rec list_to_nat = function
    | Lang.Types.Nil info -> Some (Lang.Types.Z (info, None))
    | Cons (info, a, lst) ->
      let%map lst = list_to_nat lst in
      Lang.Types.S ((info, Some a), lst)
    | List_var _ -> failwith "TODO(20)"
  ;;

  let rec nat_to_list = function
    | Lang.Types.Z (info, None) -> Some (Lang.Types.Nil info)
    | S ((info, Some a), n) ->
      let%map lst = nat_to_list n in
      Lang.Types.Cons (info, a, lst)
    | _ -> None
  ;;

  module Properties = struct
    let round_trip_1 lst =
      match list_to_nat lst with
      | None -> Property_result.Uninteresting
      | Some nat ->
        (match nat_to_list nat with
        | None -> Failed "Failed to convert nat back to list"
        | Some lst' ->
          Property_result.check
            (List.equal ~info_eq:Unit.( = ) lst lst')
            "Lists not equal")
    ;;

    let round_trip_2 nat =
      match nat_to_list nat with
      | None -> Property_result.Uninteresting
      | Some lst ->
        (match list_to_nat lst with
        | None -> Failed "Failed to convert list back to nat"
        | Some nat' ->
          let info_eq =
            Lvca_util.Tuple2.equal
              Unit.( = )
              (Option.equal (Primitive.String.equal ~info_eq:Unit.( = )))
          in
          Property_result.check (Nat.equal ~info_eq nat nat') "Nats not equal")
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
