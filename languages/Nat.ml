open Base
open Lvca_syntax

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

module Foo (A : LanguageObject.AllTermS) : sig
  (* module List : LanguageObject.AllTermS *)
  module List : sig
    type 'info t
  end

  val list_to_nat : 'info List.t -> ('info * 'info A.t option) Lang.Nat.t option
  val nat_to_list : ('info * 'info A.t option) Lang.Nat.t -> 'info List.t option

  module Properties : sig
    val round_trip_1 : unit List.t -> PropertyResult.t
    val round_trip_2 : (unit * unit A.t option) Lang.Nat.t -> PropertyResult.t
  end
end = struct
  module List = Lang.List (A)
  open Option.Let_syntax

  let rec list_to_nat = function
    | List.Nil info -> Some (Lang.Nat.Z (info, None))
    | Cons (info, a, lst) ->
      let%map lst = list_to_nat lst in
      Lang.Nat.S ((info, Some a), lst)
  ;;

  let rec nat_to_list = function
    | Lang.Nat.Z (info, None) -> Some (List.Nil info)
    | S ((info, Some a), n) ->
      let%map lst = nat_to_list n in
      List.Cons (info, a, lst)
    | _ -> None
  ;;

  module Properties = struct
    let round_trip_1 lst =
      match list_to_nat lst with
      | None -> PropertyResult.Uninteresting
      | Some nat ->
        (match nat_to_list nat with
        | None -> Failed "Failed to convert nat back to list"
        | Some lst' ->
          PropertyResult.check (List.equal ~info_eq:Unit.( = ) lst lst') "Lists not equal")
    ;;

    let round_trip_2 nat =
      match nat_to_list nat with
      | None -> PropertyResult.Uninteresting
      | Some lst ->
        (match list_to_nat lst with
        | None -> Failed "Failed to convert list back to nat"
        | Some nat' ->
          let info_eq =
            Lvca_util.Tuple2.equal Unit.( = ) (Option.equal (A.equal ~info_eq:Unit.( = )))
          in
          PropertyResult.check (Lang.Nat.equal ~info_eq nat nat') "Nats not equal")
    ;;
  end
end

(* TODO:
  - check:
    * list_to_nat, nat_to_list round-trip
    * correspondence parser, matches list_to_nat, nat_to_list
    * give example: implement addition, get concatenation
*)
