open Base
open Lvca.Binding

let abstract_syntax =
  {|
import { integer } from "lvca/builtin"

tm :=
  | lit(integer())
  | neg(tm())
  | abs(tm())
  | add(tm(); tm())
  | sub(tm(); tm())
  | mul(tm(); tm())
  | max(tm(); tm())
  | min(tm(); tm())
|}
;;

let concrete_syntax =
  {|
INT := /[0-9]+/
BAR := "|"
ADD := "+"
SUB := "-"
MUL := "*"
ABS := "abs"
MAX := "max"
MIN := "min"
LPAREN := "("
RPAREN := ")"

tm := tm = tm_1 { tm }

tm_1 :=
  | SUB x = tm_2 { neg(x) }
  | x = tm_1 ADD y = tm_2 { add(x; y) }
  | x = tm_1 SUB y = tm_2 { sub(x; y) }
  | tm = tm_2 { tm }

tm_2 :=
  | x = tm_2 MUL y = tm_3 { mul(x; y) }
  | tm = tm_3 { tm }

tm_3 :=
  | MAX x = tm_4 y = tm_4 { max(x; y) }
  | MIN x = tm_4 y = tm_4 { min(x; y) }
  | tm = tm_4 { tm }

tm_4 :=
  | LPAREN tm = tm_1 RPAREN { tm }
  | BAR tm = tm_1 BAR { abs(tm) }
  | i = INT { lit(integer(i)) }
|}
;;

(* TODO: lessen duplication *)
let rec eval' : Bigint.t list -> DeBruijn.term -> Bigint.t option =
 fun env tm ->
  match tm with
  | Operator (op, [ Scope ([], a) ]) ->
    (match eval' env a with
    | Some a' ->
      (match op with
      | "neg" -> Some (Bigint.neg a')
      | "abs" -> Some (Bigint.abs a')
      | _ -> None)
    | _ -> None)
  | Operator (op, [ Scope ([], a); Scope ([], b) ]) ->
    (match eval' env a, eval' env b with
    | Some a', Some b' ->
      Bigint.(
        (match op with
        | "add" -> Some (a' + b')
        | "sub" -> Some (a' - b')
        | "mul" -> Some (a' * b')
        | "max" -> Some (max a' b')
        | "min" -> Some (min a' b')
        | _ -> None))
    | _, _ -> None)
  | Var (i, 0) -> List.nth env i
  | Primitive (PrimInteger i) -> Some i
  | Var _ | Operator _ | Primitive _ -> None
;;

let eval : DeBruijn.term -> Bigint.t option = eval' []

let eval_tm : Nominal.term -> (Nominal.term, string) Result.t =
 fun tm ->
  match DeBruijn.from_nominal tm with
  | Error msg -> Error msg
  | Ok db_tm ->
    (match eval db_tm with
    | None -> Error "failed to evaluate"
    | Some result -> Ok (Nominal.Primitive (PrimInteger result)))
;;
