open Core_kernel

let abstractSyntax =
  {|
import { integer } from "builtins"

tm :=
  | lit(integer)
  | neg(tm)
  | abs(tm)
  | add(tm; tm)
  | sub(tm; tm)
  | mul(tm; tm)
  | max(tm; tm)
  | min(tm; tm)
|}
;;

let concreteSyntax =
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

tm :=
  | LPAREN tm RPAREN { $2 }
  | BAR tm BAR { abs($2) }
  | INT { lit(integer($1)) }
  > MAX tm tm { max($2; $3) }
  | MIN tm tm { min($2; $3) }
  > tm MUL tm { mul($1; $3) } %left
  > SUB tm { neg($2) }
  | tm ADD tm { add($1; $3) } %left
  | tm SUB tm { sub($1; $3) } %left
|}
;;

(* TODO: lessen duplication *)
let rec eval' : Bigint.t list -> Binding.DeBruijn.term -> Bigint.t option =
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
  | Var _ | Operator _ | Sequence _ | Primitive (PrimString _) -> None
;;

let eval : Binding.DeBruijn.term -> Bigint.t option = eval' []

let eval_tm : Binding.Nominal.term -> (Binding.Nominal.term, string) Result.t =
 fun tm ->
  match Binding.DeBruijn.from_nominal tm with
  | Error msg -> Error msg
  | Ok db_tm ->
    (match eval db_tm with
    | None -> Error "failed to evaluate"
    | Some result -> Ok (Binding.Nominal.Primitive (PrimInteger result)))
;;
