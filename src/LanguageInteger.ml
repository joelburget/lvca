let abstractSyntax =
  {|
tm :=
  | neg(tm)
  | abs(tm)
  | add(tm; tm)
  | sub(tm; tm)
  | max(tm; tm)
  | min(tm; tm)
  // TODO: how to express literal
|}
;;

let concrete = {|
  // TODO
|}

(* TODO: lessen duplication *)
let rec eval' : Bigint.t list -> Binding.DeBruijn.term -> Bigint.t option =
 fun env tm ->
  match tm with
  | Operator ("neg", [ Scope ([], a) ]) ->
    (match eval' env a with
    | Some a' -> Some (Bigint.neg a')
    | _ -> None)
  | Operator ("abs", [ Scope ([], a) ]) ->
    (match eval' env a with
    | Some a' -> Some (Bigint.abs a')
    | _ -> None)
  | Operator ("add", [ Scope ([], a); Scope ([], b) ]) ->
    (match eval' env a, eval' env b with
    | Some a', Some b' -> Some (Bigint.add a' b')
    | _, _ -> None)
  | Operator ("sub", [ Scope ([], a); Scope ([], b) ]) ->
    (match eval' env a, eval' env b with
    | Some a', Some b' -> Some (Bigint.sub a' b')
    | _, _ -> None)
  | Operator ("max", [ Scope ([], a); Scope ([], b) ]) ->
    (match eval' env a, eval' env b with
    | Some a', Some b' -> Some (Bigint.max a' b')
    | _, _ -> None)
  | Operator ("min", [ Scope ([], a); Scope ([], b) ]) ->
    (match eval' env a, eval' env b with
    | Some a', Some b' -> Some (Bigint.min a' b')
    | _, _ -> None)
  | Var (i, 0) -> Belt.List.get env i
  | Primitive (PrimInteger i) -> Some i
  | Var _ | Operator _ | Sequence _ | Primitive (PrimString _) -> None
;;

let eval : Binding.DeBruijn.term -> Bigint.t option = eval' []

let eval_tm : Binding.Nominal.term -> (Binding.Nominal.term, string) Belt.Result.t =
 fun tm ->
  match Binding.DeBruijn.from_nominal tm with
  | Error msg -> Error msg
  | Ok db_tm ->
    (match eval db_tm with
    | None -> Error "failed to evaluate"
    | Some result -> Ok (Binding.Nominal.Primitive (PrimInteger result)))
;;
