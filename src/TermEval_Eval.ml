open Base
open Binding

(* TODO: - clean up error handling - clean up typechecking *)

(* hardcoded for this language: tm() := | true() | false() | ite(tm(); tm(); tm()) | annot(tm(); ty())
   | app(tm(); tm()) | fun(tm(). tm())

   ty := | bool() | arr(ty(); ty()) *)

type ty =
  | Bool
  | Arr of ty * ty

exception InvariantViolation of string
exception Unknown of unit DeBruijn.term
exception CheckFailure
exception InferFailure

let check_assert : bool -> unit = function true -> () | false -> raise CheckFailure

let to_string : 'a DeBruijn.term -> string =
 fun tm ->
  match DeBruijn.to_nominal tm with
  | Some tm' -> Nominal.pp_term_str tm'
  | None -> raise (InvariantViolation "to_string")
;;

let rec ty_to_string : ty -> string = function
  | Bool -> "bool()"
  (* TODO: parenthesize correctly *)
  | Arr (t1, t2) -> Printf.sprintf "arr(%s; %s)" (ty_to_string t1) (ty_to_string t2)
;;

let rec ty_of : 'a DeBruijn.term -> ty = function
  | Operator (_, "bool", []) -> Bool
  | Operator (_, "arr", [ Scope (_, [], [t1]); Scope (_, [], [t2]) ])
  -> Arr (ty_of t1, ty_of t2)
  | other -> raise (Unknown other)
;;

let rec check : ty list -> ty -> 'a DeBruijn.term -> bool =
 fun env ty tm ->
  match tm with
  | Var (_, ix, 0) ->
    let ty' = List.nth env ix
      |> Util.Option.get_invariant (fun () -> (Printf.sprintf
        "bad environment index %n, environment size %n" ix (List.length env)))
    in
    Caml.(ty' = ty)
  | Var _ -> raise (InvariantViolation "unexpected non-variable binding")
  | Operator (_, "true", []) | Operator (_, "false", []) -> Caml.(ty = Bool)
  | Operator (_, "ite",
    [ Scope (_, [], [cond]); Scope (_, [], [b1]); Scope (_, [], [b2]) ])
  -> check env Bool cond && check env Bool b1 && check env Bool b2
  | Operator (_, "annot", [ Scope (_, [], [tm]); Scope (_, [], [ty]) ])
  -> check env (ty_of ty) tm
  | Operator (_, "app", [ Scope (_, [], [f]); Scope (_, [], [arg]) ]) ->
    (match infer env f with
    | Some (Arr (t1, t2)) -> check (t1 :: env) t2 f && check env t1 arg
    | _ -> false)
  | Operator (_, "fun", [ Scope (_, [ _var ], [body]) ]) ->
    (match ty with
    | Arr (t1, t2) -> check (t1 :: env) t2 body
    | ty -> raise (InvariantViolation ("check fun with " ^ ty_to_string ty)))
  | _ -> false

and infer : ty list -> 'a DeBruijn.term -> ty option =
 fun env tm ->
  match tm with
  | Var (_, ix, 0) -> List.nth env ix
  | Operator (_, "true", []) | Operator (_, "false", []) -> Some Bool
  | Operator (_, "ite",
    [ Scope (_, [], [cond]); Scope (_, [], [b1]); Scope (_, [], [b2]) ])
  ->
    check_assert (check env Bool cond);
    (match infer env b1 with
    | Some t1 ->
      check_assert (check env t1 b2);
      Some t1
    | None ->
      (match infer env b2 with
      | Some t2 ->
        check_assert (check env t2 b1);
        Some t2
      | None -> None))
  | Operator (_, "app", [ Scope (_, [], [f]); Scope (_, [], [arg]) ]) ->
    (match infer env f with
    | Some (Arr (t1, t2)) ->
      check_assert (check (t1 :: env) t2 f);
      check_assert (check env t1 arg);
      Some t2
    | _ -> None)
  | Operator (_, "annot", [ Scope (_, [], [tm]); Scope (_, [], [ty]) ]) ->
    let ty' = ty_of ty in
    check_assert (check env ty' tm);
    Some ty'
  | _ -> None
;;

let is_true = function DeBruijn.Operator (_, "true", []) -> true | _ -> false

let rec eval' : 'a DeBruijn.term list -> 'a DeBruijn.term -> 'a DeBruijn.term =
 fun env tm ->
  match tm with
  | Var (_, ix, 0) -> List.nth env ix
    |> Util.Option.get_invariant (fun () -> (Printf.sprintf
      "bad environment index %n, environment size %n" ix (List.length env)))
  | Var _ -> raise (InvariantViolation "unexpected non-variable binding")
  | Operator (_, "true", []) | Operator (_, "false", []) -> tm
  | Operator (_, "ite",
    [ Scope (_, [], [cond]); Scope (_, [], [b1]); Scope (_, [], [b2]) ]) ->
    if is_true (eval' env cond) then eval' env b1 else eval' env b2
  | Operator (_, "annot", [ Scope (_, [], [tm']); _ ]) -> eval' env tm'
  | Operator (_, "app", [ Scope (_, [], [f]); Scope (_, [], [arg]) ]) ->
    (match eval' env f with
    | Operator (_, "fun", [ Scope (_, [ _var ], [body]) ])
    -> eval' (arg :: env) body
    | other -> raise (InvariantViolation ("unexpected " ^ to_string other)))
  | Primitive _ -> raise (InvariantViolation "eval' Primitive")
  | op -> op
;;

type check_eval_result =
  | Failure of string
  | CheckFailure
  (* | CheckFailure of string *)
  (* | InferFailure of string *)
  | EvalResult of string * string

let check_eval : 'a Nominal.term -> (string * string, string) Result.t =
 fun tm ->
  match DeBruijn.from_nominal tm with
  | Error err -> Error err
  | Ok tm' ->
    (match infer [] tm' with
    | Some ty -> Ok (ty_to_string ty, to_string (eval' [] tm'))
    | None -> Error "couldn't infer type"
    | exception Unknown tm -> Error ("Unknown operator: " ^ to_string tm))
;;
