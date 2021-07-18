open Base.Result.Let_syntax

type ('info, 'a) t =
  | Nil of 'info
  | Cons of 'info * 'a * ('info, 'a) t

module Plain = struct
  type 'a t =
    | Nil
    | Cons of 'a * 'a t
end

let rec of_plain = function
  | Plain.Nil -> Nil ()
  | Cons (x, xs) -> Cons ((), x, of_plain xs)
;;

let rec to_plain = function
  | Nil _ -> Plain.Nil
  | Cons (_, x, xs) -> Plain.Cons (x, to_plain xs)
;;

let info = function Nil i | Cons (i, _, _) -> i

let rec map_info ~f = function
  | Nil i -> Nil (f i)
  | Cons (i, a, t) -> Cons (f i, a, map_info ~f t)
;;

let rec equal ~info_eq a x1 x2 =
  match x1, x2 with
  | Nil i1, Nil i2 -> info_eq i1 i2
  | Cons (i1, a1, t1), Cons (i2, a2, t2) ->
    info_eq i1 i2 && a a1 a2 && equal ~info_eq a t1 t2
  | _, _ -> false
;;

let rec to_nominal = function
  | Nil info -> Nominal.Term.Operator (info, "Nil", [])
  | Cons (info, x, xs) ->
    Operator (info, "Cons", [ Scope ([], x); Scope ([], to_nominal xs) ])
;;

let rec of_nominal = function
  | Nominal.Term.Operator (info, "Nil", []) -> Ok (Nil info)
  | Operator (info, "Cons", [ Scope ([], x); Scope ([], xs) ]) ->
    let%map xs = of_nominal xs in
    Cons (info, x, xs)
  | tm -> Error tm
;;

let map xs ~f =
  let rec go = function
    | Nil info -> Nil info
    | Cons (info, x, xs) -> Cons (info, f x, go xs)
  in
  go xs
;;

let rec length = function Nil _ -> 0 | Cons (_, _, xs) -> length xs + 1

let fold xs ~init ~f =
  let rec go = function Nil _info -> init | Cons (_, x, xs) -> f (go xs) x in
  go xs
;;
