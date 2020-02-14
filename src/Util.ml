open Tablecloth

let rec snoc lst a =
  match lst with
    | [] -> [a]
    | x :: xs -> x :: snoc xs a
;;

let rec unsnoc lst =
  match lst with
  | [] -> failwith "unsnoc empty list"
  | [ x ] -> [], x
  | x :: lst' ->
    let front, last = unsnoc lst' in
    x :: front, last
;;

let rec intersperse list el =
  match list with
  | [] | [ _ ] -> list
  | x :: y :: tl -> x :: el :: intersperse (y :: tl) el
;;

let rec intersperse_after list el =
  match list with
  | [] -> []
  | [ list_el ] -> [ list_el; el ]
  | x :: y :: tl -> x :: el :: intersperse_after (y :: tl) el
;;

let rec get_first (f : 'a -> 'b option) (lst : 'a list) : 'b option =
  match lst with
  | [] -> None
  | a :: as_ ->
    (match f a with
     | None -> get_first f as_
     | some_b -> some_b)
;;

let rec traverse_list_result (f : 'a -> ('b, 'c) Result.t)
          (lst : 'a list)
  : ('b, 'c list) Result.t
  =
  match lst with
  | [] -> Ok []
  | a :: rest ->
    (match f a with
     | Error msg -> Error msg
     | Ok b -> Result.and_then (traverse_list_result f rest)
       ~f:(fun rest' -> Ok (b :: rest')))
;;

let rec sequence_list_result (lst : ('a, 'b) Result.t list)
  : ('a, 'b list) Result.t =
  match lst with
  | [] -> Ok []
  | Ok a :: rest -> Result.map (fun rest' -> a :: rest') (sequence_list_result rest)
  | Error msg :: _ -> Error msg
;;

module type Any = sig
  type t
end

module ArrayApplicative (A : Any) = struct
  exception Traversal_exn of A.t

  let sequence_array_result (arr : (A.t, 'a) Result.t array)
    : (A.t, 'a array) Result.t =
    try
      Ok
        (Array.map ~f:(function
           | Ok a -> a
           | Error b -> raise (Traversal_exn b))
          arr)
    with
      Traversal_exn err -> Error err
  ;;

  let traverse_array_result (f : 'a -> (A.t, 'b) Result.t)
        (arr : 'a array)
    : (A.t, 'b array) Result.t
    =
    try
      Ok
        (Array.map arr ~f:(fun a ->
           match f a with
           | Ok b -> b
           | Error c -> raise (Traversal_exn c)))
    with
      Traversal_exn err -> Error err
  ;;
end

let rec traverse_list_option (f : 'a -> 'b option)
          (lst : 'a list) : 'b list option =
  match lst with
  | [] -> Some []
  | a :: rest ->
    Option.and_then (f a) ~f:(fun b ->
      Option.and_then (traverse_list_option f rest) ~f:(fun rest' -> Some (b :: rest')))
;;

let rec sequence_list_option (lst : 'a option list) : 'a list option =
  match lst with
  | [] -> Some []
  | Some a :: rest -> Option.map (sequence_list_option rest) ~f:(fun rest' -> a :: rest')
  | None :: _ -> None
;;

let rec keep_some (lst : 'a option list) : 'a list =
  match lst with
  | [] -> []
  | Some a :: rest -> a :: keep_some rest
  | None :: rest -> keep_some rest
;;

let int_map_union m1 m2 =
  IntDict.merge m1 m2 ~f:(fun _k v1 v2 ->
    match v1, v2 with
    | _, Some v -> Some v
    | Some v, None -> Some v
    | None, None -> None)
;;

let map_union m1 m2 =
  StrDict.merge m1 m2 ~f:(fun _k v1 v2 ->
    match v1, v2 with
    | _, Some v -> Some v
    | Some v, None -> Some v
    | None, None -> None)
;;

let rec fold_right (f : 'a -> 'b -> 'b) (lst : 'a list) (b : 'b) : 'b =
  match lst with
  | [] -> b
  | a :: as_ -> f a (fold_right f as_ b)
;;

let int_map_unions maps = fold_right int_map_union maps IntDict.empty

let map_unions maps = fold_right map_union maps StrDict.empty

let set_unions sets = StrSet.(fold_right union sets empty)

let array_map_unions : 'a StrDict.t array -> 'a StrDict.t
  = fun maps -> Array.fold_right ~initial:StrDict.empty ~f:map_union maps

let rec fold_left
  : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
  = fun f b lst ->
  match lst with
  | [] -> b
  | a :: as_ -> fold_left f (f b a) as_
;;

let map_error
  : ('err_a, 'a) Result.t -> f:('err_a -> 'err_b) -> ('err_b, 'a) Result.t
  = fun result ~f -> match result with
    | Ok x -> Ok x
    | Error err -> Error (f err)
;;

let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs
;;

(* TODO: remove (getBy) *)
let rec find (f : 'a -> bool) (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
  | x :: xs -> if f x then Some x else find f xs
;;

let rec find_by (lst : 'a list) (f : 'a -> 'b option) : 'b option =
  match lst with
  | [] -> None
  | x :: xs ->
    (match f x with
     | Some b -> Some b
     | None -> find_by xs f)
;;

let flip (f : 'a -> 'b -> 'c) : 'b -> 'a -> 'c = fun b a -> f a b
let id a = a

let list_flat_map : ('a -> 'b list) -> 'a list -> 'b list =
  fun f lst -> List.(map f lst |> flatten)
;;

let is_none = function
  | None -> true
  | Some _ -> false
;;

let is_some = function
  | None -> false
  | Some _ -> true
;;

let first_by (lst : 'a list) (f : 'a -> 'b option) : 'b option =
  let rec first_by' = function
    | [] -> None
    | x :: xs ->
      (match f x with
       | None -> first_by' xs
       | Some b -> Some b)
  in
  first_by' lst
;;

let get_option : 'b -> 'a option -> 'a =
  fun err -> function
    | None -> raise err
    | Some a -> a
;;

exception InvariantViolation of string

let invariant_violation str = raise (InvariantViolation str)

let get_option' : (unit -> string) -> 'a option -> 'a =
  fun msg -> function
    | None -> raise (InvariantViolation ("invariant violation: " ^ msg ()))
    | Some a -> a
;;

let stringify_list : ('a -> string) -> string -> 'a list -> string =
  fun f sep elems -> elems
    |> Array.from_list
    |> Array.map ~f
    |> Placemat.String.concat_array ~sep
;;

let get_result : ('b, 'a) Result.t -> ('b -> 'a) -> 'a
  = fun result f -> match result with
    | Ok a -> a
    | Error b -> f b
