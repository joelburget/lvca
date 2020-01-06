open Belt

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
  : ('b list, 'c) Result.t
  =
  match lst with
  | [] -> Ok []
  | a :: rest ->
    (match f a with
     | Error msg -> Error msg
     | Ok b -> Result.flatMap (traverse_list_result f rest) (fun rest' -> Ok (b :: rest')))
;;

let rec sequence_list_result (lst : ('a, 'b) Result.t list) : ('a list, 'b) Result.t =
  match lst with
  | [] -> Ok []
  | Ok a :: rest -> Result.map (sequence_list_result rest) (fun rest' -> a :: rest')
  | Error msg :: _ -> Error msg
;;

module type Any = sig
  type t
end

module ArrayApplicative (A : Any) = struct
  exception Traversal_exn of A.t

  let sequence_array_result (arr : ('a, A.t) Result.t array) : ('a array, A.t) Result.t =
    try
      Ok
        (Array.map arr (function
           | Ok a -> a
           | Error b -> raise (Traversal_exn b)))
    with
    | Traversal_exn err -> Error err
  ;;

  let traverse_array_result (f : 'a -> ('b, A.t) Result.t)
        (arr : 'a array)
    : ('b array, A.t) Result.t
    =
    try
      Ok
        (Array.map arr (fun a ->
           match f a with
           | Ok b -> b
           | Error c -> raise (Traversal_exn c)))
    with
    | Traversal_exn err -> Error err
  ;;
end

let rec traverse_list_option (f : 'a -> 'b option)
          (lst : 'a list) : 'b list option =
  match lst with
  | [] -> Some []
  | a :: rest ->
    Option.flatMap (f a) (fun b ->
      Option.flatMap (traverse_list_option f rest) (fun rest' -> Some (b :: rest')))
;;

let rec sequence_list_option (lst : 'a option list) : 'a list option =
  match lst with
  | [] -> Some []
  | Some a :: rest -> Option.map (sequence_list_option rest) (fun rest' -> a :: rest')
  | None :: _ -> None
;;

let rec keep_some (lst : 'a option list) : 'a list =
  match lst with
  | [] -> []
  | Some a :: rest -> a :: keep_some rest
  | None :: rest -> keep_some rest
;;

let map_union m1 m2 =
  Belt.Map.String.merge m1 m2 (fun _k v1 v2 ->
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

let map_unions maps = fold_right map_union maps Belt.Map.String.empty

let set_unions sets = Belt.Set.String.(fold_right union sets empty)

let rec fold_left
  : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
  = fun f b lst ->
  match lst with
  | [] -> b
  | a :: as_ -> fold_left f (f b a) as_
;;

let map_error (result : ('a, 'b) Result.t) (f : 'b -> 'c) : ('a, 'c) Result.t =
  Result.(
    match result with
    | Ok x -> Ok x
    | Error err -> Error (f err))
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
  fun f lst -> List.(map lst f |> flatten)
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

let array_map_keep : ('a -> 'b option) -> 'a array -> 'b array =
  fun f arr ->
  let result = [||] in
  arr
  |. Belt.Array.forEach (fun a ->
    match f a with
    | None -> ()
    | Some b ->
      let _ = Js.Array2.push result b in
      ());
  result
;;

let get_option : 'b -> 'a option -> 'a =
  fun err -> function
    | None -> raise err
    | Some a -> a
;;

exception InvariantViolation of string

let invariant_violation str = InvariantViolation str

let get_option' : string -> 'a option -> 'a =
  fun msg -> get_option @@ invariant_violation ("invariant violation: " ^ msg)
;;

let array_of_stack : 'a MutableStack.t -> 'a array =
  fun stack ->
  let result = [||] in
  MutableStack.forEach stack (fun item ->
    let _ = Js.Array2.push result item in
    ());
  result
;;

let stringify_list : ('a -> string) -> string -> 'a list -> string =
  fun f sep elems ->
  elems |. Belt.List.toArray |. Belt.Array.map f |. Js.Array2.joinWith sep
;;
