open Belt

let rec unsnoc lst =
  match lst with
  | []        -> failwith "unsnoc empty list"
  | [x]       -> ([], x)
  | x :: lst' -> let (front, last) = unsnoc lst' in (x :: front, last)

let rec intersperse list el =
  match list with
  | [] | [_]     -> list
  | x :: y :: tl -> x :: el :: intersperse (y::tl) el

let rec intersperse_after list el =
  match list with
  | []           -> []
  | [ list_el ]  -> [ list_el; el ]
  | x :: y :: tl -> x :: el :: intersperse_after (y::tl) el

let rec get_first (f : 'a -> 'b option) (lst : 'a list) : 'b option
  = match lst with
  | [] -> None
  | a :: as_ -> (match f a with
    | None -> get_first f as_
    | some_b -> some_b
  )

let rec traverse_list_result
  (f : 'a -> ('b, 'c) Result.t)
  (lst : 'a list)
  : ('b list, 'c) Result.t = match lst with
  | [] -> Ok []
  | a :: rest -> (match f a with
    | Error msg -> Error msg
    | Ok b -> Result.flatMap
      (traverse_list_result f rest)
      (fun rest' -> Ok (b :: rest'))
    )

exception Traversal_exn of string

let rec traverse_array_result
  (f : 'a -> ('b, string) Result.t)
  (arr : 'a array)
  : ('b array, 'c) Result.t =
    try
      Ok (Array.map arr (fun a -> match f a with
        | Ok    b -> b
        | Error c -> raise (Traversal_exn c)
      ))
    with
      Traversal_exn err -> Error err

let rec sequence_list_result
  (lst : ('a, 'b) Result.t list)
  : ('a list, 'b) Result.t = match lst with
  | []             -> Ok []
  | Ok a :: rest   -> Result.map
    (sequence_list_result rest)
    (fun rest' -> a :: rest')
  | Error msg :: _ -> Error msg

let rec sequence_array_result
  (arr : ('a, string) Result.t array)
  : ('a array, string) Result.t =

    try
      Ok (Array.map arr (function
        | Ok a    -> a
        | Error b -> raise (Traversal_exn b)
      ))
    with
      Traversal_exn err -> Error err

let rec sequence_list_option
  (lst : 'a option list)
  : ('a list option) = match lst with
  | []             -> Some []
  | Some a :: rest -> Option.map
    (sequence_list_option rest)
    (fun rest' -> a :: rest')
  | None :: _ -> None

let union m1 m2 =
  Belt.Map.String.merge
    m1
    m2
    (fun _k v1 v2 -> match (v1, v2) with
      | (_,      Some v) -> Some v
      | (Some v, None  ) -> Some v
      | (None,   None  ) -> None
    )

let rec fold_right (f : ('a * 'b) -> 'b) (lst : 'a list) (b : 'b) : 'b
  = match lst with
    | []       -> b
    | a :: as_ -> f(a, fold_right f as_ b)

let map_error (result : ('a, 'b) Result.t) (f : 'b -> 'c) : ('a, 'c) Result.t
  = Result.(match result with
  | Ok x      -> Ok x
  | Error err -> Error (f(err))
  )

let rec sum = function
  | []       -> 0
  | x  :: xs -> x + sum xs

let rec find (f: 'a -> bool) (lst: 'a list) : 'a option = match lst with
  | []       -> None
  | x  :: xs -> if f x then Some x else find f xs

let flip (f : 'a -> 'b -> 'c): ('b -> 'a -> 'c)
  = fun b a -> f a b

let id a = a

let rec list_flat_map : ('a -> 'b list) -> 'a list -> 'b list
  = fun f lst -> List.(map lst f |> flatten)
