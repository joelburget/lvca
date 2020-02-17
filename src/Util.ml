open Core_kernel
open Result

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
     | Ok b -> traverse_list_result f rest >>=
       fun rest' -> Ok (b :: rest'))
;;

let rec sequence_list_result (lst : ('a, 'b) Result.t list)
  : ('a list, 'b) Result.t =
  match lst with
  | [] -> Ok []
  | Ok a :: rest -> Result.map (sequence_list_result rest)
     ~f:(fun rest' -> a :: rest')
  | Error msg :: _ -> Error msg
;;

module type Any = sig
  type t
end

module ArrayApplicative (A : Any) = struct
  exception Traversal_exn of A.t

  let sequence_array_result (arr : (A.t, 'a) Result.t array)
    : (A.t array, 'a) Result.t =
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
    : (A.t array, 'b) Result.t
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
  | a :: rest -> Option.(
    f a >>= fun b ->
    traverse_list_option f rest >>= fun rest' ->
    Some (b :: rest'))
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
  Int.Map.merge m1 m2 ~f:(fun ~key:_k -> function
    | `Both (_, v) -> Some v
    | `Left v | `Right v -> Some v)
;;

let map_union m1 m2 =
  String.Map.merge m1 m2 ~f:(fun ~key:_k -> function
    | `Both (_, v) -> Some v
    | `Left v | `Right v -> Some v)
;;

let rec fold_right (f : 'a -> 'b -> 'b) (lst : 'a list) (b : 'b) : 'b =
  match lst with
  | [] -> b
  | a :: as_ -> f a (fold_right f as_ b)
;;

let int_map_unions maps = fold_right int_map_union maps Int.Map.empty

let map_unions maps = fold_right map_union maps String.Map.empty

let array_map_unions : 'a String.Map.t array -> 'a String.Map.t
  = fun maps -> Array.fold_right ~init:String.Map.empty ~f:map_union maps

let rec fold_left
  : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
  = fun f b lst ->
  match lst with
  | [] -> b
  | a :: as_ -> fold_left f (f b a) as_
;;

let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs
;;

let list_flat_map : ('a -> 'b list) -> 'a list -> 'b list =
  fun f lst -> List.(map ~f lst |> join)
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
    |> Array.of_list
    |> Array.map ~f
    |> String.concat_array ~sep
;;

let get_result : ('b, 'a) Result.t -> ('b -> 'a) -> 'a
  = fun result f -> match result with
    | Ok a -> a
    | Error b -> f b

module Array = struct
  type 'a t = 'a array

  let reverse_iteri : 'a t -> f:(int -> 'a -> unit) -> unit
    = fun arr ~f ->
      for i = (Array.length arr) - 1 downto 0 do
        f i arr.(i)
      done

  let reverse_iter : 'a t -> f:('a -> unit) -> unit
    = fun arr ~f -> reverse_iteri arr ~f:(fun _ a -> f a)

  let reverse : 'a t -> 'a t
    = fun arr ->
      let result = Core_kernel.Array.copy arr in
      Core_kernel.Array.rev_inplace result;
      result
end

module String = struct
  module Map = struct
    type 'a t = 'a String.Map.t

    let remove_many : 'a t -> string array -> 'a t
      = fun map keys -> Core_kernel.Array.fold keys
        ~init:map ~f:Core_kernel.String.Map.remove
  end
end

module MutableSet = struct
  module Int = struct
    type t = int Core_kernel.Hash_set.t
  end
end
