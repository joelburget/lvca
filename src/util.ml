open Belt

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

let rec traverse_list_result (lst : (('a, 'b) Result.t) list)
  : ('a list, 'b) Result.t = match lst with
  | []             -> Ok []
  | Ok a :: rest   -> Result.map
    (traverse_list_result rest)
    (fun rest' -> a :: rest')
  | Error msg :: _ -> Error msg

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
    | [] -> b
    | a :: as_ -> f(a, fold_right f as_ b)

let map_error (result : ('a, 'b) Result.t) (f : 'b -> 'c) : ('a, 'c) Result.t
  = Result.(match result with
  | Ok x      -> Ok x
  | Error err -> Error (f(err))
  )
