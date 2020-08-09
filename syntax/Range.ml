type t =
  { start : int
  ; finish : int
  }

let mk start finish = { start; finish }

let to_string = fun { start; finish } -> Printf.sprintf "%i-%i" start finish

let (<>) r1 r2 = { start = min r1.start r2.start; finish = max r1.finish r2.finish }

let list_range
  = let open Base in
    function
    | [] -> None
    | ps ->
      let start = ps
        |> List.min_elt ~compare:(fun r1 r2 -> compare r1.start r2.start)
        |> Option.value_exn
        |> fun r -> r.start
      in
      let finish = ps
        |> List.max_elt ~compare:(fun r1 r2 -> compare r1.finish r2.finish)
        |> Option.value_exn
        |> fun r -> r.finish
      in
      Some { start; finish }

exception Empty_list

let list_range_nonempty lst = match list_range lst with
  | None -> raise Empty_list
  | Some range -> range

let pp = fun ppf { start; finish } -> Fmt.pf ppf "{%u,%u}" start finish
