type t =
  { start : int
  ; finish : int
  }

type Caml.Format.stag += Stag of t

let mk start finish = { start; finish }

let to_string = fun { start; finish } -> Printf.sprintf "%i-%i" start finish

let extend_to = fun { start; finish } pos ->
  { start = min start pos; finish = max finish pos }

let union r1 r2 = { start = min r1.start r2.start; finish = max r1.finish r2.finish }

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

let intersect r1 r2 =
  let start = max r1.start r2.start in
  let finish = min r1.finish r2.finish in
  if start >= finish
  then None
  else Some { start; finish }

let is_before x y = x.finish <= y.start

let is_subset x y = x.start >= y.start && x.finish <= y.finish

let (=) x y = x.start = y.start && x.finish = y.finish

let pp = fun ppf { start; finish } -> Fmt.pf ppf "{%u,%u}" start finish

let stag_functions = Format.
  { mark_open_stag =
    (function
      | Stag rng -> Caml.Printf.sprintf "<%s>" (to_string rng)
      | _ -> ""
    )
  ; mark_close_stag =
    (function
      | Stag rng -> Caml.Printf.sprintf "</%s>" (to_string rng)
      | _ -> ""
    )
  ; print_open_stag = (fun _ -> ())
  ; print_close_stag = (fun _ -> ())
  }

let%test_module "Range" = (module struct
  let print_result = function
    | None -> Fmt.pr "%s" "None"
    | Some rng -> Fmt.pr "%a" pp rng

  let%expect_test _ =
    print_result (intersect (mk 0 1) (mk 2 3));
    [%expect{| None |}]

  let%expect_test _ =
    print_result (intersect (mk 0 4) (mk 2 3));
    [%expect{| {2,3} |}]

  let%expect_test _ =
    print_result (intersect (mk 0 2) (mk 1 3));
    [%expect{| {1,2} |}]

  let%expect_test _ =
    Fmt.pr "%b" (is_subset (mk 0 1) (mk 2 3));
    [%expect{| false |}]

  let%expect_test _ =
    Fmt.pr "%b" (is_subset (mk 0 4) (mk 2 3));
    [%expect{| false |}]

  let%expect_test _ =
    Fmt.pr "%b" (is_subset (mk 2 3) (mk 0 4));
    [%expect{| true |}]

  let%expect_test _ =
    Fmt.pr "%b" (is_subset (mk 2 3) (mk 2 3));
    [%expect{| true |}]

  let%expect_test _ =
    Fmt.pr "%b" (is_subset (mk 0 2) (mk 1 3));
    [%expect{| false |}]

  let%expect_test _ =
    Fmt.pr "%b" (is_subset (mk 18 19) (mk 21 24));
    [%expect{| false |}]

end);;
