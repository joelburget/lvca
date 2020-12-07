open Base

type t = Range.t list

let rec invariants = function
  | [] | [ _ ] -> true
  | x :: x' :: xs -> x.Range.finish < x'.Range.start && invariants xs
;;

let of_opt_range = function None -> [] | Some rng -> [ rng ]
let to_string ranges = ranges |> List.map ~f:Range.to_string |> String.concat ~sep:", "
let ( = ) r1 r2 = List.equal Range.( = ) r1 r2

(* Takes a list of ranges which satisfy the requirement that start positions are ordered,
 * but may overlap, and merges overlapping neighbors to make it well-formed. *)
let rec fixup_ordered xs =
  match xs with
  | [] | [ _ ] -> xs
  | x :: x' :: xs ->
    if x.Range.finish >= x'.Range.start
    then Range.union x x' :: fixup_ordered xs
    else x :: fixup_ordered (x' :: xs)
;;

let of_list lst =
  lst
  |> List.sort ~compare:(fun r1 r2 -> compare r1.Range.start r2.Range.start)
  |> fixup_ordered
;;

let of_set set = set |> Set.to_list |> of_list

let rec union r1 r2 =
  let go r1 r2 =
    match r1, r2 with
    | [], [] -> []
    | ranges, [] | [], ranges -> ranges
    | x :: xs, y :: ys ->
      if x.Range.finish < y.Range.start (* x doesn't overlap with any range *)
      then x :: union xs r2
      else if y.Range.finish < x.Range.start (* y doesn't overlap with any range *)
      then y :: union r1 ys
      else (
        (* x and y overlap (and their union could also overlap) *)
        let xy = Range.union x y in
        union (xy :: xs) ys)
  in
  go r1 r2 |> fixup_ordered
;;

let rec is_subset r1 r2 =
  match r1, r2 with
  | [], _ -> true
  | _, [] -> false
  | x :: xs, y :: ys ->
    if x.Range.start < y.Range.start
    then false
    else if x.Range.start < y.Range.finish
    then Range.is_subset x y && is_subset xs r2
    else is_subset r1 ys
;;

let rec intersect r1 r2 =
  match r1, r2 with
  | [], _ | _, [] -> []
  | x :: xs, y :: ys ->
    if x.Range.finish <= y.Range.start (* x doesn't overlap with any range *)
    then intersect xs r2
    else if y.Range.finish <= x.Range.start (* y doesn't overlap with any range *)
    then intersect r1 ys
    else (
      (* x and y overlap *)
      let first_intersection = Range.intersect x y |> Option.value_exn in
      let remaining_intersections =
        if x.Range.finish < y.Range.finish (* remove x *)
        then intersect xs r2
        else if y.Range.finish < x.Range.finish (* remove y *)
        then intersect r1 ys
        else intersect xs ys
        (* remove both x and y *)
      in
      first_intersection :: remaining_intersections)
;;

let pp = Fmt.box (Fmt.list ~sep:Fmt.comma Range.pp)

type string_status = Covered of Range.t | Uncovered of Range.t

let rec mark_string' rngs str start = match rngs with
  | [] ->
    if Int.(start < String.length str)
    then [Uncovered Range.{ start; finish = String.length str }]
    else []
  | rng :: rngs ->
    let start_covered = Covered rng :: mark_string' rngs str rng.Range.finish in
    if Int.(rng.Range.start = start)
    then start_covered
    else Uncovered Range.{ start; finish = rng.Range.start } :: start_covered

let mark_string rngs str = mark_string' rngs str 0

let pp_mark ppf = function
  | Covered range -> Fmt.pf ppf "c%a" Range.pp range
  | Uncovered range -> Fmt.pf ppf "u%a" Range.pp range

let pp_marks = Fmt.box (Fmt.list ~sep:Fmt.sp pp_mark)

let%test_module "Ranges" =
  (module struct
    let test1 = Range.[ mk 0 1; mk 2 5 ]
    let test2 = Range.[ mk 1 2 ]
    let test3 = Range.[ mk 4 6 ]
    let test4 = Range.[ mk 0 6 ]

    let%expect_test _ =
      Fmt.pr "%a" pp (union test1 []);
      [%expect {| {0,1}, {2,5} |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp (union test1 test2);
      [%expect {| {0,5} |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp (union test2 test3);
      [%expect {| {1,2}, {4,6} |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp (union test1 test3);
      [%expect {| {0,1}, {2,6} |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%b" (is_subset [] test2);
      [%expect {| true |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%b" (is_subset test1 test2);
      [%expect {| false |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%b" (is_subset test3 test4);
      [%expect {| true |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp (intersect test1 test2);
      [%expect {| |}]
    ;;

    (* TODO: how to represent empty? *)

    let%expect_test _ =
      Fmt.pr "%a" pp (intersect test1 test4);
      [%expect {| {0,1}, {2,5} |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%b" (invariants Range.[ mk 0 1; mk 2 3 ]);
      [%expect {| true |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%b" (invariants Range.[ mk 0 1; mk 1 2 ]);
      [%expect {| false |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%b" (invariants Range.[ mk 2 3; mk 0 1 ]);
      [%expect {| false |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp (of_list Range.[ mk 0 1; mk 1 2 ]);
      [%expect {| {0,2} |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp (of_list Range.[ mk 1 2; mk 0 1 ]);
      [%expect {| {0,2} |}]
    ;;

    let%expect_test _ =
      Fmt.pr "%a" pp_marks (mark_string (of_list []) "str");
      [%expect{| u{0,3} |}]

    let%expect_test _ =
      Fmt.pr "%a" pp_marks (mark_string (of_list Range.[ mk 0 1 ]) "str");
      [%expect{| c{0,1} u{1,3} |}]

    let%expect_test _ =
      Fmt.pr "%a" pp_marks (mark_string (of_list Range.[ mk 1 2 ]) "str");
      [%expect{| u{0,1} c{1,2} u{2,3} |}]

    let%expect_test _ =
      Fmt.pr "%a" pp_marks (mark_string (of_list Range.[ mk 1 3 ]) "str");
      [%expect{| u{0,1} c{1,3} |}]
  end)
;;
