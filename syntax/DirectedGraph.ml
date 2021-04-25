open Base
module ISet = Lvca_util.Int.Set

module Int = struct
  type t = int list list
  type numbered_components = int list
  type component_sets = ISet.t list

  let numbered_connected_components adjacencies =
    let nodes = List.length adjacencies in
    let unvisited = -1 in
    let id = ref 0 in
    let ids = Array.init nodes ~f:(fun _ -> unvisited) in
    let low = Array.init nodes ~f:(fun _ -> 0) in
    let on_stack = Array.init nodes ~f:(fun _ -> false) in
    let stack = Stack.create () in
    let rec dfs at =
      Stack.push stack at;
      on_stack.(at) <- true;
      let at_id = !id in
      Int.incr id;
      ids.(at) <- at_id;
      low.(at) <- at_id;
      (* Visit all neighbors and min low-link *)
      List.iter (List.nth_exn adjacencies at) ~f:(fun neighbor ->
          if Int.(ids.(neighbor) = unvisited) then dfs neighbor;
          if on_stack.(neighbor) then low.(at) <- Int.min low.(at) low.(neighbor));
      (* If we're at start of scc, empty the seen stack *)
      if Int.(ids.(at) = low.(at))
      then (
        let continue = ref true in
        while !continue do
          let node = Stack.pop_exn stack in
          on_stack.(node) <- false;
          low.(node) <- ids.(at);
          if Int.(node = at) then continue := false
        done)
    in
    Array.iteri ids ~f:(fun i id -> if Int.(id = unvisited) then dfs i);
    Array.to_list low
  ;;

  let make_sets numbered =
    let map = Hashtbl.create (module Int) in
    List.iteri numbered ~f:(fun i group_no ->
        match Hashtbl.find map group_no with
        | None -> Hashtbl.add_exn map ~key:group_no ~data:(ISet.singleton i)
        | Some set -> Hashtbl.set map ~key:group_no ~data:(Set.add set i));
    Hashtbl.data map
  ;;

  let connected_component_sets = Lvca_util.(numbered_connected_components >> make_sets)
end

let%test_module _ =
  (module struct
    let print_connected_components adjacency =
      let numbered = Int.numbered_connected_components adjacency in
      let sets = Int.make_sets numbered |> List.map ~f:Set.to_list in
      Fmt.(pr "%a\n" (list ~sep:(any " ") int)) numbered;
      Fmt.(pr "%a\n" (list ~sep:(any " ") (list ~sep:(any ",") int))) sets
    ;;

    let%expect_test _ =
      print_connected_components
        [ [ 1 ]; [ 2 ]; [ 0 ]; [ 4; 7 ]; [ 5 ]; [ 0; 6 ]; [ 0; 2; 4 ]; [ 3; 5 ] ];
      [%expect {|
        0 0 0 3 4 4 4 3
        4,5,6 3,7 0,1,2|}]
    ;;

    let%expect_test _ =
      print_connected_components
        [ [ 1 ]; [ 2; 4; 6 ]; [ 3 ]; [ 2; 4; 5 ]; [ 5 ]; [ 4 ]; [ 0; 2 ] ];
      [%expect {|
        0 0 2 2 4 4 0
        4,5 2,3 0,1,6|}]
    ;;
  end)
;;
