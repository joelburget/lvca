open Base

module Int = struct
  type t = int list list

  let connected_components adjacencies =
    let ( .@[] ) lst index = List.nth_exn lst index in
    let ( .![] ) lst index = !(lst.@[index]) in
    let ( .@[]<- ) lst index value = lst.@[index] := value in
    let nodes = List.length adjacencies in
    let unvisited = -1 in
    let id = ref 0 in
    let ids, low, on_stack =
      nodes |> List.init ~f:(fun _i -> ref unvisited, ref 0, ref false) |> List.unzip3
    in
    let stack = Stack.create () in
    let rec dfs at =
      Stack.push stack at;
      on_stack.@[at] <- true;
      let at_id = !id in
      Int.incr id;
      ids.@[at] <- at_id;
      low.@[at] <- at_id;
      (* Visit all neighbors and min low-link *)
      List.iter adjacencies.@[at] ~f:(fun neighbor ->
          if Int.(ids.![neighbor] = unvisited) then dfs neighbor;
          if on_stack.![neighbor] then low.@[at] <- Int.min low.![at] low.![neighbor]);
      (* If we're at start of scc, empty the seen stack *)
      if Int.(ids.![at] = low.![at])
      then (
        let continue = ref true in
        while !continue do
          let node = Stack.pop_exn stack in
          on_stack.@[node] <- false;
          low.@[node] <- ids.![at];
          if Int.(node = at) then continue := false
        done)
    in
    List.iteri ids ~f:(fun i id -> if Int.(!id = unvisited) then dfs i);
    List.map low ~f:( ! )
  ;;
end

let%test_module _ =
  (module struct
    let print_connected_components connected_components =
      Fmt.(pr "%a" (list ~sep:(any " ") int)) connected_components
    ;;

    let%expect_test _ =
      [ [ 1 ]; [ 2 ]; [ 0 ]; [ 4; 7 ]; [ 5 ]; [ 0; 6 ]; [ 0; 2; 4 ]; [ 3; 5 ] ]
      |> Int.connected_components
      |> print_connected_components;
      [%expect {|0 0 0 3 4 4 4 3|}]
    ;;

    let%expect_test _ =
      [ [ 1 ]; [ 2; 4; 6 ]; [ 3 ]; [ 2; 4; 5 ]; [ 5 ]; [ 4 ]; [ 0; 2 ] ]
      |> Int.connected_components
      |> print_connected_components;
      [%expect {|0 0 2 2 4 4 0|}]
    ;;
  end)
;;
