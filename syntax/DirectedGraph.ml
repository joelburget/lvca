open Base
module ISet = Lvca_util.Int.Set

module type Key_intf = sig
  type t

  include Comparator.S with type t := t
  include Hashtbl.Key.S with type t := t
end

module Int = struct
  type connected_components =
    { scc_count : int
    ; scc_numbering : int list
    }

  (* Tarjan's algorithm *)
  let connected_components adjacencies =
    let len = List.length adjacencies in
    let unvisited = -1 in
    let scc_count = ref 0 in
    let id = ref 0 in
    let ids = Array.create ~len unvisited in
    let low = Array.create ~len 0 in
    let on_stack = Array.create ~len false in
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
        done;
        Int.incr scc_count)
    in
    Array.iteri ids ~f:(fun i id -> if Int.(id = unvisited) then dfs i);
    { scc_count = !scc_count; scc_numbering = Array.to_list low }
  ;;

  let make_sets numbered =
    let map = Hashtbl.create (module Int) in
    List.iteri numbered ~f:(fun i group_no ->
        match Hashtbl.find map group_no with
        | None -> Hashtbl.add_exn map ~key:group_no ~data:(ISet.singleton i)
        | Some set -> Hashtbl.set map ~key:group_no ~data:(Set.add set i));
    Hashtbl.data map
  ;;

  let connected_component_sets adjacency =
    let { scc_numbering; _ } = connected_components adjacency in
    make_sets scc_numbering
  ;;
end

module F (Key : Key_intf) = struct
  type graph = (Key.t, Key.t list, Key.comparator_witness) Base.Map.t

  type connected_components =
    { scc_graph : int list list
    ; sccs : (Key.t, Key.comparator_witness) Base.Set.t Lvca_util.Int.Map.t
    }

  let connected_components graph =
    (* First find mappings from the map key to int id and back. *)
    let key_to_id, id_to_key =
      graph
      |> Map.to_alist
      |> List.mapi ~f:(fun i (key, _) -> (key, i), (i, key))
      |> List.unzip
    in
    let key_to_id = Map.of_alist_exn (module Key) key_to_id in
    let id_to_key = Map.of_alist_exn (module Base.Int) id_to_key in
    (* Now, build an (int) adjacency list to pass to Int.connected_components. *)
    let adjacency =
      graph
      |> Map.to_alist
      |> List.map ~f:(fun (_, nodes) -> List.map nodes ~f:(Map.find_exn key_to_id))
    in
    let Int.{ scc_count; scc_numbering } = Int.connected_components adjacency in
    let int_sccs = Int.make_sets scc_numbering in
    (* Build the graph between SCCs. *)
    let scc_graph =
      (* We use nodes [0, scc_count). *)
      scc_count
      |> List.init ~f:(fun scc_num ->
             (* Find all the nodes contained in this SCC: *)
             scc_num
             |> List.nth_exn int_sccs
             |> Set.to_list
             (* For each node, look at its outgoing edges: *)
             |> List.map ~f:(fun node ->
                    node
                    (* Look at outgoing edges: *)
                    |> List.nth_exn adjacency
                    |> List.filter_map ~f:(fun out_node ->
                           (* Find the SCC representative for the outgoing SCC. *)
                           let out_scc_rep = List.nth_exn scc_numbering out_node in
                           match
                             (* Find the outgoing SCC number. *)
                             List.findi int_sccs ~f:(fun _ scc_elems ->
                                 Set.mem scc_elems out_scc_rep)
                           with
                           | None -> Lvca_util.invariant_violation "no scc found"
                           | Some (out_scc_num', _) ->
                             (* Don't count self edges. *)
                             if Base.Int.(out_scc_num' = scc_num)
                             then None
                             else Some out_scc_num'))
             (* Combine all outgoing edges and dedupe. *)
             |> List.join
             |> List.dedup_and_sort ~compare:Base.Int.compare)
    in
    let sccs =
      int_sccs
      |> List.mapi ~f:(fun scc_num set ->
             scc_num, Set.map (module Key) set ~f:(Map.find_exn id_to_key))
      |> Map.of_alist_exn (module Base.Int)
    in
    { scc_graph; sccs }
  ;;
end

let%test_module _ =
  (module struct
    let adjacency1 =
      [ [ 1 ]; [ 2 ]; [ 0 ]; [ 4; 7 ]; [ 5 ]; [ 0; 6 ]; [ 0; 2; 4 ]; [ 3; 5 ] ]
    ;;

    let adjacency2 = [ [ 1 ]; [ 2; 4; 6 ]; [ 3 ]; [ 2; 4; 5 ]; [ 5 ]; [ 4 ]; [ 0; 2 ] ]

    let print_connected_components adjacency =
      let Int.{ scc_numbering; _ } = Int.connected_components adjacency in
      let sets = Int.make_sets scc_numbering |> List.map ~f:Set.to_list in
      Fmt.(pr "scc_numbering: %a\n" (list ~sep:(any " ") int)) scc_numbering;
      Fmt.(pr "sets: %a\n" (list ~sep:(any " ") (list ~sep:(any ",") int))) sets
    ;;

    let%expect_test _ =
      print_connected_components adjacency1;
      [%expect {|
        scc_numbering: 0 0 0 3 4 4 4 3
        sets: 4,5,6 3,7 0,1,2|}]
    ;;

    let%expect_test _ =
      print_connected_components adjacency2;
      [%expect {|
        scc_numbering: 0 0 2 2 4 4 0
        sets: 4,5 2,3 0,1,6|}]
    ;;

    module Int = F (Base.Int)

    let print_connected_components adjacency =
      let map =
        adjacency |> List.mapi ~f:(fun i v -> i, v) |> Map.of_alist_exn (module Base.Int)
      in
      let Int.{ scc_graph; sccs } = Int.connected_components map in
      Fmt.pr "scc_graph:\n";
      scc_graph
      |> List.iteri ~f:(fun i nodes ->
             Fmt.(pr "  %d -> %a\n" i (list ~sep:(any ",") int) nodes));
      Fmt.pr "sccs:\n";
      sccs
      |> Map.to_alist
      |> List.iter ~f:(fun (scc_num, node_set) ->
             Fmt.(
               pr "  %d -> %a\n" scc_num (list ~sep:(any " ") int) (Set.to_list node_set)))
    ;;

    let%expect_test _ =
      print_connected_components adjacency1;
      [%expect
        {|
        scc_graph:
          0 -> 2
          1 -> 0
          2 ->
        sccs:
          0 -> 4 5 6
          1 -> 3 7
          2 -> 0 1 2|}]
    ;;

    let%expect_test _ =
      print_connected_components adjacency2;
      [%expect
        {|
        scc_graph:
          0 ->
          1 -> 0
          2 -> 0,1
        sccs:
          0 -> 4 5
          1 -> 2 3
          2 -> 0 1 6|}]
    ;;
  end)
;;
