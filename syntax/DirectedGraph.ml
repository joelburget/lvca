open Base
module ISet = Lvca_util.Int.Set

module type Key_intf = sig
  type t

  include Comparator.S with type t := t
  include Hashtbl.Key.S with type t := t
end

exception NotDag

module Int = struct
  module ConnectedComponents = struct
    type t =
      { scc_count : int
      ; scc_numbering : int list
      }

    (*
    let pp =
      let open Fmt in
      record
        [ field "scc_count" (fun t -> t.scc_count) int
        ; field "scc_numbering" (fun t -> t.scc_numbering) (list ~sep:(any " ") int)
        ]
    ;;
    *)
  end

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
    ConnectedComponents.{ scc_count = !scc_count; scc_numbering = Array.to_list low }
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
    let ConnectedComponents.{ scc_numbering; _ } = connected_components adjacency in
    make_sets scc_numbering
  ;;

  (* DFS-based topsort. *)
  let topsort_exn adjacency =
    (* Start with every node unmarked *)
    let unmarked = adjacency |> List.mapi ~f:(fun i _ -> i) |> ISet.of_list in
    let temporarily_marked = Hash_set.create (module Int) in
    let permanently_marked = Hash_set.create (module Int) in
    (* Satck to hold the result *)
    let l = Stack.create () in
    let rec go unmarked =
      match Set.choose unmarked with
      | None -> () (* done -- no more unmarked *)
      | Some node ->
        let unmarked = Set.remove unmarked node in
        let rec visit node =
          if Hash_set.mem permanently_marked node
          then ()
          else if Hash_set.mem temporarily_marked node
          then raise NotDag
          else (
            Hash_set.add temporarily_marked node;
            List.nth_exn adjacency node |> List.iter ~f:visit;
            Hash_set.remove temporarily_marked node;
            Hash_set.add permanently_marked node;
            Stack.push l node)
        in
        visit node;
        go unmarked
    in
    go unmarked;
    Stack.to_list l
  ;;

  let topsort adjacency = try Some (topsort_exn adjacency) with NotDag -> None
end

module F (Key : Key_intf) = struct
  module Graph = struct
    type t = (Key.t, Key.t list, Key.comparator_witness) Base.Map.t
  end

  module ConnectedComponents = struct
    type t =
      { scc_graph : int list list
      ; sccs : (Key.t, Key.comparator_witness) Base.Set.t Lvca_util.Int.Map.t
      }
  end

  let conversions graph =
    let key_to_id, id_to_key =
      graph
      |> Map.to_alist
      |> List.mapi ~f:(fun i (key, _) -> (key, i), (i, key))
      |> List.unzip
    in
    let key_to_id = Map.of_alist_exn (module Key) key_to_id in
    let id_to_key = Map.of_alist_exn (module Base.Int) id_to_key in
    (* Build an (int) adjacency list to pass to Int functions. *)
    let adjacency =
      graph
      |> Map.to_alist
      |> List.map ~f:(fun (_, nodes) -> List.map nodes ~f:(Map.find_exn key_to_id))
    in
    key_to_id, id_to_key, adjacency
  ;;

  let connected_components graph =
    let _key_to_id, id_to_key, adjacency = conversions graph in
    (* First find mappings from the map key to int id and back. *)
    let Int.ConnectedComponents.{ scc_count; scc_numbering } =
      Int.connected_components adjacency
    in
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
    ConnectedComponents.{ scc_graph; sccs }
  ;;

  let topsort_exn graph =
    let _key_to_id, id_to_key, adjacency = conversions graph in
    adjacency |> Int.topsort_exn |> List.map ~f:(Map.find_exn id_to_key)
  ;;

  let topsort adjacency = try Some (topsort_exn adjacency) with NotDag -> None
end

let%test_module _ =
  (module struct
    (*
      3 ---> 4 <--- 6 ---> 2
      ^      |   /> |    / ^
      |      | /    |  /   |
      v      v      v <    |
      7 ---> 5 ---> 0 ---> 1

      sccs:
        - 3, 7
        - 4, 5, 6
        - 0, 1, 2
    *)
    let adjacency1 =
      [ [ 1 ]; [ 2 ]; [ 0 ]; [ 4; 7 ]; [ 5 ]; [ 0; 6 ]; [ 0; 2; 4 ]; [ 3; 5 ] ]
    ;;

    (*
      0 ---> 1 ---> 4 <---> 5
      ^    / |      ^     >
      |  /   |      |   /
      | <    v      | /
      6 ---> 2 <--> 3

      sccs:
        - 0, 1, 6
        - 2, 3
        - 4, 5
    *)
    let adjacency2 = [ [ 1 ]; [ 2; 4; 6 ]; [ 3 ]; [ 2; 4; 5 ]; [ 5 ]; [ 4 ]; [ 0; 2 ] ]
    let adjacency3 = [ [ 0 ] ]

    let print_connected_components adjacency =
      let Int.ConnectedComponents.{ scc_numbering; _ } =
        Int.connected_components adjacency
      in
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

    let%expect_test _ =
      print_connected_components adjacency3;
      [%expect {|
        scc_numbering: 0
        sets: 0|}]
    ;;

    module Int' = F (Base.Int)

    let print_connected_components adjacency =
      let graph =
        adjacency |> List.mapi ~f:(fun i v -> i, v) |> Map.of_alist_exn (module Base.Int)
      in
      let Int'.ConnectedComponents.{ scc_graph; sccs } =
        Int'.connected_components graph
      in
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

    let%expect_test _ =
      print_connected_components adjacency3;
      [%expect {|
        scc_graph:
          0 ->
        sccs:
          0 -> 0|}]
    ;;

    let print_scc adjacency =
      Fmt.(pr "%a" (list ~sep:(any " ") int)) (Int.topsort_exn adjacency)
    ;;

    let%expect_test _ =
      print_scc [ [ 2 ]; [ 0 ]; [] ];
      [%expect {|1 0 2|}]
    ;;

    let%expect_test _ =
      print_scc [ []; [ 0 ]; [ 0; 1 ] ];
      [%expect {|2 1 0|}]
    ;;

    let print_scc adjacency =
      let graph =
        adjacency |> List.mapi ~f:(fun i v -> i, v) |> Map.of_alist_exn (module Base.Int)
      in
      let Int'.ConnectedComponents.{ scc_graph; _ } = Int'.connected_components graph in
      Fmt.(pr "%a" (list ~sep:(any " ") int)) (Int.topsort_exn scc_graph)
    ;;

    let%expect_test _ =
      print_scc adjacency1;
      [%expect {|1 0 2|}]
    ;;

    let%expect_test _ =
      print_scc adjacency2;
      [%expect {|2 1 0|}]
    ;;

    let%expect_test _ =
      print_scc adjacency3;
      [%expect {|0|}]
    ;;

    module String = F (Base.String)

    let%expect_test _ =
      let graph = Map.of_alist_exn (module Base.String) [ "foo", [ "foo" ] ] in
      let String.ConnectedComponents.{ scc_graph; _ } =
        String.connected_components graph
      in
      Fmt.(pr "%a" (list ~sep:(any " ") int)) (Int.topsort_exn scc_graph);
      [%expect {|0|}]
    ;;
  end)
;;