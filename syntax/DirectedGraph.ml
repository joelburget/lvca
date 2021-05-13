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

  let graph_of_adjacency =
    Lvca_util.(Base.List.mapi ~f:(fun i v -> i, v) >> Map.of_alist_exn (module Base.Int))
  ;;

  (* Tarjan's algorithm *)
  let connected_components connections =
    let len = Map.length connections in
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
      List.iter (Map.find_exn connections at) ~f:(fun neighbor ->
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

  let pre_make_sets numbered =
    let map = Hashtbl.create (module Int) in
    List.iteri numbered ~f:(fun i group_no ->
        match Hashtbl.find map group_no with
        | None -> Hashtbl.add_exn map ~key:group_no ~data:(ISet.singleton i)
        | Some set -> Hashtbl.set map ~key:group_no ~data:(Set.add set i));
    map
  ;;

  let make_sets numbered =
    numbered |> pre_make_sets |> Hashtbl.to_alist |> Lvca_util.Int.Map.of_alist_exn
  ;;

  let connected_component_sets connections =
    let ConnectedComponents.{ scc_numbering; _ } = connected_components connections in
    make_sets scc_numbering
  ;;

  (* DFS-based topsort. *)
  let topsort_exn connections =
    (* Start with every node unmarked *)
    let unmarked = Map.keys connections |> ISet.of_list in
    let temporarily_marked = Hash_set.create (module Int) in
    let permanently_marked = Hash_set.create (module Int) in
    (* Stack to hold the result *)
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
            Map.find_exn connections node |> List.iter ~f:visit;
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

  let topsort connections = try Some (topsort_exn connections) with NotDag -> None
end

module Make (Key : Key_intf) = struct
  module Graph = struct
    type t = (Key.t, Key.t list, Key.comparator_witness) Base.Map.t
  end

  module ConnectedComponents = struct
    type t =
      { scc_graph : int list Lvca_util.Int.Map.t
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
    let connections =
      graph
      |> Map.to_alist
      |> List.map ~f:(fun (key, nodes) ->
             Map.find_exn key_to_id key, List.map nodes ~f:(Map.find_exn key_to_id))
      |> Map.of_alist_exn (module Base.Int)
    in
    key_to_id, id_to_key, connections
  ;;

  let connected_components graph =
    let _key_to_id, id_to_key, connections = conversions graph in
    (* First find mappings from the map key to int id and back. *)
    let Int.ConnectedComponents.{ scc_count = _; scc_numbering } =
      Int.connected_components connections
    in
    let int_sccs = Int.make_sets scc_numbering in
    let scc_graph =
      int_sccs
      |> Map.mapi ~f:(fun ~key:origin_scc_id ~data:node_id_set ->
             (* find ids of other sccs this one points to: *)
             (* start with set of ids contained in this scc *)
             let forward_edge_id_list =
               node_id_set
               |> Set.to_list
               (* for each id, add the set of ids it points to to a set. *)
               |> List.map ~f:(fun node_id ->
                      let forward_edges = Map.find_exn connections node_id in
                      ISet.of_list forward_edges)
               |> ISet.union_list
               |> Set.to_list
             in
             let int_scc_list = Map.to_alist int_sccs in
             (* For each outgoing edge, find the scc it lands in *)
             forward_edge_id_list
             |> List.map ~f:(fun node_id ->
                    int_scc_list
                    |> List.find_exn ~f:(fun (_scc_id, scc_ids) ->
                           Set.mem scc_ids node_id)
                    |> fst)
             |> ISet.of_list
             |> Fn.flip Set.remove origin_scc_id)
      |> Map.map ~f:Set.to_list
    in
    let sccs =
      int_sccs
      |> Map.map ~f:(fun int_set ->
             Set.map (module Key) int_set ~f:(Map.find_exn id_to_key))
    in
    ConnectedComponents.{ scc_graph; sccs }
  ;;

  let topsort_exn graph =
    let _key_to_id, id_to_key, connections = conversions graph in
    connections |> Int.topsort_exn |> List.map ~f:(Map.find_exn id_to_key)
  ;;

  let topsort connections = try Some (topsort_exn connections) with NotDag -> None
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
      let connections =
        adjacency |> List.mapi ~f:(fun i a -> i, a) |> Map.of_alist_exn (module Base.Int)
      in
      let Int.ConnectedComponents.{ scc_numbering; _ } =
        Int.connected_components connections
      in
      let sets = Int.make_sets scc_numbering |> Map.data |> List.map ~f:Set.to_list in
      Fmt.(pr "scc_numbering: %a\n" (list ~sep:(any " ") int)) scc_numbering;
      Fmt.(pr "sets: %a\n" (list ~sep:(any " ") (list ~sep:(any ",") int))) sets
    ;;

    let%expect_test _ =
      print_connected_components adjacency1;
      [%expect {|
        scc_numbering: 0 0 0 3 4 4 4 3
        sets: 0,1,2 3,7 4,5,6|}]
    ;;

    let%expect_test _ =
      print_connected_components adjacency2;
      [%expect {|
        scc_numbering: 0 0 2 2 4 4 0
        sets: 0,1,6 2,3 4,5|}]
    ;;

    let%expect_test _ =
      print_connected_components adjacency3;
      [%expect {|
        scc_numbering: 0
        sets: 0|}]
    ;;

    module Int' = Make (Base.Int)

    let print_connected_components adjacency =
      let graph = Int.graph_of_adjacency adjacency in
      let Int'.ConnectedComponents.{ scc_graph; sccs } =
        Int'.connected_components graph
      in
      Fmt.pr "scc_graph:\n";
      scc_graph
      |> Map.iteri ~f:(fun ~key:i ~data:nodes ->
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
          0 ->
          3 -> 4
          4 -> 0
        sccs:
          0 -> 0 1 2
          3 -> 3 7
          4 -> 4 5 6|}]
    ;;

    let%expect_test _ =
      print_connected_components adjacency2;
      [%expect
        {|
        scc_graph:
          0 -> 2,4
          2 -> 4
          4 ->
        sccs:
          0 -> 0 1 6
          2 -> 2 3
          4 -> 4 5|}]
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
      let graph = Int.graph_of_adjacency adjacency in
      Fmt.(pr "%a" (list ~sep:(any " ") int)) (Int.topsort_exn graph)
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
      let graph = Int.graph_of_adjacency adjacency in
      let Int'.ConnectedComponents.{ scc_graph; sccs = _ } =
        Int'.connected_components graph
      in
      Fmt.(pr "%a" (list ~sep:(any " ") int)) (Int.topsort_exn scc_graph)
    ;;

    let%expect_test _ =
      print_scc adjacency1;
      [%expect {|3 4 0|}]
    ;;

    let%expect_test _ =
      print_scc adjacency2;
      [%expect {|0 2 4|}]
    ;;

    let%expect_test _ =
      print_scc adjacency3;
      [%expect {|0|}]
    ;;

    module String = Make (Base.String)

    let%expect_test _ =
      let graph = Map.of_alist_exn (module Base.String) [ "foo", [ "foo" ] ] in
      let String.ConnectedComponents.{ scc_graph; sccs = _ } =
        String.connected_components graph
      in
      Fmt.(pr "%a" (list ~sep:(any " ") int)) (Int.topsort_exn scc_graph);
      [%expect {|0|}]
    ;;

    let%expect_test _ =
      let graph =
        Map.of_alist_exn
          (module Base.String)
          [ "attribute", []
          ; "block", [ "attribute"; "block_desc"; "list" ]
          ; "block_desc", [ "inline" ]
          ; "doc", [ "block"; "list" ]
          ; "inline", [ "attribute"; "inline_desc"; "list" ]
          ; "inline_desc", [ "inline"; "list" ]
          ; "list", [ "list" ]
          ]
      in
      let String.ConnectedComponents.{ scc_graph; sccs = _ } =
        String.connected_components graph
      in
      Stdlib.Format.set_margin 80;
      Fmt.pr "scc_graph:\n";
      scc_graph
      |> Map.iteri
           ~f:
             Fmt.(
               fun ~key:i ~data:conns ->
                 pr "  - %n -> %a\n" i (list ~sep:(any ", ") int) conns);
      Fmt.(pr "%a" (list ~sep:(any " ") int)) (Int.topsort_exn scc_graph);
      [%expect
        {|
        scc_graph:
          - 0 ->
          - 1 -> 0, 2, 5
          - 2 -> 3
          - 3 -> 0, 5
          - 5 ->
          - 6 -> 1, 5
        6 1 2 3 5 0 |}]
    ;;
  end)
;;
