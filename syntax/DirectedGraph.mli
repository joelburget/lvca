module type Key_intf = sig
  type t

  include Base.Comparator.S with type t := t
  include Base.Hashtbl.Key.S with type t := t
end

(** Raised by [topsort_exn] if the graph is not a dag. *)
exception NotDag

module Int : sig
  module ConnectedComponents : sig
    (** The output from connected component algorithm. *)
    type t =
      { scc_count : int (** The number of SCCs found. *)
      ; scc_numbering : int list
            (** A list corresponding to the input adjacency list, with the SCC number
                assigned to each node. Note that SCC numbers need not be contiguous:
                they're the numbers of a representative from each SCC (the lowest-numbered
                representative). So, each SCC number is in the range \[0,n). *)
      }
  end

  val graph_of_adjacency : int list list -> int list Lvca_util.Int.Map.t

  (** Given an adjacency list, give the SCCs. *)
  val connected_components : int list Lvca_util.Int.Map.t -> ConnectedComponents.t

  (** Given an SCC numbering (see [connected_components]), give a list of SCCs, each
      represented as a set of nodes contained in it. *)
  val make_sets : int list -> Lvca_util.Int.Set.t list

  (* better version of make_sets? *)
  val make_sets' : int list -> Lvca_util.Int.Set.t Lvca_util.Int.Map.t

  (** The composition of [connected_components] and [make_sets]. *)
  val connected_component_sets : int list Lvca_util.Int.Map.t -> Lvca_util.Int.Set.t list

  (** Topologically sort a graph given as an adjacency list. *)
  val topsort_exn : int list Lvca_util.Int.Map.t -> int list

  (** Topologically sort a graph given as an adjacency list. *)
  val topsort : int list Lvca_util.Int.Map.t -> int list option
end

module F (Key : Key_intf) : sig
  module Graph : sig
    (** A graph is represented as a mapping from key to key list. *)
    type t = (Key.t, Key.t list, Key.comparator_witness) Base.Map.t
  end

  module ConnectedComponents : sig
    (** The output from the connected component algorithm. *)
    type t =
      { scc_graph : int list Lvca_util.Int.Map.t
            (** An adjacency list representing the graph of SCCs. *)
      ; sccs : (Key.t, Key.comparator_witness) Base.Set.t Lvca_util.Int.Map.t
            (** Mapping from SCC number to keys contained in it. *)
      }
  end

  (** Find the (strongly) [connected_components] in a [graph]. *)
  val connected_components : Graph.t -> ConnectedComponents.t

  (** Topologically sort a graph given as an adjacency list. *)
  val topsort_exn : Graph.t -> Key.t list

  (** Topologically sort a graph given as an adjacency list. *)
  val topsort : Graph.t -> Key.t list option
end
