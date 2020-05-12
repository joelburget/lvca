let int_map_unions : 'a Int.Map.t list -> 'a Int.Map.t =
  List.fold_right ~f:map_union ~init:Int.Map.empty
;;

module MutableSet = struct
  module Impl = struct
    type ('elt, 'cmp) t = ('elt, 'cmp) Set.t ref

    type ('k, 'cmp) comparator =
      (module Comparator.S with type comparator_witness = 'cmp and type t = 'k)

    let of_list : ('a, 'cmp) comparator -> 'a list -> ('a, 'cmp) t =
     fun comparator lst -> ref (Set.of_list comparator lst)
   ;;

    let create : ('a, 'cmp) comparator -> ('a, 'cmp) t =
     fun comparator -> of_list comparator []
   ;;

    let of_array : ('a, 'cmp) comparator -> 'a array -> ('a, 'cmp) t =
     fun comparator lst -> ref (Set.of_array comparator lst)
   ;;

    let to_list : ('a, _) t -> 'a list = fun t -> Set.to_list !t
    let to_array : ('a, _) t -> 'a array = fun t -> Set.to_array !t
    let copy : ('a, 'cmp) t -> ('a, 'cmp) t = fun t -> ref !t
    let is_empty : ('a, _) t -> bool = fun t -> Set.is_empty !t
    let iter : ('a, _) t -> f:('a -> unit) -> unit = fun t ~f -> Set.iter !t ~f
    let mem : ('a, _) t -> 'a -> bool = fun t a -> Set.mem !t a
    let add : ('a, 'cmp) t -> 'a -> unit = fun t a -> t := Set.add !t a
    let remove : ('a, 'cmp) t -> 'a -> unit = fun t a -> t := Set.remove !t a

    let union : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t =
     fun a b -> ref (Set.union !a !b)
   ;;

    let is_subset : ('a, 'cmp) t -> of_:('a, 'cmp) t -> bool =
     fun t1 ~of_:t2 -> Set.is_subset !t1 ~of_:!t2
   ;;

    let min_elt : ('a, _) t -> 'a option = fun t -> Set.min_elt !t
    let min_elt_exn : ('a, _) t -> 'a = fun t -> Set.min_elt_exn !t
    let snapshot : ('a, 'cmp) t -> ('a, 'cmp) Set.t = fun t -> !t
  end

  module Int = struct
    type t = (int, Int.comparator_witness) Impl.t

    let of_list = Impl.of_list (module Int)
    let to_list = Impl.to_list
    let to_array = Impl.to_array
    let add = Impl.add
    let mem = Impl.mem
    let snapshot = Impl.snapshot
    let is_empty = Impl.is_empty
    let create () = Impl.create (module Int)

    let merge_many : t -> int list -> unit =
     fun t ints -> List.iter ints ~f:(add t)
   ;;
  end

  module String = struct
    type t = (string, String.comparator_witness) Impl.t

    let of_list = Impl.of_list (module String)
    let to_list = Impl.to_list
    let to_array = Impl.to_array
    let add = Impl.add
    let mem = Impl.mem
    let snapshot = Impl.snapshot
    let is_empty = Impl.is_empty
    let create () = Impl.create (module String)

    let merge_many : t -> string list -> unit =
     fun t strings -> List.iter strings ~f:(add t)
   ;;
  end

  include Impl
end

module Array = struct
  type 'a t = 'a array

  let reverse_iteri : 'a t -> f:(int -> 'a -> unit) -> unit =
   fun arr ~f ->
    for i = Array.length arr - 1 downto 0 do
      f i arr.(i)
    done
 ;;

  let reverse_iter : 'a t -> f:('a -> unit) -> unit =
   fun arr ~f -> reverse_iteri arr ~f:(fun _ a -> f a)
 ;;

  let reverse : 'a t -> 'a t =
   fun arr ->
    let result = Array.copy arr in
    Array.rev_inplace result;
    result
 ;;
end

module Hash_set = struct
  let is_subset : 'a Hash_set.t -> of_:'a Hash_set.t -> bool =
   fun s1 ~of_:s2 -> Hash_set.(for_all s1 ~f:(mem s2))
 ;;
end
